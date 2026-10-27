from pathlib import Path
import subprocess

import pytest

from gcmpy.run_tools import mpi_runner
from gcmpy.run_tools.mpi_runner import _build_command, _parse_ocean_preload

GEOSDIR = Path("/GEOS")


@pytest.mark.parametrize('enabled', [False, True])
def test_run_environment_logging(monkeypatch, tmp_path, capsys, enabled):
    inherited = {
        'UDUNITS2_XML_PATH': '/baselibs/share/udunits/udunits2.xml',
        'PATH': '/usr/bin',
        'OMP_NUM_THREADS': '1',
        'MY_API_KEY': 'api-secret-value',
        'github_token': 'token-secret-value',
        'PASSWORD': 'password-secret-value',
        'MULTILINE': 'first\nsecond\rthird',
        'LESS_TERMCAP_so': '\x1b[38;5;246m',
        'OTHER_CONTROLS': '\t\x07\x08\x7f\x9b',
    }
    monkeypatch.setattr(mpi_runner.os, 'environ', inherited.copy())
    (tmp_path / 'EGRESS').touch()
    launches = []

    def fake_run(cmd, *, env, cwd):
        output = capsys.readouterr().out
        launches.append(output)
        assert env == {**inherited, 'LD_PRELOAD': '/GEOS/lib/libmom6.so'}
        assert cwd == tmp_path
        return subprocess.CompletedProcess(cmd, 0)

    monkeypatch.setattr(mpi_runner.subprocess, 'run', fake_run)
    kwargs = {'print_run_environment': True} if enabled else {}
    mpi_runner.run_gcm(
        'mpiexec -n', 6, Path('/GEOS/bin/GEOSgcm.x'), Path('/GEOS/bin'),
        tmp_path, ocean_preload='env LD_PRELOAD=$GEOSDIR/lib/libmom6.so',
        **kwargs,
    )
    assert len(launches) == 1
    output = launches[0]
    if not enabled:
        assert output == ''
        return
    lines = output.splitlines()
    assert lines[0].startswith('=== BEGIN model-launch environment')
    assert lines[-1] == '=== END model-launch environment ==='
    assert lines[1:-1] == sorted(lines[1:-1])
    assert 'UDUNITS2_XML_PATH=' + inherited['UDUNITS2_XML_PATH'] in lines
    assert 'LD_PRELOAD=/GEOS/lib/libmom6.so' in lines
    assert 'OMP_NUM_THREADS=1' in lines
    assert r'MULTILINE=first\nsecond\rthird' in lines
    assert r'LESS_TERMCAP_so=\x1b[38;5;246m' in lines
    assert r'OTHER_CONTROLS=\x09\x07\x08\x7f\x9b' in lines
    assert '\x1b' not in output
    assert '\x9b' not in output
    for key in ('MY_API_KEY', 'github_token', 'PASSWORD'):
        assert f'{key}=<redacted>' in lines
        assert inherited[key] not in output


def test_run_environment_printed_before_each_retry(monkeypatch, tmp_path, capsys):
    (tmp_path / 'EGRESS').touch()
    calls = []

    def fake_run(cmd, *, env, cwd):
        output = capsys.readouterr().out
        assert output.count('=== BEGIN model-launch environment') == 1
        calls.append(cmd)
        return subprocess.CompletedProcess(cmd, 1 if len(calls) == 1 else 0)

    monkeypatch.setattr(mpi_runner.subprocess, 'run', fake_run)
    mpi_runner.run_gcm(
        'mpiexec -n', 6, tmp_path / 'GEOSgcm.x', tmp_path, tmp_path,
        max_attempts=2, print_run_environment=True,
    )
    assert len(calls) == 2


def test_preload_empty_returns_none():
    assert _parse_ocean_preload(None, GEOSDIR) is None
    assert _parse_ocean_preload("", GEOSDIR) is None
    assert _parse_ocean_preload("   ", GEOSDIR) is None


def test_preload_expands_geosdir_and_strips_env():
    env = _parse_ocean_preload(
        "env LD_PRELOAD=$GEOSDIR/lib/libmom6.so:$GEOSDIR/lib/libcice6.so",
        GEOSDIR,
    )
    assert env == {
        "LD_PRELOAD": "/GEOS/lib/libmom6.so:/GEOS/lib/libcice6.so"
    }


def test_preload_expands_braced_geosdir():
    env = _parse_ocean_preload(
        "env LD_PRELOAD=${GEOSDIR}/lib/libmom6.so", GEOSDIR
    )
    assert env == {"LD_PRELOAD": "/GEOS/lib/libmom6.so"}


def test_preload_without_env_wrapper():
    env = _parse_ocean_preload("LD_PRELOAD=$GEOSDIR/lib/libmom6.so", GEOSDIR)
    assert env == {"LD_PRELOAD": "/GEOS/lib/libmom6.so"}


def test_preload_dyld_insert_libraries():
    env = _parse_ocean_preload(
        "env DYLD_INSERT_LIBRARIES=$GEOSDIR/lib/libmom6.dylib", GEOSDIR
    )
    assert env == {"DYLD_INSERT_LIBRARIES": "/GEOS/lib/libmom6.dylib"}


def test_preload_unresolved_cmake_token_returns_none():
    # Catalog value before configure_file() substitution.
    assert (
        _parse_ocean_preload(
            "env LD_PRELOAD=$GEOSDIR/lib/libmom6@CMAKE_SHARED_LIBRARY_SUFFIX@",
            GEOSDIR,
        )
        is None
    )


def test_preload_malformed_returns_none():
    assert _parse_ocean_preload("garbage", GEOSDIR) is None
    assert _parse_ocean_preload("=value", GEOSDIR) is None
    assert _parse_ocean_preload("LD_PRELOAD=", GEOSDIR) is None


def test_build_command_merges_preload_env():
    cmd, env = _build_command(
        "srun",
        24,
        Path("/fake/bin/GEOSgcm.x"),
        [],
        None,
        {"LD_PRELOAD": "/GEOS/lib/libmom6.so"},
    )
    assert cmd[:3] == ["srun", "24", "/fake/bin/GEOSgcm.x"]
    assert "--logging_config" in cmd
    assert env["LD_PRELOAD"] == "/GEOS/lib/libmom6.so"


def test_build_command_without_preload():
    _, env = _build_command(
        "srun",
        24,
        Path("/fake/bin/GEOSgcm.x"),
        [],
        None,
        None,
    )
    # No preload injected: the caller's environment is inherited untouched.
    import os

    assert env == os.environ.copy()


def test_build_command_singularity_and_ioserver():
    cmd, _ = _build_command(
        "mpiexec",
        48,
        Path("/fake/bin/GEOSgcm.x"),
        ["--npes_io_server", "16"],
        "singularity exec /fake/image.sif",
        None,
    )
    assert cmd == [
        "mpiexec",
        "48",
        "singularity",
        "exec",
        "/fake/image.sif",
        "/fake/bin/GEOSgcm.x",
        "--npes_io_server",
        "16",
        "--logging_config",
        "logging.yaml",
    ]


def test_build_command_expands_multword_run_command(monkeypatch):
    # setup writes run_command = '$GEOSBIN/esma_mpirun -np ' (setup_envs.py:34);
    # csh word-splits and expands it before the launch.
    monkeypatch.setenv("GEOSBIN", "/install/bin")
    cmd, _ = _build_command(
        "$GEOSBIN/esma_mpirun -np ",
        560,
        Path("/install/bin/GEOSgcm.x"),
        [],
        None,
        None,
    )
    assert cmd[:4] == ["/install/bin/esma_mpirun", "-np", "560", "/install/bin/GEOSgcm.x"]


def test_build_command_run_command_trailing_space_only():
    cmd, _ = _build_command(
        "srun ",
        24,
        Path("/fake/bin/GEOSgcm.x"),
        [],
        None,
        None,
    )
    assert cmd[:3] == ["srun", "24", "/fake/bin/GEOSgcm.x"]


def test_build_command_unresolved_run_command_var_raises(monkeypatch):
    monkeypatch.delenv("GEOSBIN", raising=False)
    import pytest

    with pytest.raises(ValueError, match="unexpanded variable"):
        _build_command(
            "$GEOSBIN/esma_mpirun -np ",
            24,
            Path("/fake/bin/GEOSgcm.x"),
            [],
            None,
            None,
        )

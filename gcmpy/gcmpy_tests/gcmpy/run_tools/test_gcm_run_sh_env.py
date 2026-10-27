"""
Tests for the job environment block in gcm_run.sh (gcm_run.j:21-50).

Runs the real gcm_run.sh against a fake install tree whose
gcmpy/gcm_run.py dumps the environment it inherits, then asserts the
architecture-specific exports.  Also covers GEOSBIN discovery: the
script-dir fallback must only fire when that directory actually holds
g5_modules.sh (a SLURM spool copy must fail loudly instead of sourcing
/var/spool/slurmd/job<id>/g5_modules.sh).
"""

import os
import resource
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

GCM_RUN_SH = Path(__file__).resolve().parents[4] / "gcm_run.sh"
LD_VAR = "DYLD_LIBRARY_PATH" if os.uname().sysname == "Darwin" else "LD_LIBRARY_PATH"


def _make_fake_install(tmp_path: Path) -> Path:
    """Create a minimal $install/{bin,etc} tree that gcm_run.sh can run."""
    bin_dir = tmp_path / "install" / "bin"
    etc_dir = tmp_path / "install" / "etc"
    (bin_dir / "gcmpy").mkdir(parents=True)
    etc_dir.mkdir(parents=True)

    (bin_dir / "g5_modules.sh").write_text("# fake module init\n")
    (etc_dir / "SITE.rc").write_text("SITE_NAME discover\n")
    (bin_dir / "gcmpy" / "gcm_run.py").write_text(
        "import os, resource, sys\n"
        "print(f'CONFIG={sys.argv[-1]}')\n"
        "print(f'CWD={os.getcwd()}')\n"
        "for key in ('ARCH', 'GEOSDIR', 'GEOSUTIL', 'GEOSETC', 'SITE',\n"
        "            'GEOSBIN', 'LD_LIBRARY_PATH', 'DYLD_LIBRARY_PATH', 'PATH', 'BASEDIR',\n"
        "            'GEOS_RUN_ENV_READY', 'UDUNITS2_XML_PATH'):\n"
        "    print(f\"{key}={os.environ.get(key, '')}\")\n"
        "soft, hard = resource.getrlimit(resource.RLIMIT_STACK)\n"
        "print(f'STACK_SOFT={soft}')\n"
        "print(f'STACK_HARD={hard}')\n"
    )
    return bin_dir


def _run_gcm_run_sh(bin_dir: Path, extra_env: dict | None = None) -> dict:
    env = {"PATH": os.environ["PATH"], "GEOSBIN": str(bin_dir)}
    if extra_env:
        env.update(extra_env)
    result = subprocess.run(
        ["bash", str(GCM_RUN_SH), "run_config.yaml"],
        env=env,
        capture_output=True,
        text=True,
        check=True,
    )
    parsed = {}
    for line in result.stdout.splitlines():
        key, _, value = line.partition("=")
        parsed[key] = value
    return parsed


def test_gcm_run_sh_exists():
    assert GCM_RUN_SH.is_file()


def test_pbs_wrapper_receives_config_without_script_arguments(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    config = tmp_path / 'run_exp.yaml'
    result = subprocess.run(
        ['bash', str(GCM_RUN_SH)],
        env={
            'PATH': os.environ['PATH'], 'GEOSBIN': str(bin_dir),
            'GEOS_RUN_CONFIG': str(config), 'PBS_JOBID': '123.pbs',
            'PBS_O_WORKDIR': str(tmp_path),
        },
        capture_output=True, text=True, check=True,
    )
    assert f'CONFIG={config}' in result.stdout.splitlines()
    assert f'CWD={tmp_path.resolve()}' in result.stdout.splitlines()


def test_gcm_run_sh_exports_arch_env(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    env = _run_gcm_run_sh(bin_dir)

    assert env["ARCH"] == os.uname().sysname
    assert env["GEOSBIN"] == str(bin_dir)
    # GEOSBIN is $install/bin -> GEOSDIR is $install.
    assert env["GEOSDIR"] == str(bin_dir.parent)
    assert env["GEOSUTIL"] == str(bin_dir.parent)
    assert env["GEOSETC"] == str(bin_dir.parent / "etc")
    assert env["SITE"] == "discover"
    assert env["GEOS_RUN_ENV_READY"] == str(bin_dir)


def test_gcm_run_sh_exports_module_environment(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    xml_path = tmp_path / "udunits2.xml"
    (bin_dir / "g5_modules.sh").write_text(
        f'export UDUNITS2_XML_PATH="{xml_path}"\n'
    )
    env = _run_gcm_run_sh(bin_dir)
    assert env["UDUNITS2_XML_PATH"] == str(xml_path)


def test_python_bootstrap_reaches_initialized_python(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    shutil.copy(GCM_RUN_SH, bin_dir / 'gcm_run.sh')
    xml_path = tmp_path / 'udunits2.xml'
    (bin_dir / 'g5_modules.sh').write_text(
        f'export UDUNITS2_XML_PATH="{xml_path}"\n'
    )
    result = subprocess.run(
        [sys.executable, '-c',
         'from pathlib import Path; import gcm_run; '
         f'gcm_run.ensure_runtime_environment(Path("run_exp.yaml"), {{"bin_path": {str(bin_dir)!r}}})'],
        env={'PATH': os.environ['PATH'], 'SLURM_JOB_ID': '123'},
        capture_output=True, text=True, check=True,
    )
    assert f'UDUNITS2_XML_PATH={xml_path}' in result.stdout.splitlines()
    assert f'GEOS_RUN_ENV_READY={bin_dir}' in result.stdout.splitlines()


def test_gcm_run_sh_prepends_geosdir_lib(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    env = _run_gcm_run_sh(bin_dir)

    ld = env[LD_VAR].split(":")
    assert ld[0] == str(bin_dir.parent / "lib")
    assert "" not in ld


def test_gcm_run_sh_appends_basedir_when_set(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    basedir = tmp_path / "basedir"
    env = _run_gcm_run_sh(bin_dir, extra_env={"BASEDIR": str(basedir)})

    arch = os.uname().sysname
    ld = env[LD_VAR].split(":")
    assert str(bin_dir.parent / "lib") in ld
    assert str(basedir / arch / "lib") in ld
    assert str(basedir / arch / "bin") == env["PATH"].split(":")[-1]


def test_gcm_run_sh_falls_back_to_script_dir(tmp_path):
    """GEOSBIN unset: the script's own directory wins when it really is
    an install bin/ (manual run straight from install/bin)."""
    bin_dir = _make_fake_install(tmp_path)
    copied = bin_dir / "gcm_run.sh"
    shutil.copy(GCM_RUN_SH, copied)

    result = subprocess.run(
        ["bash", str(copied), "run_config.yaml"],
        env={"PATH": os.environ["PATH"]},
        capture_output=True,
        text=True,
        check=True,
    )
    env = {}
    for line in result.stdout.splitlines():
        key, _, value = line.partition("=")
        env[key] = value

    assert env["GEOSBIN"] == str(bin_dir)
    assert env["GEOSDIR"] == str(bin_dir.parent)


def test_gcm_run_sh_spool_copy_fails_clearly(tmp_path):
    """SLURM-style copy (script NOT next to g5_modules.sh, GEOSBIN unset):
    exit 1 with an actionable message instead of sourcing
    /var/spool/slurmd/job<id>/g5_modules.sh (job 58877485 died that way)."""
    spool_dir = tmp_path / "spool"
    spool_dir.mkdir()
    copied = spool_dir / "slurm_script"
    shutil.copy(GCM_RUN_SH, copied)

    result = subprocess.run(
        ["bash", str(copied), "run_config.yaml"],
        env={"PATH": os.environ["PATH"]},
        capture_output=True,
        text=True,
    )

    assert result.returncode == 1
    assert "cannot locate g5_modules.sh" in result.stderr
    # GEOSBIN must have been rejected (left empty), and no stray source
    # failure from a spool path may appear.
    assert "GEOSBIN=''" in result.stderr
    assert "No such file or directory" not in result.stderr


def test_gcm_run_sh_raises_stack_limit(tmp_path):
    env = _run_gcm_run_sh(_make_fake_install(tmp_path))
    expected = int(env["STACK_HARD"]) if os.uname().sysname == "Darwin" else resource.RLIM_INFINITY
    assert int(env["STACK_SOFT"]) == expected


@pytest.mark.parametrize("arch, expected", [
    ("Darwin", ["-H -s", "-S -s 65520"]),
    ("Linux", ["-s unlimited"]),
])
def test_gcm_run_sh_stack_commands(tmp_path, arch, expected):
    bin_dir = _make_fake_install(tmp_path)
    log = tmp_path / "ulimit.log"
    # Record the shell builtin's arguments so both OS branches run on either host.
    script = '''
uname() { echo "$TEST_ARCH"; }
ulimit() {
    echo "$*" >> "$STACK_LOG"
    if [ "$*" = "-H -s" ]; then echo 65520; fi
    return 0
}
source "$1" "$2"
'''
    subprocess.run(
        ["bash", "-c", script, "test", str(GCM_RUN_SH), "run_config.yaml"],
        env={"PATH": os.environ["PATH"], "GEOSBIN": str(bin_dir),
             "TEST_ARCH": arch, "STACK_LOG": str(log)},
        capture_output=True, text=True, check=True,
    )
    assert log.read_text().splitlines() == expected

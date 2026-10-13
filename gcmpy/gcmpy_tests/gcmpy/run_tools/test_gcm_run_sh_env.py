"""
Tests for the job environment block in gcm_run.sh (gcm_run.j:21-50).

Runs the real gcm_run.sh against a fake install tree whose
gcmpy/gcm_run.py dumps the environment it inherits, then asserts the
architecture-specific exports.
"""

import os
import subprocess
from pathlib import Path

GCM_RUN_SH = Path(__file__).resolve().parents[4] / "gcm_run.sh"


def _make_fake_install(tmp_path: Path) -> Path:
    """Create a minimal $install/{bin,etc} tree that gcm_run.sh can run."""
    bin_dir = tmp_path / "install" / "bin"
    etc_dir = tmp_path / "install" / "etc"
    (bin_dir / "gcmpy").mkdir(parents=True)
    etc_dir.mkdir(parents=True)

    (bin_dir / "g5_modules").write_text("# fake module init\n")
    (etc_dir / "SITE.rc").write_text("SITE_NAME discover\n")
    (bin_dir / "gcmpy" / "gcm_run.py").write_text(
        "import os\n"
        "for key in ('ARCH', 'GEOSDIR', 'GEOSUTIL', 'GEOSETC', 'SITE',\n"
        "            'GEOSBIN', 'LD_LIBRARY_PATH', 'PATH', 'BASEDIR'):\n"
        "    print(f\"{key}={os.environ.get(key, '')}\")\n"
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


def test_gcm_run_sh_prepends_geosdir_lib(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    env = _run_gcm_run_sh(bin_dir)

    ld = env["LD_LIBRARY_PATH"].split(":")
    assert ld[0] == str(bin_dir.parent / "lib")
    assert "" not in ld


def test_gcm_run_sh_appends_basedir_when_set(tmp_path):
    bin_dir = _make_fake_install(tmp_path)
    basedir = tmp_path / "basedir"
    env = _run_gcm_run_sh(bin_dir, extra_env={"BASEDIR": str(basedir)})

    arch = os.uname().sysname
    ld = env["LD_LIBRARY_PATH"].split(":")
    assert str(bin_dir.parent / "lib") in ld
    assert str(basedir / arch / "lib") in ld
    assert str(basedir / arch / "bin") == env["PATH"].split(":")[-1]

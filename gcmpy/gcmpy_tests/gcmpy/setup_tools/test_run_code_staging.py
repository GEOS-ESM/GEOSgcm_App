"""
Run-code staging: ``SetupGCM.stage_run_code()`` copies the run-relevant
part of the installed gcmpy package into the experiment directory --
``$EXPDIR/gcm_run.py`` plus the package at ``$EXPDIR/gcmpy`` (so the
script directory lands on sys.path and absolute imports resolve),
excluding the setup-only pieces (setup_tools/, geos_settings/, tests,
other setup entry points, __pycache__).
"""

from pathlib import Path

import gcmpy.setup_tools.setup_main as setup_main
from gcmpy.setup_tools.setup_main import SetupGCM


def _fake_install_gcmpy(bin_dir: Path) -> Path:
    """Build a minimal install/bin/gcmpy tree with run + setup decoys."""
    src = bin_dir / "gcmpy"
    run_pkg = src / "gcmpy"
    for pkg in ("run_tools", "batch_tools", "utils", "setup_tools", "geos_settings"):
        (run_pkg / pkg).mkdir(parents=True)
        (run_pkg / pkg / "mod.py").write_text("# stub\n")
        (run_pkg / pkg / "__pycache__").mkdir()
        (run_pkg / pkg / "__pycache__" / "mod.cpython-312.pyc").write_bytes(b"\x00")
    (run_pkg / "__init__.py").write_text("")
    (src / "gcm_run.py").write_text("# run orchestrator\n")
    (src / "gcm_setup.py").write_text("# setup entry point\n")
    (src / "gcm_create.py").write_text("# setup entry point\n")
    (src / "gcm_configure.py").write_text("# setup entry point\n")
    (src / "README.md").write_text("docs\n")
    (src / "gcmpy_event_tracking.log").write_text("log\n")
    (src / "gcmpy_tests").mkdir()
    (src / "gcmpy_tests" / "test_x.py").write_text("# test\n")
    (src / "gcmpy_doc").mkdir()
    (src / "gcmpy_doc" / "doc.md").write_text("doc\n")
    (src / ".pytest_cache").mkdir()
    (src / ".pytest_cache" / "CACHEDIR.TAG").write_text("tag\n")
    return src


def _bare_setup(exp_dir: Path) -> SetupGCM:
    inst = SetupGCM.__new__(SetupGCM)
    inst.exp_dir = str(exp_dir)
    return inst


def test_stage_run_code_copies_run_side_only(tmp_path, monkeypatch):
    bin_dir = tmp_path / "install" / "bin"
    bin_dir.mkdir(parents=True)
    _fake_install_gcmpy(bin_dir)
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()
    monkeypatch.setattr(setup_main, "pathdict", {"bin": bin_dir})

    inst = _bare_setup(exp_dir)
    inst.stage_run_code()

    # gcm_run.py sits at the experiment root; the package (minus
    # setup-only pieces) directly under $EXPDIR/gcmpy.
    assert (exp_dir / "gcm_run.py").is_file()
    pkg = exp_dir / "gcmpy"
    assert (pkg / "__init__.py").is_file()
    for run_pkg in ("run_tools", "batch_tools", "utils"):
        assert (pkg / run_pkg / "mod.py").is_file()
    # Setup-only pieces are excluded.
    assert not (pkg / "setup_tools").exists()
    assert not (pkg / "geos_settings").exists()
    assert not (exp_dir / "gcmpy_tests").exists()
    assert not (exp_dir / "gcmpy_doc").exists()
    assert not (exp_dir / "gcm_setup.py").exists()
    assert not (exp_dir / "gcm_create.py").exists()
    assert not (exp_dir / "gcm_configure.py").exists()
    assert not (exp_dir / "README.md").exists()
    assert not (exp_dir / "gcmpy_event_tracking.log").exists()
    assert not (exp_dir / ".pytest_cache").exists()
    # Byte-code caches and pytest caches are never copied.
    assert not list(pkg.rglob("__pycache__"))
    assert not list(pkg.rglob("*.pyc"))


def test_stage_run_code_idempotent(tmp_path, monkeypatch):
    bin_dir = tmp_path / "install" / "bin"
    bin_dir.mkdir(parents=True)
    _fake_install_gcmpy(bin_dir)
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()
    monkeypatch.setattr(setup_main, "pathdict", {"bin": bin_dir})

    inst = _bare_setup(exp_dir)
    inst.stage_run_code()
    # A second setup over the same experiment dir must not fail.
    inst.stage_run_code()

    assert (exp_dir / "gcm_run.py").is_file()
    assert (exp_dir / "gcmpy" / "run_tools" / "mod.py").is_file()

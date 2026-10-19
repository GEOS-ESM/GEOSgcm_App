import stat
from pathlib import Path

import pytest

from gcmpy.run_tools.executable_setup import (
    ExecutableNotFoundError,
    setup_executable,
)


def _write_executable(path: Path) -> Path:
    path.write_text("#!/bin/sh\n")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


# ---------------------------------------------------------------------------
# gcm_run.j:436-540 -- GEOSgcm.x resolution (gcmpy divergence: install/bin
# directly, no copy into scratch; EXPDIR fallback for custom builds)
# ---------------------------------------------------------------------------

def test_install_bin_hit(tmp_path):
    geosbin = tmp_path / "install" / "bin"
    geosbin.mkdir(parents=True)
    exe = _write_executable(geosbin / "GEOSgcm.x")
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()

    geosexe, singularity_run = setup_executable(geosbin, exp_dir, {})

    # gcmpy divergence: the executable is resolved from install/bin; the
    # function no longer takes a scratch_dir or copies anything.
    assert geosexe == exe
    assert singularity_run is None


def test_expdir_fallback(tmp_path):
    geosbin = tmp_path / "install" / "bin"
    geosbin.mkdir(parents=True)
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()
    fallback = _write_executable(exp_dir / "GEOSgcm.x")

    geosexe, singularity_run = setup_executable(geosbin, exp_dir, {})

    assert geosexe == fallback
    assert singularity_run is None


def test_install_bin_preferred_over_expdir(tmp_path):
    geosbin = tmp_path / "install" / "bin"
    geosbin.mkdir(parents=True)
    primary = _write_executable(geosbin / "GEOSgcm.x")
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()
    _write_executable(exp_dir / "GEOSgcm.x")

    geosexe, _ = setup_executable(geosbin, exp_dir, {})

    assert geosexe == primary


def test_both_missing_raises(tmp_path):
    geosbin = tmp_path / "install" / "bin"
    geosbin.mkdir(parents=True)
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()

    with pytest.raises(ExecutableNotFoundError):
        setup_executable(geosbin, exp_dir, {})


def test_singularity_gate(tmp_path):
    geosbin = tmp_path / "bin"
    geosbin.mkdir()
    _write_executable(geosbin / "GEOSgcm.x")
    exp_dir = tmp_path / "exp"
    exp_dir.mkdir()

    # is_enabled semantics: '' (empty line-prefix) means enabled.
    with pytest.raises(NotImplementedError):
        setup_executable(geosbin, exp_dir, {"singularity_build": ""})

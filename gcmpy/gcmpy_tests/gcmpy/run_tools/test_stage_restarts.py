from pathlib import Path

from gcmpy.run_tools.stage_restarts import fetch_restarts, stage_coupled_input


def _make_experiment(tmp_path: Path) -> tuple[Path, Path]:
    """Minimal exp/scratch pair with AGCM.rc, HISTORY.rc and cap_restart."""
    exp = tmp_path / "exp"
    scratch = tmp_path / "scratch"
    exp.mkdir()
    scratch.mkdir()

    agcm = (
        "READ_RESTART_BY_FACE: NO\n"
        "DYN_INTERNAL_RESTART_FILE: fvcore_internal_rst\n"
    )
    history = "COLLECTIONS: 'inst1_2d_asm_Nx'\n             ::\n"
    cap = "20000301 210000\n"

    for directory in (exp, scratch):
        (directory / "AGCM.rc").write_text(agcm)
        (directory / "HISTORY.rc").write_text(history)
        (directory / "cap_restart").write_text(cap)

    return exp, scratch


def _fetch(exp: Path, scratch: Path, coupled: bool) -> bool:
    return fetch_restarts(
        exp,
        scratch,
        scratch / "AGCM.rc",
        scratch / "HISTORY.rc",
        scratch / "cap_restart",
        expid="myexp",
        gcmver="v1",
        bctag="TAG",
        bcrslv="C90_R",
        coupled=coupled,
    )


def test_stage_coupled_input_copies_files_only(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    restart_dir = exp / "RESTART"
    (restart_dir / "subdir").mkdir(parents=True)
    (restart_dir / "ocean_state.nc").write_text("ocean")
    (restart_dir / "ice_state.nc").write_text("ice")
    (restart_dir / "subdir" / "nested.nc").write_text("nested")

    stage_coupled_input(exp, scratch)

    input_dir = scratch / "INPUT"
    assert (input_dir / "ocean_state.nc").read_text() == "ocean"
    assert (input_dir / "ice_state.nc").read_text() == "ice"
    # Plain csh `cp` without -r never copies subdirectories.
    assert not (input_dir / "subdir").exists()


def test_stage_coupled_input_missing_restart_dir(tmp_path):
    exp, scratch = _make_experiment(tmp_path)

    stage_coupled_input(exp, scratch)

    # csh's failed `$EXPDIR/RESTART/*` glob is non-fatal; INPUT still
    # gets created (matching the separate `mkdir INPUT` line).
    assert (scratch / "INPUT").is_dir()
    assert not any((scratch / "INPUT").iterdir())


def test_stage_coupled_input_idempotent(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    restart_dir = exp / "RESTART"
    restart_dir.mkdir()
    (restart_dir / "ocean_state.nc").write_text("ocean")

    stage_coupled_input(exp, scratch)
    stage_coupled_input(exp, scratch)

    assert (scratch / "INPUT" / "ocean_state.nc").exists()


def test_stage_coupled_input_follows_symlinks(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    restart_dir = exp / "RESTART"
    restart_dir.mkdir()
    target = tmp_path / "real.nc"
    target.write_text("linked")
    (restart_dir / "link.nc").symlink_to(target)

    stage_coupled_input(exp, scratch)

    copied = scratch / "INPUT" / "link.nc"
    assert copied.is_file() and not copied.is_symlink()
    assert copied.read_text() == "linked"


def test_fetch_restarts_stages_input_when_coupled(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    (exp / "fvcore_internal_rst").write_bytes(b"\x00" * 16)
    restart_dir = exp / "RESTART"
    restart_dir.mkdir()
    (restart_dir / "ocean_state.nc").write_text("ocean")

    rst_by_face = _fetch(exp, scratch, coupled=True)

    assert rst_by_face is False
    # gcm_run.j:619-620 -- pre-launch INPUT seeding.
    assert (scratch / "INPUT" / "ocean_state.nc").read_text() == "ocean"
    # Staged restart + coupled initial tar (includes RESTART.{edate}).
    assert (scratch / "fvcore_internal_rst").exists()
    tar_path = exp / "restarts" / "restarts.e20000301_21z.tar"
    assert tar_path.exists()
    import tarfile

    with tarfile.open(tar_path) as tar:
        names = tar.getnames()
    assert "RESTART.e20000301_21z" in names
    assert any("fvcore_internal_rst" in name for name in names)


def test_fetch_restarts_skips_input_when_uncoupled(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    (exp / "fvcore_internal_rst").write_bytes(b"\x00" * 16)
    restart_dir = exp / "RESTART"
    restart_dir.mkdir()
    (restart_dir / "ocean_state.nc").write_text("ocean")

    _fetch(exp, scratch, coupled=False)

    assert not (scratch / "INPUT").exists()
    # The uncoupled archive must not include the RESTART/ directory.
    import tarfile

    tar_path = exp / "restarts" / "restarts.e20000301_21z.tar"
    with tarfile.open(tar_path) as tar:
        names = tar.getnames()
    assert not any(name.startswith("RESTART.") for name in names)

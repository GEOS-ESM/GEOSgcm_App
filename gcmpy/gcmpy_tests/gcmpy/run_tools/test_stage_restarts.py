from pathlib import Path

import pytest

from gcmpy.run_tools.stage_restarts import (
    fetch_restarts,
    force_num_readers_1_for_binary_restarts,
    stage_coupled_input,
)


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


def _fetch(exp: Path, scratch: Path, coupled: bool, **kwargs) -> bool:
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
        **kwargs,
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


@pytest.mark.parametrize('coupled', [False, True])
@pytest.mark.parametrize('skip', [None, False, True])
@pytest.mark.parametrize('mode', ['copy', 'link'])
def test_optional_initial_archive_preserves_staging(tmp_path, coupled, skip, mode):
    exp, scratch = _make_experiment(tmp_path)
    (exp / 'fvcore_internal_rst').write_bytes(b'\x00' * 16)
    with (scratch / 'AGCM.rc').open('a') as f:
        f.write('NUM_READERS: 6\n')
    (exp / 'RESTART').mkdir()
    (exp / 'RESTART' / 'ocean_state.nc').write_text('ocean')
    kwargs = {} if skip is None else {'skip_initial_restart_archive': skip}

    assert _fetch(exp, scratch, coupled, stage_restarts_in_scratch=mode, **kwargs) is False

    assert (scratch / 'fvcore_internal_rst').read_bytes() == b'\x00' * 16
    assert (scratch / 'fvcore_internal_rst').is_symlink() == (mode == 'link')
    assert 'NUM_READERS: 1\n' in (scratch / 'AGCM.rc').read_text()
    assert (scratch / 'INPUT' / 'ocean_state.nc').exists() == coupled
    archive = exp / 'restarts' / 'restarts.e20000301_21z.tar'
    assert archive.exists() == (skip is not True)
    if skip:
        assert not (exp / 'restarts').exists()


def test_binary_restart_forces_num_readers_1(tmp_path):
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    agcm = tmp_path / "AGCM.rc"
    agcm.write_text("NUM_READERS: 6\n")
    # Zero bytes -> application/octet-stream for `file --mime-type`.
    (scratch / "fvcore_internal_rst").write_bytes(b"\x00" * 16)

    found = force_num_readers_1_for_binary_restarts(
        scratch, agcm, ["fvcore_internal_rst"]
    )

    assert found is True
    # gcm_run.j:664 -- NUM_READERS rewritten to 1 (not e.g. 16).
    assert "NUM_READERS: 1\n" == agcm.read_text()


@pytest.mark.parametrize('mode', [None, 'copy', 'link'])
@pytest.mark.parametrize('gcmemip', [False, True])
@pytest.mark.parametrize('by_face', [False, True])
def test_restart_staging_modes(tmp_path, mode, gcmemip, by_face):
    exp, scratch = _make_experiment(tmp_path)
    source = exp / 'restarts' / '20000301' if gcmemip else exp
    source.mkdir(parents=True, exist_ok=True)
    names = ([f'fvcore_internal_face_{n}_rst' for n in range(1, 7)]
             if by_face else ['fvcore_internal_rst'])
    names.append('monthly_rst')
    if not gcmemip:
        names.append('restart.ww3')
    for name in names:
        (source / name).write_bytes(b'\x00' * 16)
    # Also exercise source symlinks (as installed by makeoneday).
    original = tmp_path / 'original_rst'
    original.write_bytes(b'\x00' * 16)
    (source / names[0]).unlink()
    (source / names[0]).symlink_to(original)
    (scratch / 'AGCM.rc').write_text(
        (scratch / 'AGCM.rc').read_text() + 'NUM_READERS: 6\nNUM_WRITERS: 6\n'
    )
    (scratch / 'HISTORY.rc').write_text('monthly.monthly: 1\n')
    (exp / 'RESTART').mkdir()
    (exp / 'RESTART' / 'ocean.nc').write_text('ocean')
    kwargs = {} if mode is None else {'stage_restarts_in_scratch': mode}

    # Restaging replaces existing files/links without modifying their targets.
    for _ in range(2):
        assert _fetch(exp, scratch, True, gcmemip=gcmemip, rstdate='20000301',
                      wavewatch=True, **kwargs) == by_face
        for name in names:
            staged = scratch / name
            assert staged.is_symlink() == (mode == 'link')
            assert staged.read_bytes() == (source / name).read_bytes()
            if mode == 'link':
                assert staged.resolve() == (source / name).resolve()
        ocean = scratch / 'INPUT' / 'ocean.nc'
        assert ocean.is_symlink() == (mode == 'link')
        assert ocean.read_text() == 'ocean'
    assert original.read_bytes() == b'\x00' * 16
    import tarfile

    with tarfile.open(exp / 'restarts' / 'restarts.e20000301_21z.tar') as tar:
        for name in names:
            if name == 'monthly_rst':
                continue  # Monthly checkpoints are staged but not initially archived.
            member = tar.getmember(f'myexp.{name}.e20000301_21z.v1.TAG_C90_R')
            assert member.isfile()
            assert tar.extractfile(member).read() == (source / name).read_bytes()


@pytest.mark.parametrize('mode', ['COPY', '', 'symlink', None, True])
def test_invalid_staging_mode_changes_nothing(tmp_path, mode):
    exp, scratch = _make_experiment(tmp_path)
    (exp / 'fvcore_internal_rst').write_text('original')
    before = {p.name: p.read_bytes() for p in scratch.iterdir()}
    with pytest.raises(ValueError, match='stage_restarts_in_scratch'):
        _fetch(exp, scratch, True, stage_restarts_in_scratch=mode)
    assert {p.name: p.read_bytes() for p in scratch.iterdir()} == before
    assert not (exp / 'restarts').exists()


def test_copy_mode_replaces_old_link_without_writing_through_it(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    original = tmp_path / 'old_restart'
    original.write_text('do not change')
    (scratch / 'fvcore_internal_rst').symlink_to(original)
    (exp / 'fvcore_internal_rst').write_text('new restart')

    _fetch(exp, scratch, False)

    assert not (scratch / 'fvcore_internal_rst').is_symlink()
    assert (scratch / 'fvcore_internal_rst').read_text() == 'new restart'
    assert original.read_text() == 'do not change'


def test_netcdf_restart_leaves_num_readers_alone(tmp_path):
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    agcm = tmp_path / "AGCM.rc"
    agcm.write_text("NUM_READERS: 6\n")
    # Plain text content is not application/octet-stream.
    (scratch / "fvcore_internal_rst").write_text("netcdf-ish")

    found = force_num_readers_1_for_binary_restarts(
        scratch, agcm, ["fvcore_internal_rst"]
    )

    assert found is False
    assert "NUM_READERS: 6\n" == agcm.read_text()


def test_binary_override_skips_missing_restarts(tmp_path):
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    agcm = tmp_path / "AGCM.rc"
    agcm.write_text("NUM_READERS: 6\n")

    # csh checks `-e $rst` before probing the type.
    found = force_num_readers_1_for_binary_restarts(
        scratch, agcm, ["not_staged.nc"]
    )

    assert found is False
    assert "NUM_READERS: 6\n" == agcm.read_text()


def test_fetch_restarts_applies_binary_num_readers_override(tmp_path):
    exp, scratch = _make_experiment(tmp_path)
    # Add NUM_READERS to both AGCM copies (exp seed + already-populated scratch).
    for directory in (exp, scratch):
        (directory / "AGCM.rc").write_text(
            (directory / "AGCM.rc").read_text() + "NUM_READERS: 6\n"
        )
    (exp / "fvcore_internal_rst").write_bytes(b"\x00" * 32)

    _fetch(exp, scratch, coupled=False)

    assert (scratch / "fvcore_internal_rst").exists()
    assert "NUM_READERS: 1" in (scratch / "AGCM.rc").read_text()

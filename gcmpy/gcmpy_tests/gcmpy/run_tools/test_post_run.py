from pathlib import Path

from gcmpy.run_tools.post_run import (
    copy_restarts_to_exp_dir,
    move_history_to_holding,
    move_ocean_outputs,
    run_forecast_setup,
    run_post_processing,
)


def _write_executable(path: Path, body: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body)
    path.chmod(0o755)
    return path


# ---------------------------------------------------------------------------
# gcm_run.j:1314-1333 -- HISTORY files to holding
# ---------------------------------------------------------------------------

def test_move_history_to_holding(tmp_path):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    (exp / "holding" / "inst1_2d_asm_Nx").mkdir(parents=True)
    scratch.mkdir()

    history = scratch / "myexp.inst1_2d_asm_Nx.t12z.nc4"
    history.write_text("output")
    unrelated = scratch / "AGCM.rc"
    unrelated.write_text("config")

    move_history_to_holding(
        scratch, exp, collections=["inst1_2d_asm_Nx"]
    )

    moved = exp / "holding" / "inst1_2d_asm_Nx" / history.name
    assert moved.exists()
    assert moved.read_text() == "output"
    assert not history.exists()
    assert unrelated.exists()


def test_move_history_with_no_matching_files(tmp_path):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    (exp / "holding" / "coll").mkdir(parents=True)
    scratch.mkdir()

    move_history_to_holding(scratch, exp, collections=["coll"])


# ---------------------------------------------------------------------------
# gcm_run.j:1337-1352 -- MOM6/CICE6 output moves
# ---------------------------------------------------------------------------

def test_move_ocean_outputs_mom6(tmp_path):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    scratch.mkdir()
    exp.mkdir()
    for dset in ("ocean_state", "prog_z", "sfc_ave", "forcing"):
        (scratch / f"{dset}.nc").write_text(dset)

    move_ocean_outputs(
        scratch, exp, edate="e20000301_21z", mom6=True, cice6=False
    )

    for dset in ("ocean_state", "prog_z", "sfc_ave", "forcing"):
        moved = exp / "MOM_Output" / f"{dset}.e20000301_21z.nc"
        assert moved.exists()
        assert moved.read_text() == dset
    assert not (scratch / "ocean_state.nc").exists()


def test_move_ocean_outputs_cice6(tmp_path):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    scratch.mkdir()
    exp.mkdir()
    (scratch / "iceh.200003.nc").write_text("ice")
    (scratch / "iceh.200004.nc").write_text("ice")
    (scratch / "other.nc").write_text("keep")

    move_ocean_outputs(
        scratch, exp, edate="e20000301_21z", mom6=False, cice6=True
    )

    assert (exp / "CICE_Output" / "iceh.200003.nc").exists()
    assert (exp / "CICE_Output" / "iceh.200004.nc").exists()
    assert (scratch / "other.nc").exists()


def test_move_ocean_outputs_disabled_is_noop(tmp_path):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    scratch.mkdir()
    (scratch / "ocean_state.nc").write_text("keep")

    move_ocean_outputs(
        scratch, exp, edate="e20000301_21z", mom6=False, cice6=False
    )

    assert (scratch / "ocean_state.nc").exists()
    assert not (exp / "MOM_Output").exists()


# ---------------------------------------------------------------------------
# gcm_run.j:1356-1361 -- gcmpost.script
# ---------------------------------------------------------------------------

def test_run_post_processing_invokes_script(tmp_path):
    exp = tmp_path / "exp"
    exp.mkdir()
    install = tmp_path / "install"
    _write_executable(
        install / "post" / "gcmpost.script",
        '#!/bin/bash\ntouch "$2/gcmpost_ran"\n',
    )

    run_post_processing(exp, str(install))

    assert (exp / "gcmpost_ran").exists()


def test_run_post_processing_missing_script_warns(tmp_path):
    exp = tmp_path / "exp"
    exp.mkdir()

    # Missing install path and missing script both skip quietly.
    run_post_processing(exp, "")
    run_post_processing(exp, str(tmp_path / "nowhere"))


# ---------------------------------------------------------------------------
# gcm_run.j:1363-1378 -- cycled forecast setup
# ---------------------------------------------------------------------------

def _forecast_fixture(tmp_path, beg="19000101", end="20991231"):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    scratch.mkdir()
    exp.mkdir()
    (exp / "CAP.rc").write_text(
        f" BEG_REPDATE: {beg}\n END_REPDATE: {end}\n"
    )
    _write_executable(
        exp / "forecasts" / "gcm_forecast.setup",
        '#!/bin/bash\necho "$1 $2 $3 $4 CYCLED=$CYCLED" '
        "> forecast_marker.txt\n",
    )
    return scratch, exp


def test_forecast_setup_disabled_for_normal_segments(tmp_path):
    scratch, exp = _forecast_fixture(tmp_path)

    run_forecast_setup(scratch, exp, "00000000", "20000301", "210000")

    assert not (scratch / "forecast_marker.txt").exists()


def test_forecast_setup_runs_inside_replay_window(tmp_path):
    scratch, exp = _forecast_fixture(tmp_path)

    # nday=1 -> dt = 10800 - 86400 = -75600 s = -21 h
    # 20000301 21:00 - 21 h = 20000301 00:00
    run_forecast_setup(scratch, exp, "00000001", "20000301", "210000")

    marker = scratch / "forecast_marker.txt"
    assert marker.exists()
    assert marker.read_text() == "20000301 20000301 1 TRUE CYCLED=.TRUE.\n"


def test_forecast_setup_outside_window_is_skipped(tmp_path):
    scratch, exp = _forecast_fixture(tmp_path, beg="20500101", end="20501231")

    run_forecast_setup(scratch, exp, "00000001", "20000301", "210000")

    assert not (scratch / "forecast_marker.txt").exists()


def test_forecast_setup_placeholder_window_is_skipped(tmp_path):
    scratch, exp = _forecast_fixture(tmp_path, beg="YYYYMMDD", end="YYYYMMDD")

    run_forecast_setup(scratch, exp, "00000001", "20000301", "210000")

    assert not (scratch / "forecast_marker.txt").exists()


# ---------------------------------------------------------------------------
# gcm_run.j:1403-1431 -- copy restarts back to the experiment
# ---------------------------------------------------------------------------

def _copy_fixture(tmp_path):
    scratch = tmp_path / "scratch"
    exp = tmp_path / "exp"
    scratch.mkdir()
    exp.mkdir()
    (scratch / "fvcore_rst").write_text("fresh")
    (scratch / "moist_rst").write_text("fresh")
    (scratch / "cap_restart").write_text("20000302 210000")
    return scratch, exp


def test_copy_restarts_to_exp_dir_normal(tmp_path):
    scratch, exp = _copy_fixture(tmp_path)
    (exp / "fvcore_rst").write_text("stale")
    (exp / "restart.ww3").write_text("stale-ww3")
    (scratch / "restart.ww3").write_text("fresh-ww3")

    copy_restarts_to_exp_dir(
        scratch, exp, gcmemip=False, rstdate="", wavewatch=True, coupled=False
    )

    assert (exp / "fvcore_rst").read_text() == "fresh"
    assert (exp / "moist_rst").read_text() == "fresh"
    assert (exp / "cap_restart").read_text() == "20000302 210000"
    assert (exp / "restart.ww3").read_text() == "fresh-ww3"


def test_copy_restarts_to_exp_dir_gcmemip(tmp_path):
    scratch, exp = _copy_fixture(tmp_path)
    rstdate = "20000301"
    (exp / "restarts" / rstdate).mkdir(parents=True)
    (scratch / "restart.ww3").write_text("ww3")

    copy_restarts_to_exp_dir(
        scratch, exp, gcmemip=True, rstdate=rstdate, wavewatch=True,
        coupled=False,
    )

    dest = exp / "restarts" / rstdate
    assert (dest / "fvcore_rst").read_text() == "fresh"
    assert (dest / "cap_restart").read_text() == "20000302 210000"
    # The GCMEMIP branch of gcm_run.j has no WaveWatch3 handling.
    assert not (dest / "restart.ww3").exists()
    assert not (exp / "restart.ww3").exists()


def test_copy_restarts_to_exp_dir_coupled(tmp_path):
    scratch, exp = _copy_fixture(tmp_path)
    (scratch / "RESTART").mkdir()
    (scratch / "RESTART" / "ocean_internal_rst").write_text("ocean")
    (exp / "RESTART").mkdir()
    (exp / "RESTART" / "keep_me").write_text("kept")

    copy_restarts_to_exp_dir(
        scratch, exp, gcmemip=False, rstdate="", wavewatch=False,
        coupled=True,
    )

    # `cp -rf RESTART $EXPDIR` merges into the existing directory.
    assert (exp / "RESTART" / "ocean_internal_rst").exists()
    assert (exp / "RESTART" / "keep_me").read_text() == "kept"

import os
import stat
from pathlib import Path

import pytest

from gcmpy.run_tools.pre_run import (
    check_merra2ox_dates,
    configure_cice6_pond_restart,
    configure_gwd_restarts,
    configure_replay,
    ensure_tile_bin,
    rename_rrtmgp_instance_files,
    validate_openwater_tiles,
)


def _write_executable(path: Path, body: str) -> Path:
    path.write_text(body)
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


# ---------------------------------------------------------------------------
# gcm_run.j:886-894 -- RRTMGP instance-file renaming
# ---------------------------------------------------------------------------

def test_rename_rrtmgp_instance_files(tmp_path):
    instance = tmp_path / "GOCART_instance_1.rc"
    instance.write_text(
        "RRTMG band selection\nalready RRTMGP band\nRRTMG_path: foo\n"
    )
    other = tmp_path / "other_config.rc"
    other.write_text("RRTMG band selection\n")

    rename_rrtmgp_instance_files(tmp_path)

    # \b guard: a standalone RRTMG is rewritten, an already-converted
    # RRTMGP and a name like RRTMG_path (underscore is a word character,
    # exactly as in GNU sed) are left alone, as are non-instance files.
    assert instance.read_text() == (
        "RRTMGP band selection\nalready RRTMGP band\nRRTMG_path: foo\n"
    )
    assert other.read_text() == "RRTMG band selection\n"


# ---------------------------------------------------------------------------
# gcm_run.j:901-921 -- ridge-scheme GWD restart handling
# ---------------------------------------------------------------------------

def test_gwd_face_layout_only_warns(tmp_path):
    agcm_rc = tmp_path / "AGCM.rc"
    agcm_rc.write_text("FOO: 1\nNCAR_NRDG: 5\n")

    configure_gwd_restarts(tmp_path, rst_by_face=True)

    assert agcm_rc.read_text() == "FOO: 1\nNCAR_NRDG: 5\n"


def test_gwd_missing_restart_keeps_existing_ncar_nrdg(tmp_path):
    (tmp_path / "AGCM.rc").write_text("FOO: 1\n  NCAR_NRDG: 5  # comment\n")

    configure_gwd_restarts(tmp_path, rst_by_face=False)

    assert "NCAR_NRDG: 0" in (tmp_path / "AGCM.rc").read_text()
    assert "5" not in (tmp_path / "AGCM.rc").read_text()


def test_gwd_missing_restart_appends_ncar_nrdg(tmp_path):
    agcm_rc = tmp_path / "AGCM.rc"
    agcm_rc.write_text("FOO: 1")

    configure_gwd_restarts(tmp_path, rst_by_face=False)

    assert agcm_rc.read_text() == "FOO: 1\nNCAR_NRDG: 0\n"


def test_gwd_restart_present_is_noop(tmp_path):
    agcm_rc = tmp_path / "AGCM.rc"
    agcm_rc.write_text("FOO: 1\n")
    (tmp_path / "gwd_internal_rst").write_text("restart data")

    configure_gwd_restarts(tmp_path, rst_by_face=False)

    assert agcm_rc.read_text() == "FOO: 1\n"


# ---------------------------------------------------------------------------
# gcm_run.j:923-925 -- tile.bin generation
# ---------------------------------------------------------------------------

def test_tile_bin_created_by_binarytile(tmp_path):
    geosbin = tmp_path / "geosbin"
    scratch = tmp_path / "scratch"
    geosbin.mkdir()
    scratch.mkdir()
    (scratch / "tile.data").write_text("tile layout")
    _write_executable(
        geosbin / "binarytile.x",
        "#!/bin/bash\ntouch tile.bin\n",
    )

    ensure_tile_bin(scratch, geosbin)

    assert (scratch / "tile.bin").exists()


def test_tile_bin_already_present_skips_run(tmp_path):
    geosbin = tmp_path / "geosbin"
    scratch = tmp_path / "scratch"
    geosbin.mkdir()
    scratch.mkdir()
    (scratch / "tile.bin").write_text("existing")
    _write_executable(
        geosbin / "binarytile.x",
        "#!/bin/bash\nrm -f tile.bin\n",
    )

    ensure_tile_bin(scratch, geosbin)

    assert (scratch / "tile.bin").read_text() == "existing"


def test_tile_bin_missing_helper_warns_only(tmp_path):
    scratch = tmp_path / "scratch"
    scratch.mkdir()

    # No binarytile.x at all -- csh would print "Command not found" and
    # keep going, and so must we.
    ensure_tile_bin(scratch, tmp_path / "geosbin")


# ---------------------------------------------------------------------------
# gcm_run.j:933-942 -- CICE6 pond-state restart flag
# ---------------------------------------------------------------------------

def _make_iced_nc(path: Path, with_apnd: bool):
    import netCDF4

    path.parent.mkdir(parents=True, exist_ok=True)
    with netCDF4.Dataset(path, "w") as dataset:
        name = "apnd" if with_apnd else "salt"
        dataset.createDimension("lon", 4)
        dataset.createVariable(name, "f4", ("lon",))


@pytest.mark.parametrize(
    "with_apnd, initial, expected",
    [
        (True, "    restart_pond_lvl  = .false.\n",
               "    restart_pond_lvl  = .true.\n"),
        (False, "    restart_pond_lvl  = .true.\n",
                "    restart_pond_lvl  = .false.\n"),
        (True, "restart_pond_lvl=.false.\nother = 1\n",
               "    restart_pond_lvl  = .true.\nother = 1\n"),
    ],
)
def test_cice6_pond_flag_follows_restart(tmp_path, with_apnd, initial, expected):
    _make_iced_nc(tmp_path / "INPUT" / "iced.nc", with_apnd)
    ice_in = tmp_path / "ice_in"
    ice_in.write_text(initial)

    configure_cice6_pond_restart(tmp_path)

    assert ice_in.read_text() == expected


def test_cice6_missing_restart_treats_pond_as_absent(tmp_path):
    ice_in = tmp_path / "ice_in"
    ice_in.write_text("    restart_pond_lvl  = .true.\n")

    configure_cice6_pond_restart(tmp_path)

    assert ice_in.read_text() == "    restart_pond_lvl  = .false.\n"


# ---------------------------------------------------------------------------
# gcm_run.j:943-958 -- openwater restart tile-count validation
# ---------------------------------------------------------------------------

def _openwater_fixture(tmp_path, expected_lines, reported_total):
    geosbin = tmp_path / "geosbin"
    scratch = tmp_path / "scratch"
    geosbin.mkdir()
    scratch.mkdir()

    # tile.data rows for tile 0 (openwater) are what `grep '^\s*0'` counts.
    lines = [f"0 {n} 0 0" for n in range(expected_lines)]
    (scratch / "tile.data").write_text("\n".join(lines) + "\n")
    _write_executable(
        geosbin / "rs_numtiles.x",
        "#!/bin/bash\necho \"Total openwater tiles: "
        f"{reported_total}\"\n",
    )
    launcher = _write_executable(
        tmp_path / "launcher",
        "#!/bin/bash\nshift\nexec \"$@\"\n",
    )
    return scratch, geosbin, str(launcher)


def test_openwater_tile_counts_match(tmp_path):
    scratch, geosbin, launcher = _openwater_fixture(
        tmp_path, expected_lines=3, reported_total=3
    )

    validate_openwater_tiles(scratch, geosbin, launcher)


def test_openwater_tile_counts_mismatch_exits_7(tmp_path):
    scratch, geosbin, launcher = _openwater_fixture(
        tmp_path, expected_lines=4, reported_total=3
    )

    with pytest.raises(SystemExit) as excinfo:
        validate_openwater_tiles(scratch, geosbin, launcher)

    assert excinfo.value.code == 7


def test_openwater_launcher_expands_geosbin_env(tmp_path, monkeypatch):
    scratch, geosbin, _ = _openwater_fixture(
        tmp_path, expected_lines=3, reported_total=3
    )
    _write_executable(
        geosbin / "esma_mpirun",
        "#!/bin/bash\nshift\nexec \"$@\"\n",
    )
    monkeypatch.setenv("GEOSBIN", str(geosbin))

    validate_openwater_tiles(scratch, geosbin, "$GEOSBIN/esma_mpirun")


def test_openwater_without_helper_is_skipped(tmp_path):
    scratch = tmp_path / "scratch"
    scratch.mkdir()

    # rs_numtiles.x absent (or not executable) -> no validation at all.
    validate_openwater_tiles(scratch, tmp_path / "geosbin", "srun")


def test_openwater_missing_tile_data_counts_zero(tmp_path):
    scratch, geosbin, launcher = _openwater_fixture(
        tmp_path, expected_lines=0, reported_total=0
    )
    os.remove(scratch / "tile.data")

    validate_openwater_tiles(scratch, geosbin, launcher)


# ---------------------------------------------------------------------------
# gcm_run.j:929-950 -- MERRA2OX consistency check
# ---------------------------------------------------------------------------

def _merra2ox_fixture(tmp_path, pchem_line, cap_restart):
    (tmp_path / "AGCM.rc").write_text(
        "FOO: 1\n" + (pchem_line + "\n" if pchem_line else "")
    )
    (tmp_path / "cap_restart").write_text(cap_restart)


def test_merra2ox_after_validity_end_exits_8(tmp_path):
    _merra2ox_fixture(
        tmp_path, "pchem_clim_years: 48", "20260901 210000"
    )

    with pytest.raises(SystemExit) as excinfo:
        check_merra2ox_dates(tmp_path)

    assert excinfo.value.code == 8


def test_merra2ox_before_validity_end_passes(tmp_path):
    _merra2ox_fixture(
        tmp_path, "pchem_clim_years: 48", "20260701 210000"
    )

    check_merra2ox_dates(tmp_path)


def test_non_merra2ox_climatology_skips_check(tmp_path):
    _merra2ox_fixture(
        tmp_path, "pchem_clim_years: 1", "20180701 210000"
    )

    check_merra2ox_dates(tmp_path)


def test_missing_pchem_key_skips_check(tmp_path):
    _merra2ox_fixture(tmp_path, "", "20180701 210000")

    check_merra2ox_dates(tmp_path)


# ---------------------------------------------------------------------------
# gcm_run.j:996-1022 -- REPLAY-mode GAAS/forcing-file linking
# ---------------------------------------------------------------------------

REPLAY_AGCM_RC = """
REPLAY_MODE: {mode}
REPLAY_ANA_EXPID: ia1
REPLAY_ANA_LOCATION: {ana_location}
REPLAY_FILE: {replay_file}
REPLAY_FILE09: {replay_file09}
"""


def _replay_fixture(tmp_path, mode="Exact", replay_file="das/aod",
                    replay_file09="das/od"):
    ana_location = tmp_path / "ana"
    for name in ("chem", "das", "other"):
        (ana_location / name).mkdir(parents=True, exist_ok=True)
        (ana_location / name / "data.nc").write_text(name)

    scratch = tmp_path / "scratch"
    scratch.mkdir()
    gaas = "ROOT: GAAS\nmap: das.aod_\n"
    (scratch / "GAAS_GridComp_ExtData.yaml").write_text(gaas)
    (scratch / "GAAS_GridComp_ExtData.rc").write_text(gaas)
    (scratch / "AGCM.rc").write_text(
        REPLAY_AGCM_RC.format(
            mode=mode,
            ana_location=ana_location,
            replay_file=replay_file,
            replay_file09=replay_file09,
        )
    )
    return scratch, ana_location


def test_replay_rewrites_gaas_and_links_forcing(tmp_path):
    scratch, ana_location = _replay_fixture(tmp_path)

    configure_replay(scratch)

    expected = "ROOT: GAAS\nmap: chem/Y%y4/M%m2/ia1.aod_\n"
    assert (scratch / "GAAS_GridComp_ExtData.yaml").read_text() == expected
    assert (scratch / "GAAS_GridComp_ExtData.rc").read_text() == expected
    # The .tmpl backup keeps the original template.
    assert (scratch / "GAAS_GridComp_ExtData.yaml.tmpl").read_text().startswith(
        "ROOT: GAAS"
    )
    assert "das.aod_" in (
        scratch / "GAAS_GridComp_ExtData.yaml.tmpl"
    ).read_text()

    assert (scratch / "chem").is_symlink()
    assert (scratch / "chem").resolve() == (ana_location / "chem").resolve()
    assert (scratch / "das").is_symlink()
    assert (scratch / "das").resolve() == (ana_location / "das").resolve()


def test_replay_disabled_is_noop(tmp_path):
    scratch, ana_location = _replay_fixture(tmp_path, mode="Disabled")

    configure_replay(scratch)

    assert "das.aod_" in (scratch / "GAAS_GridComp_ExtData.yaml").read_text()
    assert not (scratch / "GAAS_GridComp_ExtData.yaml.tmpl").exists()
    assert not (scratch / "chem").exists()
    assert not (scratch / "das").exists()


def test_replay_percent_type_links_ana_location(tmp_path):
    scratch, ana_location = _replay_fixture(
        tmp_path, replay_file="Y%y4/aod"
    )

    configure_replay(scratch)

    # First segment carries a format placeholder -> grep -v % drops it and
    # `ln -sf ${ANA_LOCATION}/ .` links ANA_LOCATION under its basename.
    link = scratch / ana_location.name
    assert link.is_symlink()
    assert link.resolve() == ana_location.resolve()
    # The plain (placeholder-free) file still links normally.
    assert (scratch / "das").resolve() == (ana_location / "das").resolve()


def test_replay_rerun_refreshes_existing_symlinks(tmp_path):
    scratch, ana_location = _replay_fixture(tmp_path)

    configure_replay(scratch)
    configure_replay(scratch)

    assert (scratch / "chem").is_symlink()
    assert (scratch / "chem").resolve() == (ana_location / "chem").resolve()

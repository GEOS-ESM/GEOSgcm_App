from pathlib import Path

from gcmpy.run_tools.extdata import (
    generate_extdata_file,
    patch_wsub_extdata,
    prune_chem_collections,
    stage_extdata,
)

CHEM_RC = """\
ENABLE_STRATCHEM: .FALSE.
ENABLE_GMICHEM: .TRUE.
ENABLE_GEOSCHEM: .FALSE.
ENABLE_CARMA: .FALSE.
ENABLE_DNA: .FALSE.
ENABLE_ACHEM: .FALSE.
"""


def test_prune_renames_only_disabled_collections(tmp_path):
    (tmp_path / "GEOS_ChemGridComp.rc").write_text(CHEM_RC)
    (tmp_path / "StratChem_ExtData.rc").write_text("disabled")
    (tmp_path / "GMI_ExtData.rc").write_text("enabled")

    prune_chem_collections(tmp_path)

    # Disabled component -> renamed out of the way (gcm_run.j:841-852).
    assert (tmp_path / "StratChem_ExtData.rc.NOT_USED").exists()
    assert not (tmp_path / "StratChem_ExtData.rc").exists()
    # Enabled component untouched.
    assert (tmp_path / "GMI_ExtData.rc").exists()


def test_prune_missing_chem_rc_treats_all_disabled(tmp_path):
    # gcm_run.j's `grep ... | wc -l` yields 0 for a missing file, so every
    # gated collection is renamed even without GEOS_ChemGridComp.rc.
    (tmp_path / "StratChem_ExtData.rc").write_text("x")
    (tmp_path / "DNA_ExtData.rc").write_text("x")

    prune_chem_collections(tmp_path)

    assert (tmp_path / "StratChem_ExtData.rc.NOT_USED").exists()
    assert (tmp_path / "DNA_ExtData.rc.NOT_USED").exists()


def test_prune_missing_everything_is_noop(tmp_path):
    prune_chem_collections(tmp_path)


def test_patch_wsub_extdata_redirects_wsub_clim(tmp_path):
    wsub = tmp_path / "WSUB_ExtData.yaml"
    wsub.write_text(
        "collections:\n"
        "- collection: WSUB_SWclim.v1\n"
        "  filename: wsub.nc4\n"
    )

    patch_wsub_extdata(tmp_path)

    text = wsub.read_text()
    assert "collection: /dev/null" in text
    assert "WSUB_SWclim" not in text


def test_patch_wsub_extdata_missing_file_is_noop(tmp_path):
    patch_wsub_extdata(tmp_path)


def _write_constructor(geosbin: Path):
    geosbin.mkdir(parents=True, exist_ok=True)
    (geosbin / "construct_extdata_yaml_list.py").write_text(
        "from pathlib import Path\n"
        "Path('extdata.yaml').write_text('subconfigs:\\n - GMI_ExtData.yaml\\n')\n"
    )


def test_generate_extdata_file_runs_constructor(tmp_path):
    geosbin = tmp_path / "bin"
    _write_constructor(geosbin)
    rc_dir = tmp_path / "scratch"
    rc_dir.mkdir()

    generate_extdata_file(rc_dir, geosbin)

    assert (rc_dir / "extdata.yaml").exists()
    assert (rc_dir / "ExtData.rc").exists()


def test_generate_extdata_file_missing_constructor_warns_only(tmp_path):
    geosbin = tmp_path / "bin"
    geosbin.mkdir()
    rc_dir = tmp_path / "scratch"
    rc_dir.mkdir()
    (rc_dir / "ExtData.rc").write_text("stale")

    # gcm_run.j runs the constructor without `set -e`: failure must not
    # abort the segment, and ExtData.rc is recreated as a placeholder.
    generate_extdata_file(rc_dir, geosbin)

    assert (rc_dir / "ExtData.rc").exists()
    assert (rc_dir / "ExtData.rc").read_text() == ""


def test_stage_extdata_orchestrates_all_steps(tmp_path):
    geosbin = tmp_path / "bin"
    _write_constructor(geosbin)
    rc_dir = tmp_path / "scratch"
    rc_dir.mkdir()
    (rc_dir / "GEOS_ChemGridComp.rc").write_text(CHEM_RC)
    (rc_dir / "StratChem_ExtData.rc").write_text("x")
    (rc_dir / "GMI_ExtData.rc").write_text("x")
    (rc_dir / "WSUB_ExtData.yaml").write_text("- collection: WSUB_SWclim.v1\n")

    stage_extdata(rc_dir, geosbin, turn_off_wsub=True)

    assert (rc_dir / "StratChem_ExtData.rc.NOT_USED").exists()
    assert (rc_dir / "GMI_ExtData.rc").exists()
    assert "collection: /dev/null" in (rc_dir / "WSUB_ExtData.yaml").read_text()
    assert (rc_dir / "extdata.yaml").exists()
    assert (rc_dir / "ExtData.rc").exists()


def test_stage_extdata_wsub_disabled_leaves_yaml(tmp_path):
    geosbin = tmp_path / "bin"
    _write_constructor(geosbin)
    rc_dir = tmp_path / "scratch"
    rc_dir.mkdir()
    (rc_dir / "GEOS_ChemGridComp.rc").write_text(CHEM_RC)
    (rc_dir / "WSUB_ExtData.yaml").write_text("- collection: WSUB_SWclim.v1\n")

    stage_extdata(rc_dir, geosbin, turn_off_wsub=False)

    assert "WSUB_SWclim" in (rc_dir / "WSUB_ExtData.yaml").read_text()

"""
scratch_setup.py

Populates an already-created, empty scratch/run directory with the files
needed to run a segment: RC files from the experiment directory, the
restart/cap_restart files, the boundary-dataset linking manifest, and the
coupled-ocean model input files (MOM6/CICE6) where applicable.

Assumes the scratch directory itself has already been created (and
cleaned, if pre-existing) by gcmpy.run_tools.scratchdir_ops.create_scratchdir --
this module only *populates* it.

Replaces lines 311-355 of gcm_run.j.
"""

import glob
import shutil
from pathlib import Path

from gcmpy.utils.file_ops import get_val_from_rc_file


def _copy_glob(src_pattern: str, dest_dir: Path) -> None:
    """Copy all files matching src_pattern (non-recursive) into dest_dir."""
    for src in glob.glob(src_pattern):
        src_path = Path(src)
        if src_path.is_file():
            shutil.copy2(src_path, dest_dir / src_path.name)


def populate_scratch_from_home(
    exp_dir: Path | str,
    scratch_dir: Path | str,
    geosbin: Path | str,
    gcmemip: bool = False,
    rstdate: str = "",
    gigatraj_file: str = "",
    mom6: bool = False,
    cice6: bool = False,
) -> dict:
    """
    Top-level entry point: populate the scratch directory with RC files,
    restart bookkeeping files, and coupled-ocean input files from the
    experiment ("home") directory, then read back the segment-control
    values from the copied CAP.rc.

    Note: HOMDIR == EXPDIR always in gcm_run.j (both set to the same
    experiment directory), so a single exp_dir argument covers both.

    Args:
        exp_dir:       Experiment directory (HOMDIR/EXPDIR).
        scratch_dir:   Scratch/run directory (already created+cleaned).
        geosbin:       Path to the GEOS bin directory (for bundleParser.py).
        gcmemip:       True if this is a GCMEMIP run.
        rstdate:       Restart date string (required when gcmemip=True).
        gigatraj_file: Optional GIGATRAJ_PARCELS_FILE name to copy, if set.
        mom6:          True if running the MOM6 ocean model.
        cice6:         True if running the CICE6 seaice model.

    Returns:
        dict with keys END_DATE, NUM_SGMT, FCST_SEGMENT, USE_SHMEM, parsed
        from the scratch directory's CAP.rc.

    Raises:
        RuntimeError: If input.nml is still zero-length after the
        fvcore_layout.rc append+retry (mirrors `exit 2`).
    """
    exp_dir = Path(exp_dir)
    scratch_dir = Path(scratch_dir)
    geosbin = Path(geosbin)

    # -- Copy RC files and restart bookkeeping from $EXPDIR --
    _copy_glob(str(exp_dir / "RC" / "*"), scratch_dir)

    cap_restart_src = exp_dir / "cap_restart"
    if cap_restart_src.exists():
        shutil.copy2(cap_restart_src, scratch_dir / "cap_restart")

    # Replaces the old `cp $EXPDIR/linkbcs .` -- linkbcs.yaml is the
    # manifest produced once at setup time by SetupGCM.stage_bcs(), applied
    # every segment by gcmpy.run_tools.linkbcs_ops.apply_linkbcs().
    linkbcs_manifest_src = exp_dir / "linkbcs.yaml"
    if linkbcs_manifest_src.exists():
        shutil.copy2(linkbcs_manifest_src, scratch_dir / "linkbcs.yaml")

    if gigatraj_file:
        gigatraj_src = exp_dir / gigatraj_file
        if gigatraj_src.exists():
            shutil.copy2(gigatraj_src, scratch_dir / gigatraj_file)

    # -- Copy top-level RC/nml/yaml files and bundleParser.py from $HOMDIR --
    _copy_glob(str(exp_dir / "*.rc"), scratch_dir)
    _copy_glob(str(exp_dir / "*.nml"), scratch_dir)
    _copy_glob(str(exp_dir / "*.yaml"), scratch_dir)

    bundle_parser_src = geosbin / "bundleParser.py"
    if bundle_parser_src.exists():
        shutil.copy2(bundle_parser_src, scratch_dir / "bundleParser.py")

    # -- WaveWatch3 RC files, if present --
    ww3_dir = exp_dir / "RC.ww3"
    if ww3_dir.is_dir():
        _copy_glob(str(ww3_dir / "mod_def.*"), scratch_dir)
        _copy_glob(str(ww3_dir / "ww3*.nml"), scratch_dir)

    # -- Append fvcore_layout.rc into input.nml, with zero-length retry --
    fvcore_layout = scratch_dir / "fvcore_layout.rc"
    input_nml = scratch_dir / "input.nml"
    if fvcore_layout.exists():
        with open(input_nml, "a") as f:
            f.write(fvcore_layout.read_text())

        if input_nml.stat().st_size == 0:
            with open(input_nml, "a") as f:
                f.write(fvcore_layout.read_text())

        if input_nml.stat().st_size == 0:
            raise RuntimeError("input.nml is zero-length")

    # -- Coupled-ocean/seaice input files --
    if mom6:
        for fname in ("MOM_input", "MOM_override"):
            src = exp_dir / fname
            if src.exists():
                shutil.copy2(src, scratch_dir / fname)

    if cice6:
        src = exp_dir / "ice_in"
        if src.exists():
            shutil.copy2(src, scratch_dir / "ice_in")

    # -- GCMEMIP overrides for cap_restart/CAP.rc --
    if gcmemip:
        rstdate_dir = exp_dir / "restarts" / rstdate
        for fname in ("cap_restart", "CAP.rc"):
            src = rstdate_dir / fname
            if src.exists():
                shutil.copy2(src, scratch_dir / fname)

    # -- Read back segment-control values from the now-populated CAP.rc --
    cap_rc = scratch_dir / "CAP.rc"
    return {
        "END_DATE": get_val_from_rc_file(cap_rc, "END_DATE:"),
        "NUM_SGMT": get_val_from_rc_file(cap_rc, "NUM_SGMT:"),
        "FCST_SEGMENT": get_val_from_rc_file(cap_rc, "FCST_SEGMENT:"),
        "USE_SHMEM": get_val_from_rc_file(cap_rc, "USE_SHMEM:"),
    }

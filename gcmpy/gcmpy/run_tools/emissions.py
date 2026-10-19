"""
data_staging/emissions.py

Logic for staging AMIP emissions files based on the vertical level configuration.

Replaces lines 781-790 of gcm_run.j.
"""

import shutil
from pathlib import Path


def _copy_and_patch_emissions(amip_dir: Path, scratch_dir: Path, agcm_lm: int) -> None:
    """
    Copy all .rc and .yaml files from amip_dir into scratch_dir.

    If agcm_lm is not 72, substitutes '/L72/' with '/L{agcm_lm}/' and
    'z72' with 'z{agcm_lm}' in each file's content to match the target
    vertical level count.

    Args:
        amip_dir: Source directory containing the AMIP emissions files.
        scratch_dir: Scratch directory where newly patched emissions files will be copied to.
        agcm_lm:  Number of atmospheric model levels.
    """
    files = list(amip_dir.glob("*.rc")) + list(amip_dir.glob("*.yaml"))

    for src in files:
        dest = scratch_dir / src.name
        dest.unlink(missing_ok=True)

        # dont need this conditional, just always do it
        if agcm_lm == 72:
            shutil.copy2(src, dest)
        else:
            text = src.read_text()
            patched = text.replace(f"/L72/", f"/L{agcm_lm}/").replace(f"z72", f"z{agcm_lm}")
            dest.write_text(patched)


def stage_amip_emissions(
    rc_dir: Path,
    scratch_dir: Path,
    agcm_lm: int,
) -> None:
    """
    Top-level entry point: copy AMIP emissions files into the scratch directory.

    Only applies when EMISSIONS == 'AMIP_EMISSIONS'. For OPS_EMISSIONS runs,
    do not call this function.

    Replaces the simplified gcm_run.j AMIP block:
        set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
        cp ${AMIP_EMISSIONS_DIRECTORY}/*.rc .
        cp ${AMIP_EMISSIONS_DIRECTORY}/*.yaml .

    Args:
        rc_dir: Path to the experiment RC directory (contains AMIP/).
        scratch_dir: Destination directory to copy emissions files into.
        agcm_lm: Number of atmospheric model levels.
    """
    _copy_and_patch_emissions(rc_dir / "AMIP", scratch_dir, agcm_lm)

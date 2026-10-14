"""
data_staging/emissions.py

Logic for staging AMIP emissions files based on the vertical level configuration.

Replaces lines 750-791 of gcm_run.j.
"""

import re
import shutil
from pathlib import Path

from gcmpy.utils.file_ops import get_val_from_rc_file
from gcmpy.utils.regex_ops import extract_line_starting_with, search_replace_in_file

_AMIP_TRANSITION_DATE = 20000301


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


def select_amip_emissions_directory(
    cap_rc_path: Path | str,
    current_date: str,
    segment_finish_date: str,
    segment_finish_time: str,
) -> tuple:
    """
    Select which AMIP emissions subdirectory ('AMIP' or 'AMIP.20C') to use
    for this segment, and clamp the segment finish date to the AMIP
    transition date (2000-03-01) if needed -- rewriting CAP.rc's END_DATE:
    accordingly.

    Replaces:
        set  EXTDATA2G_TRUE = `grep -i '^\\s*USE_EXTDATA2G:\\s*\\.TRUE\\.' CAP.rc | wc -l`
        if( $EXTDATA2G_TRUE == 0 ) then
           set AMIP_Transition_Date = 20000301
           if( $nymdc < ${AMIP_Transition_Date} ) then
                set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP.20C
                if( $nymdf > ${AMIP_Transition_Date} ) then
                 set nymdf = ${AMIP_Transition_Date}
                 set oldstring = `grep '^\\s*END_DATE:' CAP.rc`
                 set newstring = "END_DATE: $nymdf $nhmsf"
                 ... (rewrite CAP.rc END_DATE)
                endif
           else
                set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
           endif
        else
           set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
        endif

    Note: USE_EXTDATA2G is hardcoded '.TRUE.' in gcmpy's CAP.rc.tmpl with
    no user-facing toggle, so the EXTDATA2G_TRUE == 0 (legacy AMIP.20C
    selection) branch below is unreachable for any gcmpy-generated
    experiment today -- but CAP.rc is still read dynamically here in case
    of manual edits, rather than assuming always-True.

    Args:
        cap_rc_path:          Path to the segment's (scratch-copy) CAP.rc.
        current_date:         This segment's current date (YYYYMMDD str).
        segment_finish_date:  This segment's computed finish date (YYYYMMDD str).
        segment_finish_time:  This segment's computed finish time (hhmmss str) --
                              reused as-is in the clamped END_DATE (matches
                              gcm_run.j, which reuses $nhmsf unchanged).

    Returns:
        (amip_subdir, segment_finish_date) tuple:
            amip_subdir:          'AMIP' or 'AMIP.20C'.
            segment_finish_date:  Possibly clamped to the AMIP transition
                                  date (still a zero-padded YYYYMMDD str).
    """
    cap_rc_path = Path(cap_rc_path)

    use_extdata2g = get_val_from_rc_file(cap_rc_path, "USE_EXTDATA2G:")
    extdata2g_true = bool(use_extdata2g) and ".TRUE." in use_extdata2g.upper()

    if extdata2g_true:
        return "AMIP", segment_finish_date

    if int(current_date) < _AMIP_TRANSITION_DATE:
        amip_subdir = "AMIP.20C"

        if int(segment_finish_date) > _AMIP_TRANSITION_DATE:
            segment_finish_date = str(_AMIP_TRANSITION_DATE)

            new_end_date = f"END_DATE: {segment_finish_date} {segment_finish_time}"
            old_end_date_line = extract_line_starting_with(str(cap_rc_path), "END_DATE:")
            if old_end_date_line:
                search_replace_in_file(str(cap_rc_path), re.escape(old_end_date_line), new_end_date)
    else:
        amip_subdir = "AMIP"

    return amip_subdir, segment_finish_date


def stage_amip_emissions(
    rc_dir: Path,
    scratch_dir: Path,
    agcm_lm: int,
    amip_subdir: str = "AMIP",
) -> None:
    """
    Top-level entry point: copy AMIP emissions files into the scratch directory.

    Only applies when EMISSIONS == 'AMIP_EMISSIONS'. For OPS_EMISSIONS runs,
    do not call this function.

    Args:
        rc_dir: Path to the experiment RC directory (contains AMIP/ and,
                optionally, AMIP.20C/).
        scratch_dir: Destination directory to copy emissions files into.
        agcm_lm: Number of atmospheric model levels.
        amip_subdir: Which subdirectory of rc_dir to use ('AMIP' or
                     'AMIP.20C') -- see select_amip_emissions_directory().
    """
    _copy_and_patch_emissions(rc_dir / amip_subdir, scratch_dir, agcm_lm)

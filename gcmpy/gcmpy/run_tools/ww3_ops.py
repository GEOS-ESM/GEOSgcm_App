"""
ww3_ops.py

Sets the WaveWatch3 (ww3_multi.nml) DOMAIN%START/DOMAIN%STOP dates for
this segment.

WaveWatch3 has no representation in gcmpy's setup/config model today (no
ww3_multi.nml template, no setup question) -- this function is only ever
reached when wavewatch=True (see gcmpy.run_tools.restart_metadata.
determine_wavewatch()), which itself only returns True if a WGCM.rc
happens to be manually present, so this is currently unreachable in
practice for gcmpy-generated experiments, similar to MIT ocean/singularity.
It's still implemented correctly for when/if WW3 support is added.

Replaces lines 792-813 of gcm_run.j.
"""

import re
from pathlib import Path

from gcmpy.utils.regex_ops import extract_line_starting_with, search_replace_in_file, normalize_whitespace


def set_ww3_dates(
    scratch_dir: Path | str,
    current_date: str,
    current_time: str,
    experiment_end_date: str,
    experiment_end_time: str,
) -> None:
    """
    Top-level entry point: normalize ww3_multi.nml and rewrite its
    DOMAIN%START/DOMAIN%STOP lines for this segment.

    Replaces:
        cp ww3_multi.nml ww3_multi.nml.orig
        awk '{$1=$1};1' < ww3_multi.nml.orig > ww3_multi.nml

        set oldstring =  `grep '^\\s*DOMAIN%START' ww3_multi.nml`
        set newstring =  "DOMAIN%START = '${nymdc} ${nhmsc}'"
        ... (rewrite)

        set oldstring =  `grep '^\\s*DOMAIN%STOP' ww3_multi.nml`
        set newstring =  "DOMAIN%STOP = '${nymde} ${nhmse}'"
        ... (rewrite)

    Args:
        scratch_dir:         Scratch/run directory containing ww3_multi.nml.
        current_date:        This segment's current date (nymdc).
        current_time:        This segment's current time (nhmsc).
        experiment_end_date: The overall experiment's end date (nymde).
        experiment_end_time: The overall experiment's end time (nhmse).
    """
    nml_path = Path(scratch_dir) / "ww3_multi.nml"
    if not nml_path.exists():
        return

    nml_path.write_text(normalize_whitespace(nml_path.read_text()))

    old_start_line = extract_line_starting_with(str(nml_path), "DOMAIN%START")
    if old_start_line:
        new_start_line = f"DOMAIN%START = '{current_date} {current_time}'"
        search_replace_in_file(str(nml_path), re.escape(old_start_line), new_start_line)

    old_stop_line = extract_line_starting_with(str(nml_path), "DOMAIN%STOP")
    if old_stop_line:
        new_stop_line = f"DOMAIN%STOP = '{experiment_end_date} {experiment_end_time}'"
        search_replace_in_file(str(nml_path), re.escape(old_stop_line), new_stop_line)

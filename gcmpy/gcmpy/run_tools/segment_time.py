"""
segment_time.py

Computes the Current/Ending/Segment-Finish date-time variables for one
segment iteration of the GCM run loop, including the END_DATE clamp and
the non-Reynolds-SST year-boundary clamp (which rewrites CAP.rc).

Replaces lines 673-750 of gcm_run.j.
"""

import re
from pathlib import Path

from gcmpy.utils.clock_ops import create_clock, advance_clock
from gcmpy.utils.regex_ops import extract_line_starting_with, search_replace_in_file
from gcmpy.run_tools.cap_files_ops import get_date_from_cap_restart, get_date_from_cap_rc


def compute_segment_finish_time(
    cap_restart_path: Path | str,
    cap_rc_path: Path | str,
    ocean_res: str,
) -> dict:
    """
    Top-level entry point: compute the Current/Ending/Segment-Finish
    date-time variables for this segment, clamping to the experiment end
    date and (for non-Reynolds SST configurations) to the current
    calendar year.

    Replaces:
        set nymdc = `awk '{print $1}' cap_restart`
        set nhmsc = `awk '{print $2}' cap_restart`
        set nymde = `grep '^\\s*END_DATE:' CAP.rc | cut -d: -f2 | awk '{print $1}'`
        set nhmse = `grep '^\\s*END_DATE:' CAP.rc | cut -d: -f2 | awk '{print $2}'`
        set nymds = `grep '^\\s*JOB_SGMT:' CAP.rc | cut -d: -f2 | awk '{print $1}'`
        set nhmss = `grep '^\\s*JOB_SGMT:' CAP.rc | cut -d: -f2 | awk '{print $2}'`
        ... (segment finish time + END_DATE clamp + non-Reynolds year clamp)

    Args:
        cap_restart_path: Path to the segment's cap_restart file.
        cap_rc_path:      Path to the segment's (scratch-copy) CAP.rc.
        ocean_res:        run_config['ocean']['res'] (e.g. 'DE0360xPE0180').

    Returns:
        dict with keys:
            current_date, current_time:
                This segment's starting date/time (from cap_restart).
            experiment_end_date, experiment_end_time:
                The overall experiment's end date/time (from CAP.rc
                END_DATE:) -- the hard upper bound this segment is
                clamped to.
            segment_length_date, segment_length_time:
                The JOB_SGMT *duration* (not a date/time point) this
                segment should advance by.
            segment_finish_date, segment_finish_time:
                This segment's computed finish date/time, after
                clamping to experiment_end_date/time and (for
                non-Reynolds SST) to the current calendar year.

        All values are returned as zero-padded strings (YYYYMMDD /
        hhmmss), matching the format cap_restart/CAP.rc expect elsewhere.
    """
    cap_rc_path = Path(cap_rc_path)

    cap_restart_vars = get_date_from_cap_restart(cap_restart_path)
    current_date = cap_restart_vars["current_date"]
    current_time = cap_restart_vars["current_time"]

    cap_rc_vars = get_date_from_cap_rc(cap_rc_path)
    experiment_end_date = cap_rc_vars["experiment_end_date"]
    experiment_end_time = cap_rc_vars["experiment_end_time"]
    segment_length_date = cap_rc_vars["segment_length_date"]
    segment_length_time = cap_rc_vars["segment_length_time"]

    # -- Decompose the segment-length duration into year/month/day and
    #    hour/minute/sec offsets --
    offset_years = int(segment_length_date[0:4])
    offset_months = int(segment_length_date[4:6])
    offset_days = int(segment_length_date[6:8])
    offset_hours = int(segment_length_time[0:2])
    offset_minutes = int(segment_length_time[2:4])
    offset_secs = int(segment_length_time[4:6])

    dt = offset_secs + 60 * offset_minutes + 3600 * offset_hours + 86400 * offset_days

    # -- Advance current date/time by dt seconds --
    clock = create_clock(current_date, current_time)
    clock = advance_clock(clock, secs_to_add=dt)

    year = clock.year
    month = clock.month
    day = clock.day

    # -- Add the segment-length month/year offset (replicating the exact
    #    csh carry-the-month-into-year-then-add-years arithmetic, including
    #    not re-validating day-of-month overflow) --
    month = month + offset_months
    while month > 12:
        month -= 12
        year += 1
    year += offset_years

    segment_finish_date = year * 10000 + month * 100 + day
    segment_finish_time = int(f"{clock.hour:02d}{clock.minute:02d}{clock.second:02d}")

    experiment_end_date_int = int(experiment_end_date)
    experiment_end_time_int = int(experiment_end_time)

    # -- Clamp to the experiment end date/time --
    if segment_finish_date > experiment_end_date_int:
        segment_finish_date = experiment_end_date_int
    if segment_finish_date == experiment_end_date_int:
        if segment_finish_time > experiment_end_time_int:
            segment_finish_time = experiment_end_time_int

    # -- Non-Reynolds-SST year-boundary clamp --
    current_year = int(current_date[0:4])
    finish_year = segment_finish_date // 10000

    if ocean_res != "DE0360xPE0180":
        if finish_year > current_year:
            finish_year = current_year + 1
            segment_finish_date = finish_year * 10000 + 101

            new_end_date = f"END_DATE: {segment_finish_date} {segment_finish_time:06d}"
            old_end_date_line = extract_line_starting_with(str(cap_rc_path), "END_DATE:")
            if old_end_date_line:
                search_replace_in_file(str(cap_rc_path), re.escape(old_end_date_line), new_end_date)

    return {
        "current_date": current_date,
        "current_time": current_time,
        "experiment_end_date": experiment_end_date,
        "experiment_end_time": experiment_end_time,
        "segment_length_date": segment_length_date,
        "segment_length_time": segment_length_time,
        "segment_finish_date": f"{segment_finish_date:08d}",
        "segment_finish_time": f"{segment_finish_time:06d}",
    }

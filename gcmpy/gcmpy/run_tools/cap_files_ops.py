
import sys
import datetime as dttm
from pathlib import Path

from gcmpy.utils.clock_ops import advance_clock, create_clock
from gcmpy.utils.regex_ops import search_replace_in_file
from gcmpy.utils.regex_ops import extract_line_starting_with
from gcmpy.utils.file_ops import get_val_from_rc_file
from gcmpy.utils.path_ops import check_file_exists

num_seconds_per_day = 86400


def create_cap_files(dir_name: str, 
                     year: str|int, month: str|int, day: str|int) -> None:
    """
    Use the date information to create the file cap_restart 
    and edit the CAP.rc file in the provided directory. 
    It is assumed that a copy of CAP.rc is already there.

    Parameters
    ----------
    dir_name : str
       The full path to the directorory where CAP.rc is localed and
       cap_restart will be created.
    year : str|int
       The year in the format YYYY
    month : str|int
       The month in the format MM
    day : str|int
       The day in the format DD
    """

    # Ensure the inputs are strings (in case integers were passed)
    # .zfill() adds zeros on the left if necessary
    year_str = str(year).zfill(4)
    month_str = str(month).zfill(2)
    day_str = str(day).zfill(2)

    # Create the cap_restart file
    nymd = f"{year_str}{month_str}{day_str}"
    nhms = "210000"

    cap_restart = Path(dir_name) / "cap_restart"
    cap_restart.write_text(f"{nymd} {nhms}")

    # Advancve the clock
    cur_clock = create_clock(nymd, nhms)
    cur_month = cur_clock.month
    counter = 0
    while counter < 4:
         clock = advance_clock(cur_clock, secs_to_add=num_seconds_per_day)
         month = clock.month
         cur_clock = clock
         if cur_month != month:
             cur_month = month
             counter += 1

    # Edit the file CAP.rc
    cap_rc_file = Path(dir_name) / "CAP.rc"
    if check_file_exists(cap_rc_file):
        old_string = extract_line_starting_with(cap_rc_file, "END_DATE:")
        new_string = f"END_DATE: {clock.strftime('%Y')}{clock.strftime('%m')}01 210000"
        search_replace_in_file(cap_rc_file, old_string, new_string)
    else:
        print(f"The CAP.rc file does not exists in {dir_name}")
        sys.exit(1)

def get_date_from_cap_restart(cap_restart_file: str) -> dict:
    """
    The file cap_restart_file contains one line with the format
    (representing the date and time):

         YYYYMMDD hhmmss

    This function reads the file and returns the date and time.

    Parameters
    ----------
    cap_restart_file : str
       The file name we want to read.

    Returns
    -------
    dict
       {"current_date": YYYYMMDD, "current_time": hhmmss}
    """
    with open(cap_restart_file, 'r') as fid:
        line = fid.readline()

    line_as_list = line.strip().split()
    current_date, current_time = line_as_list[0], line_as_list[1]

    return {"current_date": current_date, "current_time": current_time}

def get_date_from_cap_rc(cap_rc_file: str) -> dict:
    """
    The file cap_rc_file contains several lines including two starting
    with "END_DATE:" and "JOB_SGMT:". This function reads the file,
    gets the two lines and extracts date/time values from each of them.

    Note that JOB_SGMT is a *duration* (the segment length), not a
    calendar date/time -- its YYYYMMDD/hhmmss-shaped value represents
    years/months/days and hours/minutes/seconds to advance by, not a
    point in time.

    Parameters
    ----------
    cap_rc_file : str
       The file name we want to read.

    Returns
    -------
    dict
       {"experiment_end_date": YYYYMMDD, "experiment_end_time": hhmmss,
        "segment_length_date": YYYYMMDD-shaped duration,
        "segment_length_time": hhmmss-shaped duration}
    """
    end_date = get_val_from_rc_file(cap_rc_file, "END_DATE:")
    end_date_list = end_date.strip().split()
    experiment_end_date, experiment_end_time = end_date_list[0], end_date_list[1]

    job_sgmt = get_val_from_rc_file(cap_rc_file, "JOB_SGMT:")
    job_sgmt_list = job_sgmt.strip().split()
    segment_length_date, segment_length_time = job_sgmt_list[0], job_sgmt_list[1]

    return {
        "experiment_end_date": experiment_end_date,
        "experiment_end_time": experiment_end_time,
        "segment_length_date": segment_length_date,
        "segment_length_time": segment_length_time,
    }





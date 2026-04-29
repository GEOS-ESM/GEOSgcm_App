"""
General utilities for operations on date & time.

  - create_clock
  - split_clock
  - advance_clock
  - is_leap_year
  - get_day_of_year

"""
import datetime as dttm

def parse_iso_time(iso_string: str) -> dttm.datetime:
    """
    Creates a datetime object from an ISO 8601 
    formatted time string (YYYY-MM-DDThh:mm:ss)

    Parameters
    ----------
    iso_string : str)
        The ISO formatted time string (e.g., '2026-03-26T14:30:00').

    Returns
    -------
    clock : dttm.datetime
        The parsed datetime object.
    """
    clock = dttm.datetime.fromisoformat(iso_string)

    return clock

def create_clock(nymd: str|int, nhms: str|int) -> dttm.datetime:
    """
    Combine the date (in the format YYYYMMDD) and the time
    (in the format HHMMSS) to create a datetime object.

    Parameters
    ----------
    nymd : str|int
       The date in the format YYYYMMDD (8 characters or 8 digits)
    nhms : str|int
       The time in the format HHMMSS (6 characters or 6 digits)

    Returns
    -------
    clock : dttm.datetime
       The clock representing the combined datetime object.
    """
    # Ensure the inputs are strings (in case integers were passed)
    # .zfill() ensures that early morning times like '93000' become '093000'
    date_str = str(nymd).zfill(8)
    time_str = str(nhms).zfill(6)
    
    # Combine them into a single string: "YYYYMMDDHHMMSS"
    combined_str = f"{date_str}{time_str}"
    
    # %Y = 4-digit year, %m = 2-digit month, %d = 2-digit day
    # %H = 24-hour, %M = minute, %S = second
    clock = dttm.datetime.strptime(combined_str, "%Y%m%d%H%M%S")

    return clock

def split_clock(current_clock: dttm.datetime) -> tuple[str]:
    """
    Splits a datetime object into a date string (YYYYMMDD) 
    and a time string (HHMMSS).

    Parameters
    ----------
    current_clock : dttm.datetime
       The current date & time.

    Returns
    -------
    date_str, time_str : tuple[str]
       The date (YYYYMMDD) and the time (HHMMSS).
    """
    date_str = current_clock.strftime("%Y%m%d")
    
    # %H = 24-hour, %M = minute, %S = second
    time_str = current_clock.strftime("%H%M%S")
    
    return date_str, time_str

def advance_clock(current_clock: dttm.datetime, 
                  days_to_add: int = 0,
                  hours_to_add: int = 0,
                  mins_to_add: int = 0,
                  secs_to_add: int = 0) -> dttm.datetime:
    """
    Advances a given datetime object by a specified number of 
    days, hours, minutes and seconds.

    Parameters
    ----------
    current_clock : dttm.datetime
       The current date & time.
    days_to_add : int
       The numbers of days we want to add.
    hours_to_add : int
       The numbers of hours we want to add.
    mins_to_add : int
       The numbers of minutes we want to add.
    secs_to_add : int
       The numbers of seconds we want to add.

    Returns
    -------
    new_clock : dttm.datetime
       The new date & time after the operation.
    """
    new_clock = current_clock + dttm.timedelta(
            days=days_to_add, hours=hours_to_add, 
            minutes=mins_to_add, seconds=secs_to_add)

    return new_clock

def compute_time_vars_finish(clock_s: dttm.datetime,
                             clock_c: dttm.datetime,
                             clock_e: dttm.datetime):
    nyear = clock_s.year
    nmonth = clock_s.month
    nday = clock_s.day
    nhour = clock_s.hour
    nminute = clock_s.minute
    nsecond = clock_s.second
    dt = nsecond + 60 * nminute + 3600 * nhour + 86400 * nday

    clock = advance_clock(clock_c, secs_to_add=dt)
    year = clock.year
    month = clock.month
    day = clock.day

    month += nmonth
    while month > 12:
        month -= 12
        year += 1

    year += nyear
    nymdf = year * 10000 + month * 100 + day

    nymde = int(clock_e.strftime('%Y%m%d'))
    if nymdf > nymde:
       nymdf = nymde

    if nymdf == nymde:
        if nhmsf > nhmse:
            nhmsf = nhmse

    return dt

def set_datetime_stamp():
    return dttm.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')

def is_leap_year(year: int) -> bool:
    """
    Determine if a year is a leap year or not.
    Criteria:
        - Year must be divisible by 4
        - If it is an 'end of the century' year (a.k.a. divisible by 100)
          then it must be divisible by 400

    Parameters
    ---------
    year : int
        The year as four digits

    Returns
    -------
    bool
        True (for leap year) or False.

    """
    if year < 1:
        raise Exception('Least year is 1 A.D.')
    else:
        return year % 400 == 0 or (year % 4 == 0 and year % 100 != 0)

def get_day_of_year(yyyy: int, mm: int, dd: int) -> int:
    """
    Determine the number of days since January 1st.

    Parameters
    ---------
    yyyy : int
        The year as four digits
    mm : int
        The month as two digits
    dd : int
        The day as two digits

    Returns
    -------
    int
        The number of days since January 1st.
    """
    days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if is_leap_year(yyyy):
        days_per_month[1] = 29

    if (mm < 1 or mm > 12 or
            dd < 1 or dd > days_per_month[mm - 1]):
        raise Exception('Enter valid month (01-12) and appropriate day')

    return sum(days_per_month[0:(mm - 1)]) + dd


"""
Perform operations on restart files
"""

import glob
import subprocess as sp

def run_system_untar(archive_path: str, 
                     extract_path: str = ".",
                     wcard_pattern: str = "") -> None:
    """
    Build the command: 
       tar -xvf archive.tar --wildcards wcard_pattern

    The --wildcards option allows to use pattern matching 
    (globbing) when extracting files from the archive.

    Parameters
    ----------
    archive_path : str
       Path to the file we want to untar.
    extract_path : str
       The folder where the extracted files will be located.
    wcard_pattern : str
       Pattern of the wildcards.
    """
    command = ["tar", "-xvf", archive_path, "-C", extract_path]
    if wcard_pattern
       command += ["--wildcard", f"r{wcard_pattern}"]

    # Run the command and print the output to the console
    try:
        sp.run(command, check=True)
    except sp.CalledProcessError as e:
        print(f"tar command failed with error code: {e.returncode}")


def get_rstid() -> str:
    """
    Implmenting the Bash script:

    set RSTID = `/bin/ls *catch* | /bin/grep -Po '^.*(?=\.\w+_rst\.)'`

    Identify the file which name has the words:
    "catch" and "_rst." and extract from the name
    the substring before the first dot (".").

    For instance, if the file name is:

        experiment_name.catch_ops.geosgcm_rst.nc4

    the function will return: "experiment_name"

    Returns
    -------
    rstid : str
       Prefix of a file which name matches a specific pattern.
    """
    rstid = None
    file = glob.glob("*catch*_rst.*")[0]
    if file:
        rstid = file.strip().split(".")[0]

    return rstid

def get_day() -> str:
    """
    Implmenting the Bash script:

        set day  = `/bin/ls *catch* | /bin/grep -Po '(?<=\d{6})\d{2}(?=_21z)'`

    Identify the file which name has the words:
    "catch" and "_21z". From the file name, extract 
    the day (DD) from the the substring with format
    YYYYMMDD_21z.

    For instance, if the file name is:

        experiment_name.catch_ops.20260419_21z.nc4

    the function will return: "19"

    Returns
    -------
    rstid : str
       Prefix of a file which name matches a specific pattern.
    """
    day = None
    file = glob.glob("*catch*_21z.*")[0]
    if file:
        word = [w for w in file.split(".") if "_21z" in w][0]
        day = word[-6:-4]

    return day

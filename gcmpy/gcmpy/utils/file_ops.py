
import sys
import subprocess as sp
import datetime as dttm
from pathlib import Path
from typing import Any

from gcmpy.utils.regex_ops import extract_line_starting_with

def get_val_from_rc_file(file_name: str | Path, var_name: str) -> Any:
    """
    Read a RC file to extract the value associated with a variable.
    Assume that the RC file contains the lines:

       # BEG_DATE: 20260315 210000
       END_DATE: 20260410 210000

    We expect the variable name to end with the colon character (":").
    However, it will still works if it is not the case as long as
    the the variable is followed by one colon in the file.
    If the function is called with "END_DATE:" or "END_DATE" 
    as var_name, the returned value will be "20260410 210000". 
    If instead we use "BEG_DATE:", the function will return None.

    Parameters
    ----------
    file_name : str | Path
       The file name containing variable of interest.
    var_name : str
       The variable name we want to extract the value.
       Ideally, it should end with the colon character (":").

    Returns
    -------
    var_value : Any
       The value associated with the variable.
    """
    line = extract_line_starting_with(file_name, var_name)
    var_value = None
    if line:
        line = line.strip().split(var_name)[1]
        if line.strip().startswith(":"):
            line = line.strip().split(":")[1]
        line = line.strip().split("#")[0]
        var_value = line.strip()
 
    return var_value

def get_file_type(filepath: str) -> str:
    """
    Use Linux 'file' command to extract the file type.

    Parameters
    ----------
    filepath : str
       File name.

    Returns
    -------
    file_type : str
       The file type. It can be something like:
          "application/octet-stream"
          "application/x-hdf"
          "text/x-python"
    """
    command = ["file", "-Lb", "--mime-type", filepath]

    file_type = None
    try:
        # capture_output=True grabs the stdout, text=True returns it as a string
        result = sp.run(command, capture_output=True, text=True, check=True)

        # .strip() removes the trailing newline character
        file_type = result.stdout.strip()
        return file_type

    except sp.CalledProcessError as e:
        print(f"Command failed: {e.stderr}")
        return file_type



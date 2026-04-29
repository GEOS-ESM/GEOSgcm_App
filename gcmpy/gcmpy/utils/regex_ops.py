"""
Use the regular expression module (re) to perform
basic operations on strings.

The following functions are implemented:
    - does_not_contain_digit
    - is_string_digits
    - is_string_integer
    - is_string_float
    - search_replace_in_string
    - search_replace_in_file
    - multiple_search_replace_in_file
"""

import logging
import sys
import re
from pathlib import Path
from typing import Any, Union, List

from gcmpy.utils.logger_ops import logger_setup
from gcmpy.utils.path_ops import check_file_exists

logger = logger_setup(filename=__name__,
                      file_handler=True,
                      file_level=logging.ERROR,
                      stream_handler=False)

# Set the various pattterns
#    It is best practice to compile the pattern outside 
#    the function. So it is only compiled once when the script 
#    loads, rather than every time the function is called.

DIGIT_PATTERN = re.compile(r'^\d+$')

# Pattern explanation:
# ^          - start of the string
# [-+]?      - optional sign (+ or -)
# [0-9]+     - one or more digits
# $          - end of the string
# The pattern ensures the *entire* string is an integer, not just a substring.
INTEGER_PATTERN = re.compile(r'^[+-]?[0-9]+$')

# Pattern explanation:
# ^          - start of the string
# [-+]?      - optional sign (+ or -)
# [0-9]*     - zero or more digits (optional integer part)
# \.?        - optional decimal point
# [0-9]+     - one or more digits (required for fractional part or if no integer part)
# (?:[eE][-+]?[0-9]+)? - optional exponent part (e/E, optional sign, one or more digits)
# $          - end of the string
# The pattern ensures the *entire* string is a float, not just a substring.
FLOAT_PATTERN = re.compile(r'^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$')

def is_string_digits(mystring: str) -> bool:
    """
    Check if a string is only made up of digits.

    Parameters
    ----------
    mystring : str
      A arbitrary string.

    Returns
    -------
    isdigit : bool
      True if string has only digits (and is not empty) and False otherwise.
    """
    isdigit = bool(DIGIT_PATTERN.match(mystring))

    return isdigit

def is_string_integer(mystring: str) -> bool:
    """
    Check if a string represents an integer .

    Parameters
    ----------
    mystring : str
      A arbitrary string.

    Returns
    -------
    isfloat : bool
      True if string represents an integer and False otherwise.
    """

    # re.match() returns a match object if the pattern matches at the start of the string,
    # or None otherwise. bool() converts the match object to True/False.
    isinteger = bool(INTEGER_PATTERN.match(mystring.strip()))

    return isinteger

def is_string_float(mystring: str) -> bool:
    """
    Check if a string represents a floating point number.

    Parameters
    ----------
    mystring : str
      A arbitrary string.

    Returns
    -------
    isfloat : bool
      True if string represents a float and False otherwise.
    """
    # re.match() returns a match object if the pattern matches at the start of the string,
    # or None otherwise. bool() converts the match object to True/False.
    isfloat = bool(FLOAT_PATTERN.match(mystring.strip()))

    return isfloat

def does_not_contain_digit(mystring: str) -> bool:
    """
    Check if a string does not contain a digit.

    Parameters
    ----------
    mystring : str
      A arbitrary string.

    Returns
    -------
    nodigit : bool
      True if string has no digits and False otherwise.
    """
    onedigit_pattern = re.compile(r'\d')
    nodigit = not bool(onedigit_pattern.match(mystring.strip()))

    return nodigit

def search_replace_in_string(search_pattern: str, 
                             repl_pattern: str, mystring: str) -> str:
    """
    Perform text substitution, replacing all occurrences 
    of a pattern within a string with a specified 
    replacement. It can be used for tasks like data cleaning, 
    reformatting text, and removing unwanted characters.

    Parameters
    ----------
    mystring : str
       The input string where the substitution will take place.
    search_pattern : str
       The regex pattern to search for (use raw strings, e.g., r'pattern').
    replace_pattern : str
       The replacement string (use raw strings, e.g., r'replacement').

    Returns
    -------
    new_string : str
       A new string after the search and replacement.
    """
    new_string = re.sub(search_pattern, repl_pattern, mystring)

    return new_string

def search_replace_in_file(file_path: str, 
                           search_pattern: str, 
                           replace_pattern: str) -> None:
    """
    Performs a regex search and replace on the content of a file.

    Parameters
    ----------
    file_path : str
       The path to the file.
    search_pattern : str
       The regex pattern to search for (use raw strings, e.g., r'pattern').
    replace_pattern : str
       The replacement string (use raw strings, e.g., r'replacement').
    """
    
    # Read the file content
    with open(file_path, 'r') as fid:
        file_content = fid.read()

    # Perform the regex substitution
    # re.sub(pattern, replacement, string, count=0, flags=0)
    # The 'r' prefix creates a raw string, which is 
    #    best practice for regex patterns.
    # Note that the re.subn returns two elements, the second 
    #    being the number of replacements it has made.
    modified_content, num_replaced = re.subn(search_pattern, replace_pattern, file_content)

    # Write the modified content back to the file (only if changes were made)
    if num_replaced > 0:
        with open(file_path, 'w') as fid:
            fid.write(modified_content)
        print(f"Successfully replaced {num_replaced} occurrences of '{search_pattern}' in {file_path}.")
    else:
        print(f"No occurrences of '{search_pattern}' found to replace.")

def multiple_search_replace_in_file(file_path: str,
                                    find_replace_list: list[tuple]) -> None:
    """
    Performs multiple regex searches and replaces on the content of a file.

    Parameters
    ----------
    file_path : str
       The path to the file.
    find_replace_list : list[tuple]
       A list of tuples (each with two entries) where
       the first entry is the regex pattern to search for
       and the second one is replacement string.
    """

    # Read the file content
    with open(file_path, 'r') as fid:
        file_content = fid.read()

    # Loop over the search/replacE
    num_replaced = 0
    for search_pattern, replace_pattern in find_replace_list:
        modified_content, count = re.subn(search_pattern, replace_pattern, file_content)
        num_replaced += count
        file_content = modified_content

    # Write the modified content back to the file (only if changes were made)
    if num_replaced > 0:
        with open(file_path, 'w') as fid:
            fid.write(modified_content)
        print(f"Successfully replaced {num_replaced} occurrences in {file_path}.")

def extract_line_starting_with(file_name: str, search_string: str) -> str:
    """
    Search a file for the first line that starts with a specific string.
    If we are searching for 'END_DATE:', the first occurence of any of
    the following will be returned:

        'END_DATE:     29990302 210000'
        '   END_DATE:     29990302 210000'
        '\t\tEND_DATE:     29990302 210000'
        ' \nEND_DATE:     29990302 210000'
    
    Parameters
    ----------
    file_name : str
       The name of the file we want to search.
    search_string : str 
       The exact string the line should start with.
        
    Returns
    -------
        str: The matching line (stripped of trailing newlines), or None if not found.
    """
    # 1. Compile the regex pattern. 
    # ^ means "start of line". 
    # re.escape() ensures special characters in your search string (like *, ?, or :) 
    # are treated as literal text, not regex commands.
    # We add \s* just in case there are leading spaces/tabs before the string.
    pattern = re.compile(r"^\s*" + re.escape(search_string))

    try:
        with open(file_name, 'r') as fid: #, encoding='utf-8') as fid:
            for line in fid:
                # 2. Check if the line matches the pattern
                if pattern.search(line):
                    return line.strip()  # Return the line without the trailing newline (\n)
    except FileNotFoundError:
        print(f"Error: The file {file_name} was not found.")
        
    return None  # Return None if no match is found

def grep_file(pattern: str, 
              files: Union[str, Path, List[Union[str, Path]]] = None, 
              text: str = None, 
              ignore_case: bool = False
              ) -> str:
    """
    Mimics the Unix 'grep' command. 
    Searches for a regex pattern in text or files.
    
    Parameters
    ----------
    pattern : str
       The regular expression pattern to search for.
    files : str|Path, list[str|Path]
       A single file path or a list of file paths to search.
    text  : str
       raw string of text to search (if you aren't reading from files).
    ignore_case : bool
       If True, performs a case-insensitive search (like grep -i).
        
    Returns
    -------
    grep_str : str
        A string containing all lines that match the pattern, separated by newlines.
    """
    # Compile the regex pattern
    flags = re.IGNORECASE if ignore_case else 0

    try:
        regex = re.compile(pattern, flags)
    except re.error as e:
        return f"grep: invalid regular expression: {e}"

    matched_lines = list()

    # Helper function to scan lines and apply the regex
    def process_content(content: str, prefix: str = ""):
        for line in content.splitlines():
            if regex.search(line):
                # If searching multiple files, prepend the filename like standard grep
                if prefix:
                    matched_lines.append(f"{prefix}:{line}")
                else:
                    matched_lines.append(line)

    # 1. Search directly provided text (acting like piped standard input)
    if text is not None:
        process_content(text)

    # 2. Search within provided file(s)
    if files is not None:
        if isinstance(files, (str, Path)):
            files = [files]
            
        # Standard grep behavior: show the filename if searching multiple files
        more_than_one_file = len(files) > 1 

        for file in files:
            file_path = Path(file)
            try:
                if file_path.is_file():
                    content = file_path.read_text(encoding='utf-8')
                    prefix = str(file_path) if more_than_one_file else ""
                    process_content(content, prefix)
                else:
                    print(f"grep: {path}: Is a directory or does not exist")
            except PermissionError:
                print(f"grep: {path}: Permission denied")
            except Exception as e:
                print(f"grep: {path}: {e}")

    # Return all matching lines as a single string
    if len(matched_lines) == 1:
        grep_str = matched_lines[0]
    else:
        grep_str = "\n".join(matched_lines)

    return grep_str


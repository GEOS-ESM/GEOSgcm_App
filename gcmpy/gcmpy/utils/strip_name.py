"""
Implement the the Cshell script stripname.
It can take up to three command line arguments,
the first one is required.
  arg1: the portion of the name to replace.
        Wildcards (*) may be used in this argument.
  arg2: (optional) is what will replace Arg1
        Default: Arg2 is blank
  arg3: (optional) is the specific file to modify

Usage:
 python strip_name.py <search_pattern> [replacement] [files...]

Examples:
 python strip_name.py stripdate _restart.e19860525_21z _rst
    replaces all occurances of "_restart.e19860525_21z" with "_rst"

 python strip_name.py .e19860525_21z
    replaces all occurances of ".e19860525_21z" with ""
"""
import sys
import os
import glob

def strip_name():
    """
    """

    # Ensure at least the first argument is provided
    if len(sys.argv) < 2:
        print("Usage: python strip_name.py <search_pattern> [replacement] [files...]")
        sys.exit(1)

    # --> Read command line arguments
    # First argument
    arg1 = sys.argv[1]

    # Get the second  argument if privided
    name2 = ""
    if len(sys.argv) > 2:
        name2 = sys.argv[2] if len(sys.argv) > 2 else ""
    
    # Get the third argument if privided
    # It is captured as a list
    user_files = list()
    if len(sys.argv) > 3:
        user_files = sys.argv[3:]

    # --> Extract base name before '*' 
    has_star = '*' in arg1
    if has_star:
        name1 = arg1.split('*')[0]
    else:
        name1 = arg1

    # --> Determine files to process
    # If no files were provided, mimic: ls -1 *$name1*
    if not user_files:
        files = glob.glob(f"*{name1}*")
    else:
        files = user_files

    if not files:
        print("No files found to process.")
        sys.exit(0)

    # --> Redefine name1 if '*' was present 
    if has_star and files:
        first_file = files[0]
        idx = first_file.find(name1)
        if idx != -1:
            # Extract from the start of name1 
            # to the end of the filename
            name1 = first_file[idx:]

    # --> Rename files 
    for filepath in files:
        if not os.path.exists(filepath):
            continue
        
        # replace() does global replacement by default, 
        # matching the 'g' in sed
        new_name = filepath.replace(name1, name2)
        
        if filepath != new_name:
            print(f"Renaming: {filepath} -> {new_name}")
            try:
                os.rename(filepath, new_name)
            except OSError as e:
                print(f"Error renaming {filepath}: {e}")

if __name__ == "__main__":
    strip_name()

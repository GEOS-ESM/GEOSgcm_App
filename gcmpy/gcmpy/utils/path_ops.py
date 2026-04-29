"""
Utilities for running commands.

  - Pathlib Attribute Functions
      * get_file_name
      * get_file_stem
      * get_file_suffix
      * get_file_anchor
      * get_parent
      * get_current_dir
      * get_home_dir

  - File/Directory Manipulation Functions
      * change_dir
      * create_dir
      * create_empty_file
      * copy_file
      * rename_file
      * remove_file
      * move_item
      * cat_files

  - File/Directory Search Functions
      * check_file_exists
      * check_dir_exists
      * find_files
      * grep_file

  - Listing Functions (ls command)
      * list_all_paths
      * list_all
      * list_subfile_paths
      * list_subfiles
      * list_subdir_paths
      * list_subdirs
      * list_subfile_paths_ext
      * list_subfiles_ext

  - Tree Functions (tree command)
      * print_dir_tree
      * print_dir_tree_given_path
      * are_dir_trees_equal
      * compare_directory_trees

  - Other Functions
      * which
      * create_symbolic_link
"""
import os
import sys
import shutil
import filecmp
import logging
import subprocess as sp
from pathlib import Path
from typing import Any, Union, List

from gcmpy.utils.color_ops import Color
from gcmpy.utils.logger_ops import logger_setup

# Logger settings
logger = logger_setup(filename=__name__,
                      file_handler=True,
                      file_level=logging.WARNING,
                      stream_handler=False)


def check_file_exists(file_path: str) -> bool:
    """
    Check if file exists given its path.

    Parameters
    ----------
    file_path : str
        File path.

    Returns
    -------
    result : bool
        Whether file path exists or not
    """
    return Path(file_path).is_file()
    # logger.error(f'The file {file_path} does not exists!')

def check_dir_exists(dir_path: str) -> bool:
    """
    Check if a directory exists given its path.

    Parameters
    ----------
    dir_path : str
        File path.

    Returns
    -------
    result : bool
        Whether file path exists or not
    """
    return Path(dir_path).is_dir()
    # logger.error(f'The directory {dir_path} does not exists!')

def get_path_address(path: Path) -> str:
    """
    Returns the full path address of a Path object

    Parameters
    ----------
    path : Path
        A Path object

    Returns
    -------
    str
        The path address of the Path object

    """
    return str(path)

def get_name(path_name: str) -> str:
    """
    Extracts the file's name given its path
    Note that file does not need to actually exist
    and therefore does not check for that.

    Parameters
    ----------
    path_name : str
        The path of a file

    Returns
    -------
    file_name: str
        The file name of the given file path
    """
    logger.debug("Getting file's name")
    return Path(path_name).name

def get_stem(path_name: str) -> str:
    """
    Extracts the file's stem name given its path
    Note that file does not need to actually exist
    and therefore does not check for that.

    Parameters
    ----------
    path_name : str
        The path of a file

    Returns
    -------
    stem_name: str
        The stem name of the given file path
    """
    logger.debug("Getting file's stem name")
    return Path(path_name).stem

def get_suffix(path_name: str) -> str:
    """
    Extracts the file's suffix name given its path
    Note that file does not need to actually exist
    and therefore does not check for that.

    Parameters
    ----------
    path_name : str
        The path of a file

    Returns
    -------
    suffix_name: str
        The suffix name of the given file path
    """
    logger.debug("Getting file's suffix")
    return Path(path_name).suffix

def get_anchor(path_name: str) -> str:
    """
    Extracts the file's anchor given its path
    Note that file does not need to actually exist
    and therefore does not check for that.

    Parameters
    ----------
    path_name : str
        The path of a file

    Returns
    -------
    anchor: str
        The anchor of the given file path
    """
    logger.debug("Getting file's anchor")
    return Path(path_name).anchor

def get_parent(path_name: str) -> Path:
    """
    Extracts the file's/directory's parent directory given its path
    Note that file does not need to actually exist
    and therefore does not check for that.

    Parameters
    ----------
    path_name : str
        The path of a file

    Returns
    -------
    parent_name: Path
        The path object of the parent directory of the given file path
    """
    logger.debug("Getting file's parent")
    return Path(path_name).parent

def change_dir(dir_path: str) -> None:
    """
    Change the current directory

    Parameters
    ----------
    dir_path : str
        Path of the directory
    """
    if check_dir_exists(dir_path):
        os.chdir(Path(dir_path))
    else:
        logger.error(f'Directory {dir_path} does not exist.')

def get_current_dir() -> Path:
    """
    Get the current directory of the OS.

    Returns
    -------
    cwd: str
        The path object corresponding to the current working directory
    """
    return Path.cwd()

def get_home_dir() -> Path:
    """
    Get the home directory of the OS.

    Returns
    -------
    home_dir: str
        The path object corresponding to the current working directory
    """
    return Path.home()

def create_dir(dir_path: str) -> None:
    """
    Create a directory (with all its parents) and does error checking.

    Parameters
    ----------
    dir_path : str
       Full path of directory that we want to create.
    """
    try:
        Path(dir_path).mkdir(parents=True, exist_ok=True)
        logger.debug(f'Created the directory: {dir_path}')
    except TypeError:
        raise TypeError('Cannot create directory. Please specify a directory path.')
    except PermissionError:
        raise PermissionError('Cannot create directory. Please check directory path.')
    except FileExistsError:
        if Path(dir_path).exists():
            logger.warning(f'Directory already exists: {dir_path}', exc_info=True)
        else:
            logger.error(f'Permission denied: cannot create {dir_path}', exc_info=True)
            sys.exit()

def create_empty_file(file_path: str) -> None:
    """
    Equivalent to Linux 'touch' command

    Parameters
    ----------
    file_path : str
        The full file path or file name
    """
    try:
        Path(file_path).touch(exist_ok=True)
    except FileExistsError:
        logger.warning(f'Could not create the file: {file_path}',
                       exc_info=True)

def cat_files(file_paths: Union[str, Path, List[Union[str, Path]]]) -> str:
    """
    Reads one or more files and concatenates 
    their contents into a single string, mimicking 
    the behavior of the Unix 'cat' command.
    
    Parameters
    ----------
    file_paths : 
       A single file path or a list of file paths.
        
    Returns
    -------
    cat_str : str
        A string containing the concatenated contents of the files.
    """
    # If a single string or Path is provided, convert it to a list
    if isinstance(file_paths, (str, Path)):
        file_paths = [file_paths]
        
    concatenated_text = list()
    
    for file in file_paths:
        file_path = Path(file)
        try:
            # Read the file contents and append to our list
            # read_text() automatically handles opening and closing the file
            content = file_path.read_text(encoding='utf-8')
            concatenated_text.append(content)
        except FileNotFoundError:
            print(f"cat: {file_path}: No such file or directory")
        except IsADirectoryError:
            print(f"cat: {file_path}: Is a directory")
        except PermissionError:
            print(f"cat: {file_path}: Permission denied")
        except Exception as e:
            print(f"cat: {file_path}: An error occurred - {e}")
            
    # Join all the file contents together (efficient string concatenation)
    cat_str = "".join(concatenated_text)

    return cat_str

def move_item(source_name: str | Path, 
              destination_name: str | Path) -> Path | None: 
    """
    Moves a file or directory into a new file or directory.

    Parmeters
    ---------
    source_name : str/Path
        The current location of the file/directory.
    destination_name : str/Path 
        The target location or new name.

    Returns
    -------
    new_location : Path/None
        The path to the new location or None if move failed.
    """
    source_path = Path(source_name)
    destination_path = Path(destination_name)

    # 1. Verify the source exists
    if not source_path.exists():
        print(f"Error: The source '{source}' does not exist.")
        return False

    try:
        # 2. Perform the move
        # shutil.move accepts pathlib.Path objects directly in Python 3.6+
        new_location = shutil.move(source_path, destination_path)
        print(f"Successfully moved '{source}' to '{new_location}'")
        return Path(new_location)
    except PermissionError:
        print(f"Permission denied: You do not have the required permissions to move '{source_path}'.")
        return None
    except Exception as e:
        print(f"An error occurred while moving: {e}")
        return None

def list_all_paths(dir_name: str) -> list[Path]:
    """
    Lists both directories and files within
    a directory given its path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    Returns
    -------
    list[Path]
        The directories and files within a directory
        as a list of Path objects
    """
    dir_path = Path(dir_name)
    if dir_path.exists():
        return list(dir_path.iterdir())
    else:
        logger.error(f'The directory does not exist: {dir_name}')
        return []

def list_all(dir_name: str) -> list[str]:
    """
    Lists the Path objects of both directories and
    files within a directory given its path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    Returns
    -------
    list[str]
        The names of directories and files within a directory
    """
    paths = list_all_paths(dir_name)
    return [obj.name for obj in paths]

def list_subfile_paths(dir_name: str) -> list[Path]:
    """
    Lists files as Path objects within a directory given
    the directory's path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    Returns
    -------
    list[Path]
        The files within a directory as a list of Path objects
    """
    sub_all = list_all_paths(dir_name)
    return [obj for obj in sub_all if obj.is_file()]

def list_subfiles(dir_name: str) -> list[str]:
    """
    Lists files within a directory given its path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    Returns
    -------
    list[str]
        The files within a directory
    """
    return [obj.name for obj in list_subfile_paths(dir_name)]

def list_subfile_paths_with_ext(dir_name: str, ext: str) -> list[Path]:
    """
    Lists files wit extension within a directory as Path objects
    given the directory's path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    ext : str
        The desired file extension

    Returns
    -------
    list[Path]
        The files within a directory with the extension 'ext' as Path objects
    """
    return [obj for obj in list_subfile_paths(dir_name) if obj.suffix == ext]

def list_subfiles_with_ext(dir_name: str, ext: str) -> list[str]:
    """
    Lists files wit extension within a directory as Path objects
    given the directory's path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    ext : str
        The desired file extension

    Returns
    -------
    list[str]
        The files within a directory with the extension 'ext'
    """
    return [obj.name for obj in list_subfile_paths_with_ext(dir_name, ext)]

def list_subdir_paths(dir_name: str) -> list[Path]:
    """
    Lists  files within a directory given its path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    Returns
    -------
    list[Path]
        The directories within a directory as a list of Path objects
    """
    sub_all = list_all_paths(dir_name)
    return [obj for obj in sub_all if obj.is_dir()]

def list_subdirs(dir_name: str) -> list[str]:
    """
    Lists  files within a directory given its path name

    Parameters
    ----------
    dir_name : str
        The path name of a directory

    Returns
    -------
    list[str]
        The subdirectories within a directory
    """
    return [obj.name for obj in list_subdir_paths(dir_name)]

def print_dir_tree(dir_name: str) -> None:
    """
    Prints out the tree structure of a directory given its name.

    Parameters
    ----------
    dir_name : str
        The path name of a directory
    """
    dir_path = Path(dir_name)
    if not dir_path.exists():
        logger.error(f'The directory does not exist: {dir_name}')
    elif not dir_path.is_dir():
        logger.error(f'The path does not lead to a directory: {dir_name}')
    else:
        print(f"+ {dir_name}")
        for path in sorted(dir_path.rglob("*")):
            level = len(path.relative_to(dir_path).parts)
            space = "    " * level
            print(f"{space} + {path.name}")

def print_dir_tree_given_path(dir_path: Path) -> None:
    """
    Prints out the tree structure of a directory given as a Path object

    Parameters
    ----------
    dir_path : Path
        The path object of a directory
    """
    dir_name = dir_path.name
    if not dir_path.exists():
        logger.error(f'The directory does not exist: {dir_name}')
    elif not dir_path.is_dir():
        logger.error(f'The path does not lead to a directory: {dir_name}')
    else:
        print(f"+ {dir_name}")
        for path in sorted(dir_path.rglob("*")):
            level = len(path.relative_to(dir_path).parts)
            space = "    " * level
            print(f"{space} + {path.name}")

def find_files(src_dir: str, pattern: str) -> list[str]:
    """
    Find files under 'src_dir' matching 'pattern'.

    Parameters
    ---------
    src_dir : str
        The name of top directory for file search
    pattern : str
        The pattern to be used to look for files matching it.

    Returns
    -------
    list[str]
        List of matched files
    """
    return [obj.name for obj in list(Path(src_dir).glob(pattern))]

def recurs_find_files(src_dir: str, pattern: str) -> list[str]:
    """
    Find files recursively under 'src_dir' matching 'pattern'.

    Parameters
    ----------
    src_dir : str
        The name of top directory for file search
    pattern : str
        The pattern to be used to look for files matching it.

    Returns
    -------
    list[str]
        List of matched files
    """
    return [str(obj) for obj in list(Path(src_dir).rglob(pattern))]

def clean_dir(adir: str) -> None:
    """
    'Safe' way to clean the contents of a directory

    Parameters
    ----------
    adir : directory
    """

    if not check_dir_exists(adir):
        logger.error(f"The directory {adir} does not exist.")
        return

    if adir == '/' or adir == "\\":
        logger.error('Cannot clean %s', adir)
        return
    else:
        for file_object in os.listdir(adir):
            logger.info('Will clean up %s', adir)
            file_object_path = os.path.join(adir, file_object)
            if os.path.isfile(file_object_path):
                os.unlink(file_object_path)
            else:
                try:
                    shutil.rmtree(file_object_path)
                except OSError:
                    logger.error('Permission denied: cannot remove '
                                 + file_object_path, exc_info=True)
                    sys.exit()

def clean_scratch(scratch_dir: str) -> None:
    """
    Clean the directory where the run will take place.

    Parameters
    ----------
    scratch_dir : str
        Directory where all the work will be performed
    """

    if not Path(scratch_dir).exists():
        logger.warning(f'The folder {scratch_dir} does not exists!')
    else:
        clean_dir(scratch_dir)

def copy_file(src_file: str, dest_file: str, 
              comment: str = "") -> None:
    """
    Copies from src file to dest file.

    Parameters
    ----------
    src_file : str
        Source file
    dest_file : str
        Destination file
    comment : str
        An arbitrary text that will be printed if not empty.
    """
    if not check_file_exists(src_file): # or not check_file_exists(dest_file):
        logger.error('Source and destination files must both exist.')
    elif src_file == dest_file:
        logger.error('Source and destination files must be different')
    else:
        try:
            shutil.copy(src_file, dest_file)
            if comment:
                print(f"Creating {Color.RED}{comment}{Color.RESET}")
        except OSError as e:
            logger.error(f'Error: {e}', exc_info=True)

def rename_file(src: str, dst: str) -> None:
    """
    Rename the src file to dest file

    Parameters
    ----------
    src : str
        Source file
    dst : str
        Destination file
    """
    if not check_file_exists(src):
        logger.error('Source must exist.')
    else:
        try:
            f = Path(src)
            f.rename(dst)
        except OSError as e:
            logger.error(f'Error: {e}', exc_info=True)

def remove_file(fname: str) -> None:
    """
    Remove a file.

    Parameters
    ----------
    fname : str
        File name to delete
    """
    if check_file_exists(fname):
        try:
            file_to_remove = Path(fname)
            file_to_remove.unlink()
        except OSError as e:
            logger.error(f'Error: {e}', exc_info=True)
    else:
        logger.error(f"The file {fname} does not exist.")

def which(program: Any) -> Any:
    """
    Test if an executable program exists in the path - like unix's which

    Parameters
    ----------
    program : Any
        A program

    Returns
    -------
    Any
        Either the program, exe_file, or None
    """

    def is_exe(file_path):
        return os.path.isfile(file_path) and os.access(file_path, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file
    return None

def grep_file(file_name: str, pattern: str) -> bool:
    """
    Parameters
    ----------
    file_name : str
        The file name
    pattern : str
        The pattern to search for

    Returns
    -------
    bool
        True if pattern found in file, False otherwise.

    """
    with open(file_name, 'r') as file:
        for line in file:
            if pattern in line:
                return True
        return False

def create_symbolic_link(link_location: str, 
                         target_location: str) -> bool:
    """
    Creates a symbolic link at 'link_location' 
    that points to 'target_location'.

    Paramters:
    ---------
    link_location : str
    target_location : str
    """

    # Create Path objects
    link_path = Path(link_location)
    target_path = Path(target_location)
    
    # Optional: Check if the target actually exists
    if not target_path.exists():
        print(f"Warning: The target path '{target_path}' does not exist.")
        print(f"--> Creating a broken symlink.")

    # Check if a file or link already exists at the link location
    if link_path.exists() or link_path.is_symlink():
        print(f"Error: A file or symlink already exists at '{link_path}'.")
        return False
        
    try:
        # Create the symbolic link
        # The syntax is: link_path.symlink_to(target_path)
        link_path.symlink_to(target_path)
        print(f"Successfully created symlink: {link_path} -> {target_path}")
        return True
    except PermissionError:
        print("Permission denied: ")
        print("You may need administrator/root privileges to create symlinks.")
        return False
    except OSError as e:
        print(f"OS Error: {e}")
        return False

def are_dir_trees_equal(dir1_name: str, dir2_name) -> bool:
    """
    Compare two directories recursively. Files in each directory are
    assumed to be equal if their names and contents are equal.

    Parameters
    ----------
    dir1_name : str
        First directory path
    dir2_name : str
        Second directory path

    Returns
    -------
    bool
        True if the directory trees are the same and
        there were no errors while accessing the directories or files, 
        False otherwise.
    """
    dir1_path = Path(dir1_name)
    dir2_path = Path(dir2_name)

    # Ensure both paths exist and are directories
    if not dir1_path.is_dir():
        raise ValueError(f"'{dir1}' is not a valid directory.")
    if not dir2_path.is_dir():
        raise ValueError(f"'{dir2}' is not a valid directory.")

    dirs_cmp = filecmp.dircmp(dir1_path, dir2_path)
    if (len(dirs_cmp.left_only) > 0 or
            len(dirs_cmp.right_only) > 0 or
            len(dirs_cmp.funny_files) > 0):
        return False
    (_, mismatch, errors) = filecmp.cmpfiles(dir1_path, dir2_path,
                                             dirs_cmp.common_files, shallow=False)
    if len(mismatch) > 0 or len(errors) > 0:
        return False
    for common_dir in dirs_cmp.common_dirs:
        #new_dir1 = os.path.join(dir1, common_dir)
        #new_dir2 = os.path.join(dir2, common_dir)
        new_dir1 = dir1_path / common_dir
        new_dir2 = dir2_path / common_dir
        if not are_dir_trees_equal(new_dir1, new_dir2):
            return False
    return True

def compare_directory_trees(dir1_name: str, dir2_name: str) -> dict:
    """
    Recursively compares two directory trees using pathlib.
    Returns a dictionary categorizing the differences.

    Parameters
    ----------
    dir1_name : str
       Path to the first directory.
    dir2_name : str
       Path to the second directory

    Returns
    -------
    diff_dict : dict
       A dictionary containing the difference between the two directories.
    """
    dir1_path = Path(dir1_name)
    dir2_path = Path(dir2_name)

    # Ensure both paths exist and are directories
    if not dir1_path.is_dir():
        raise ValueError(f"'{dir1}' is not a valid directory.")
    if not dir2_path.is_dir():
        raise ValueError(f"'{dir2}' is not a valid directory.")

    # Get all items recursively and compute their relative paths
    # p.relative_to() allows us to compare the inner structure easily
    paths1 = {p.relative_to(dir1_path) for p in dir1_path.rglob('*')}
    paths2 = {p.relative_to(dir2_path) for p in dir2_path.rglob('*')}

    # Use set operations to find unique and common paths
    only_in_dir1 = paths1 - paths2
    only_in_dir2 = paths2 - paths1
    common_paths = paths1 & paths2

    matches = list()
    mismatches = list()

    # Compare items that exist in both directories
    for rel_path in common_paths:
        p1 = dir1_path / rel_path
        p2 = dir2_path / rel_path

        # If both are directories, they "match" structurally, no content to compare
        if p1.is_dir() and p2.is_dir():
            continue
        # If both are files, compare their contents
        elif p1.is_file() and p2.is_file():
            # shallow=False ensures it looks at file content, not just os.stat metadata
            if filecmp.cmp(p1, p2, shallow=False):
                matches.append(rel_path)
            else:
                mismatches.append(rel_path)
        # If one is a file and the other is a directory (type mismatch)
        else:
            mismatches.append(rel_path)

    diff_dict = {
            'only_in_dir1': sorted(only_in_dir1),
            'only_in_dir2': sorted(only_in_dir2),
            'matches': sorted(matches),
            'mismatches': sorted(mismatches)
            }

    return diff_dict


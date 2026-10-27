
import logging
import sys
import yaml
from pathlib import Path

from gcmpy.utils.logger_ops import logger_setup
from gcmpy.utils.path_ops import check_file_exists

from typing import Any

logger = logger_setup(filename=__name__,
                      file_handler=True,
                      file_level=logging.ERROR,
                      stream_handler=False)


def read_yaml_file(yaml_fname: str) -> dict:
    """
    Read <exp_name>.yaml file into a dictionary.

    Parameters
    ----------
    yaml_fname : str
        YAML file name
    Returns
    -------
    yaml_dict : dict
        Dictionary of the YAML file settings
    """
    logger.info(f'Reading of the file: {yaml_fname}')
    check_file_exists(yaml_fname)

    try:
        with open(yaml_fname, "r") as fid:
            yaml_dict = yaml.safe_load(fid)
    except FileNotFoundError:
        print(f"File {yaml_fname} not found!")
        sys.exit()
    except yaml.YAMLError as e:
        print(f"Error parsing YAML file {yaml_fname}:  {e}")
        sys.exit()
    else:
        logger.debug(f'Done reading {yaml_fname}')

    return yaml_dict

def read_yaml_file_full(yaml_fname: str) -> dict:
    """
    Read a YAML file using the FullLoader option and 
    pass the content into a dictionary.

    Parameters
    ----------
    yaml_fname : str
        YAML file name
    Returns
    -------
    yaml_dict : dict
        Dictionary of the YAML file settings
    """
    logger.info(f'Reading of the file: {yaml_fname}')
    check_file_exists(yaml_fname)

    try:
        with open(yaml_fname, "r") as fid:
            yaml_dict = yaml.load(fid, Loader=yaml.FullLoader)
    except FileNotFoundError:
        print(f"File {yaml_fname} not found!")
        sys.exit()
    except yaml.YAMLError as e:
        print(f"Error parsing YAML file {yaml_fname}:  {e}")
        sys.exit()
    else:
        logger.debug(f'Done reading {yaml_fname}')

    return yaml_dict

class _MultilineDumper(yaml.Dumper):
    """Keep multiline strings readable without changing global YAML defaults."""


def _represent_multiline_string(dumper, value):
    style = '|' if '\n' in value else None
    return dumper.represent_scalar('tag:yaml.org,2002:str', value, style=style)


_MultilineDumper.add_representer(str, _represent_multiline_string)


def write_dict_into_yaml(
        yaml_file_path: Path,
        my_dict: dict) -> None:
    """
    Write a dictionary into a YAML file.
    Multiline strings use literal blocks where possible, preserving their
    contents (including trailing newlines) when the YAML is read back.

    Parameters
    ----------
    yaml_file_path : Path
       The path to the YAML file we are creating.
    my_dict : dict
       The dictionary we want write into the YAML file.
    """

    with open(yaml_file_path, 'w') as fid:
        yaml.dump(my_dict, fid, Dumper=_MultilineDumper,
                  default_flow_style=False, sort_keys=False)

def write_yaml(file_content: dict, file_name: str, 
               file_mode: str='a', dfs: bool=False) -> str:
    """
    write dictionary to yaml file

    Parameters
    ----------
    file_content : dict
        contents of yaml file
    file_name : str
        yaml file path
    file_mode : str
        File mode
    dfs : bool
        indicator of whether to use default flow style

    Returns
    -------
    file_name : str
        yaml file path

    """
    with open(file_name, file_mode) as fid:
        yaml.dump(file_content, fid, default_flow_style=dfs)
    return file_name

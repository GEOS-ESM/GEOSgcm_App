
import logging
import sys
import yaml
import re
import isodate
from jinja2 import Template
from pathlib import Path

from gcmpy.utils.logger_ops import logger_setup
from gcmpy.utils.path_ops import check_file_exists

from typing import Any

logger = logger_setup(filename=__name__,
                      file_handler=True,
                      file_level=logging.ERROR,
                      stream_handler=False)

# Define a regex pattern that matches 
# ISO 8601 durations (starts with 'P')
# This looks for 'P' followed by numbers 
# and letters like Y, M, D, T, H, S
iso_duration_pattern = re.compile(r'^P(?=\d|T\d)[a-zA-Z0-9\.]+$')

def read_iso_yaml_file(yaml_fname: str) -> dict:
    """
    Read A YAML file into a dictionary.
    The file contains duration in the ISO format.
    Any variable with duration setting is set in the format:

       my_timer1: !duration PT5M       # 5 minutes
       my_timer2: !duration P5DT12H30M # 5 days, 12 hours, 30 minutes

    It is important to note that when the file is read,
    all the duration values will be in datetime/timedelta
    format.

    Parameters
    ----------
    yaml_fname : str
        YAML file name

    Returns
    -------
    yaml_dict : dict
        Dictionary of the YAML file settings
    """
    # Define the constructor function that tells PyYAML 
    # what to do with the string
    def iso_duration_constructor(loader, node):
        value = loader.construct_scalar(node)
        return isodate.parse_duration(value)

    # Add the resolver and constructor to the SafeLoader
    yaml.SafeLoader.add_implicit_resolver('!duration', iso_duration_pattern, None)
    yaml.SafeLoader.add_constructor('!duration', iso_duration_constructor)

    yaml_dict = read_yaml_file(yaml_fname)

    return yaml_dict

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

def read_templated_yaml(yaml_file: str, kwargs: dict=None) -> dict:
    """
    Reads a YAML file, renders it with Jinja2 using 
    the provided kwargs, and returns the parsed Python 
    dictionary.

    Parameters
    ----------
    yaml_fname : str
        YAML file name
    kwargs : dict

    Returns
    -------
    yaml_dict : dict
        Dictionary of the YAML file settings

    """
    # 1. Read the raw template file
    with open(yaml_file, 'r') as fid:
        raw_template = fid.read()

    # 2. Render the Jinja2 template with your variables
    template = Template(raw_template)
    if kwargs:
        rendered_yaml = template.render(**kwargs)
    else:
        rendered_yaml = template.render()

    # 3. Parse the rendered string as standard YAML
    yaml_dict = yaml.safe_load(rendered_yaml)

    return yaml_dict

def write_dict_into_yaml(
        yaml_file_path: Path,
        my_dict: dict) -> None:
    """
    Write a dictionary into a YAML file.
    We make sure that sections are separated by empty lines.

    Parameters
    ----------
    yaml_file_path : Path
       The path to the YAML file we are creating.
    my_dict : dict
       The dictionary we want write into the YAML file.
    """

    with open(yaml_file_path, 'w') as fid:
        yaml.dump(my_dict, fid, default_flow_style=False, sort_keys=False)

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


#!/usr/bin/env python3

import os
import sys
import argparse
from pathlib import Path

from gcmpy.utils.yaml_ops import read_yaml_file
from gcmpy.utils.color_ops import Color
from gcmpy.run_tools.scratchdir_ops import create_scratchdir
from gcmpy.run_tools.history_file_ops import (
        get_collection_list,
        get_monthly_collection_list,
        read_history_rc
        )
from gcmpy.utils.path_ops import (
        get_current_dir, 
        get_home_dir,
        create_dir,
        change_dir
        )
from gcmpy.batch_tools.node_ops import determine_nodes
from gcmpy.batch_tools.templates.read_batch_tmpl import create_batch_header

empty_line = "\n"

def get_number_nodes(exp_dict: dict) -> int:

    io_server = bool(exp_dict['USE_IOSERVER'])
    nx = exp_dict['NX']
    ny = exp_dict['NY']
    num_cpus = exp_dict['NCPUS_PER_NODE']

    EXPDIR = Path(exp_dict['EXPDIR'])
    history_rc_file = f"{EXPDIR}/HISTORY.rc"

    num_nodes, -, - = determine_nodes(io_server,
                                      nx, ny, history_rc_file)

    return num_nodes

def define_batch_header(exp_dict: dict) -> str:

    tmpl_data = dict(
            BATCH_TIME="12:00:00",
            NUM_NODES=get_number_nodes(exp_dict),
            NTASKS_PER_NODES=exp_dict['NCPUS_PER_NODE'],
            JOB_NAME=exp_dict['EXPID']
            NODE_TYPE=exp_dict['proc_type'],
            USER_ACCOUNT=exp_dict['USER_ACCOUNT'],
            OUTPUT_FILE="myfile.out",
            QUEUE_NAME="batch"
            )

    task_type = "run"
    batch_system = "slurm"

    batch_header = create_batch_header(task_type, batch_system, tmpl_data)

    return batch_header

def create_batch_file(exp_dict: dict) -> None:

    batch_file_name = f"{exp_dict['EXPDIR']}/gcm_run_{exp_dict['EXPID']}.j"
    batch_header = define_batch_header(exp_dict)
    first_line = " #!/bin/csh -f \n"
    with open(batch_file_name, "w") fid:
        fid.write(first_line)
        fid.write(empty_line)
        fid.write(batch_header)
        fid.write(empty_line)


def create_experiment_dirs(exp_dict: dict) -> None:
    """
    Create directories (if not already available)
    needed to run an experiment.

    Parameters
    ----------
    exp_dict : dict
       Contains parameters settings needed to
       initiate an experiment.
    """

    EXPDIR = Path(exp_dict['EXPDIR'])
    GEOSDIR = Path(exp_dict['GEOSDIR'])
    GEOSUTIL = GEOSDIR
    GEOSSRC = GEOSDIR
    GEOSUTIL = GEOSDIR
    GEOSBIN = GEOSDIR / "bin"
    GEOSETC = GEOSDIR / "etc"

    dir_list = ["restarts", "holding", "archive", "post", "plot"]
    for name in dir_list:
        create_dir(EXPDIR / name)

    history_rc_file = f"{EXPDIR}/HISTORY.rc"
    history_dict = read_history_rc(history_rc_file)
    hist_colls = get_collection_list(history_dict)
    print(f"History collections: \n {hist_colls}")
    monthly_hist_colls = get_monthly_collection_list(history_dict)
    print(f"History monthly collections: \n {monthly_hist_colls}")

    scratch_dir_name = "scratch"
    scratch_dir = create_scratchdir(EXPDIR, scratch_dir_name)
    change_dir(scratch_dir)

    print(f"Current directory: \n {get_current_dir()}")

    for colls in hist_colls:
        create_dir(EXPDIR / f"{colls}")
        create_dir(EXPDIR / f"holding/{colls}")

def main():
    # Parse command line flags
    parser = argparse.ArgumentParser()
    parser.add_argument("--yaml", "-y")
    args = parser.parse_args()

    parent_dir = get_current_dir()

    yaml_file = args.yaml
    exp_settings = read_yaml_file(yaml_file)

    # Create the batch run script
    create_batch_file(exp_settings)

    # Creation of directories
    create_experiment_dirs(exp_settings) 

if __name__ == "__main__":
    main()

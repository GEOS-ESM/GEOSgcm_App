#!/usr/bin/env python3

import os
import sys
import argparse
from pathlib import Path

from gcmpy.utils.yaml_ops import read_yaml_file
from gcmpy.utils.color_ops import Color
from gcmpy.utils.path_ops import (
        get_current_dir, 
        get_home_dir
        )
from gcmpy.batch_tools.node_ops import determine_nodes
from gcmpy.batch_tools.templates.read_batch_tmpl import (
        create_batch_header,
        create_batch_content
        )


def get_number_nodes(exp_dict: dict) -> int:

    io_server = bool(exp_dict['USE_IOSERVER'])
    nx = exp_dict['NX']
    ny = exp_dict['NY']
    cpus_per_node = exp_dict['NCPUS_PER_NODE']

    EXPDIR = Path(exp_dict['EXPDIR'])
    history_rc_file = f"{EXPDIR}/HISTORY.rc"

    num_nodes, a, b = determine_nodes(io_server,
                                      nx, ny, cpus_per_node, 
                                      history_rc_file)

    return num_nodes

def define_batch_header(exp_dict: dict) -> str:

    data_tmpl = dict(
            BATCH_TIME="12:00:00",
            NUM_NODES=get_number_nodes(exp_dict),
            NTASKS_PER_NODES=exp_dict['NCPUS_PER_NODE'],
            JOB_NAME=exp_dict['EXPID'],
            NODE_TYPE=exp_dict['proc_type'],
            USER_ACCOUNT=exp_dict['USER_ACCOUNT'],
            OUTPUT_FILE="myfile.out",
            QUEUE_NAME="batch"
            )

    task_type = "run"
    batch_system = "slurm"

    batch_header = create_batch_header(task_type, batch_system, data_tmpl)

    return batch_header

def create_batch_file(config_yaml_file: str) -> None:

    exp_dict = read_yaml_file(config_yaml_file)

    batch_file_name = f"{exp_dict['EXPDIR']}/gcm_run_{exp_dict['EXPID']}.j"
    batch_header = define_batch_header(exp_dict)

    data_tmpl = dict(
            SITE=exp_dict['SITE'],
            GEOSDIR=exp_dict['GEOSDIR'],
            BATCH_HEADER=batch_header,
            CONFIG_YAML_FILE=config_yaml_file,
            )

    task_type = "run"
    batch_content = create_batch_content(task_type, data_tmpl)

    with open(batch_file_name, "w") as fid:
        fid.write(batch_content)

def main():
    # Parse command line flags
    parser = argparse.ArgumentParser()
    parser.add_argument("--yaml", "-y")
    args = parser.parse_args()

    #parent_dir = get_current_dir()

    yaml_file = args.yaml

    # Create the batch run script
    create_batch_file(yaml_file)


if __name__ == "__main__":
    main()

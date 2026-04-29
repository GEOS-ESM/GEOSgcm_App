#!/usr/bin/env python3

import os
import sys
import argparse
from jinja2 import Environment, FileSystemLoader, Undefined

from gcmpy.setup_tools.geos_ops import copy_src_tarfile
from gcmpy.setup_tools.setup_main import SetupGCM
from gcmpy.setup_tools.setup_envs import set_env_dicts
from gcmpy.setup_tools.clone import yaml_receipt, yaml_input_exp, clone_exp
from gcmpy.setup_tools.process_questions import ask_questions
from gcmpy.utils.yaml_ops import read_yaml_file
from gcmpy.utils.path_ops import get_current_dir, get_home_dir
from gcmpy.utils.color_ops import Color

def main():
    # Parse command line flags
    parser = argparse.ArgumentParser()
    parser.add_argument("--yaml", "-y")
    args = parser.parse_args()

    parent_dir = get_current_dir()
    set_env_dicts(parent_dir)

    home_dir = get_home_dir()

    if args.yaml:
        expConfig = read_yaml_file(args.yaml)
        expConfig['clone_experiment'] = None
    else:
        expConfig = ask_questions()

    # if cloning we don't care about 90% of this script
    if expConfig['clone_experiment']:
        clone_exp(expConfig)
        copy_src_tarfile(expConfig['exp_dir'])
        print("Successfully created a clone!")
        exit()

    experiment = SetupGCM(expConfig)
    experiment.initialize_models()
    experiment.set_num_CPUs()
    experiment.config_models()
    experiment.set_atmos_params()
    experiment.set_nodes()
    experiment.set_batch_params()
    experiment.create_dotfile(f"{home_dir}/.EXPDIRroot", os.path.dirname(expConfig['exp_dir']))
    experiment.create_dotfile(f"{home_dir}/.GROUProot", expConfig['group_root'])
    experiment.RC_setup()
    yaml_receipt(expConfig)
    experiment.mpistacksettings()
    experiment.copy_files_into_exp()
    experiment.restarts()
    experiment.mod_RC_dir_for_pchem()
    experiment.config_chemGridComp()
    experiment.config_surfaceGridComp()
    experiment.config_gocartGridComp()
    experiment.config_heartbeat()
    experiment.template()
    experiment.organize_exp_dir()

if __name__ == "__main__":
    main()

#!/usr/bin/env python3

import os
import argparse

from gcmpy.setup_tools.setup_envs import set_env_dicts
from gcmpy.setup_tools.process_questions import ask_questions
from gcmpy.utils.yaml_ops import write_yaml
from gcmpy.utils.path_ops import get_current_dir

def main():
    parser = argparse.ArgumentParser(
        description="Ask configuration questions and write a YAML driver file."
    )
    parser.add_argument(
        "--output", "-o",
        default=None,
        help="Directory in which to write the output YAML file. "
             "Defaults to the current working directory."
    )
    args = parser.parse_args()

    parent_dir = get_current_dir()
    set_env_dicts(parent_dir)

    expConfig = ask_questions()

    output_dir = args.output if args.output else os.getcwd()
    yaml_path = os.path.join(output_dir, f"{expConfig['experiment_id']}.yaml")

    write_yaml(expConfig, yaml_path, file_mode='w')
    print(f"Configuration written to: {yaml_path}")

if __name__ == "__main__":
    main()

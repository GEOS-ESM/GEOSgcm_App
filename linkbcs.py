#!/usr/bin/env python3

import argparse
import logging
import shutil
import sys
from datetime import datetime
from pathlib import Path

import yaml

def validate_yaml_file(file_path):
    path = Path(file_path)

    # Check if file exists
    if not path.exists():
        raise argparse.ArgumentTypeError(f"File does not exist: {file_path}")

    # Check file extension
    if path.suffix.lower() not in ['.yaml', '.yml']:
        raise argparse.ArgumentTypeError(f"File must have .yaml or .yml extension: {file_path}")

    return file_path

def capture_arguments():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--manifest",
        required=True,
        type=validate_yaml_file,
        help="User-provided YAML manifest file"
    )
    parser.add_argument(
        "--timestamp",
        required=True,
        type=validate_iso_datetime,
        help="ISO format date"
    )

    args = parser.parse_args()
    return args

def setup_logger(timestamp: datetime) -> logging.Logger:
    timestamp_str = timestamp.strftime("%Y-%m-%dT%H:%M:%S")
    log_file = Path.cwd() / f"{timestamp_str}_bcs.log"

    logger = logging.getLogger("linkbcs")
    logger.setLevel(logging.INFO)

    handler = logging.FileHandler(log_file)
    handler.setFormatter(logging.Formatter("[%(asctime)s] %(levelname)s: %(message)s"))
    logger.addHandler(handler)

    return logger

def validate_iso_datetime(datetime_string):
    try:
        date = datetime.fromisoformat(datetime_string)
        return date
    except ValueError:
        raise argparse.ArgumentTypeError(
            f"Invalid datetime format: '{datetime_string}'. "
            f"Expected ISO format (YYYY-MM-DDTHH:MM:SS), e.g., '2025-04-04T00:00:00'"
        )

class LinkApplier:
    def __init__(self, logger: logging.Logger):
        self.logger = logger

    def create_symlink(self, symlink_name: Path, file_path: Path):
        if file_path.exists():
            parent = symlink_name.parent
            if parent != Path(".") and not parent.exists():
                parent.mkdir(parents=True)
                self.logger.info(f"DIRECTORY CREATED: {parent.resolve()}")

            # remove existing link if it exists (equivalent of -f flag)
            if symlink_name.is_symlink():
                symlink_name.unlink()

            symlink_name.symlink_to(file_path)
            self.logger.info(f"SYMLINK: {symlink_name} -> {file_path}")
        else:
            self.logger.warning(f"SKIPPED SYMLINK: {symlink_name} -> {file_path} (source does not exist)")

    def copy_file(self, target_name: Path, file_path: Path):
        if file_path.exists():
            parent = target_name.parent
            if parent != Path(".") and not parent.exists():
                parent.mkdir(parents=True)
                self.logger.info(f"DIRECTORY CREATED: {parent.resolve()}")

            # copy2 preserves file metadata
            shutil.copy2(file_path, target_name)
            self.logger.info(f"FILE COPIED: {file_path} -> {target_name}")
        else:
            self.logger.warning(f"SKIPPED FILE COPY: {target_name} -> {file_path} (source does not exist)")

    # returns broken paths and prints warnings for each broken path and exit
    def validate_paths(self, paths):
        missing_files = []
        for i in paths:
            if not paths[i].exists():
                missing_files.append(i)
                # We print an error if the missing file is *not* tile.bin
                if i != "tile.bin":
                    self.logger.error(f"{i} does not exist at: {paths[i]}")
                    print(f"ERROR: {i} does not exist at: \n{paths[i]}")
                else:
                    self.logger.warning(f"{i} does not exist at: {paths[i]}")
                    print(f"WARNING: {i} does not exist at: \n{paths[i]}")

        if missing_files:
            # We must allow for the one case that is only tile.bin is in missing_files,
            # in which case gcm_run.j will make tile.bin from tile.data
            if len(missing_files) == 1 and missing_files[0] == "tile.bin":
                self.logger.warning("tile.bin is missing, but tile.data exists. The model can still run with tile.data.")
                print("WARNING: tile.bin is missing, but tile.data exists. The model can still run with tile.data.")
            else:
                self.logger.error("One or more paths are broken. Please check the warnings above.")
                print("One or more paths are broken. Please check the warnings above.")
                sys.exit(1)

def main():
    args = capture_arguments()
    logger = setup_logger(args.timestamp)

    with open(args.manifest, "r") as f:
        manifest = yaml.safe_load(f)

    if not manifest or "symlinks" not in manifest or manifest["symlinks"] is None:
        print("ERROR: manifest missing 'symlinks' section")
        sys.exit(1)

    year = args.timestamp.year

    def resolve_paths(entries):
        return {
            name: Path(str(source).replace("{{ year }}", str(year)))
            for name, source in entries.items()
        }

    symlink_paths = resolve_paths(manifest["symlinks"])
    copy_paths = resolve_paths(manifest.get("copy_files") or {})

    applier = LinkApplier(logger)
    applier.validate_paths(symlink_paths)
    applier.validate_paths(copy_paths)

    for name, source in symlink_paths.items():
        applier.create_symlink(Path(name), source)

    for name, source in copy_paths.items():
        applier.copy_file(Path(name), source)

if __name__ == "__main__":
    main()

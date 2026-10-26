# `gcmpy`: A Python tool for GEOS related operations

## Description

 `gcmpy`is a Python tool performing GEOS related operations.
It is meant to provide a flexible and robust way to automate
processes such as the creation of experiment directory, 
the submittion of SLURM job, editing experiment related files,
etc.

It consists of a set of interfaces that are accessible to
users to perform specific tasks:

- `run_gcm_setup.py`: Create and experiment directory either
   from scratch or from a YAML configuration file.
- __More to come...__

Underneath, the tool implements a collection of classes
(handling various tasks) that are supported by low level
utility functions.

## Features

- Test case management
- Configuration using YAML files
- Compatibility with Git repositories
- Event tracking mechanism
- Easy integration of new features

## Tests

From the `gcmpy` directory, using Python 3.13:

```sh
python -m pip install -r requirements-test.txt
python -m pytest gcmpy_tests -q
```

The `Python Tests` GitHub Actions workflow runs the full suite on Linux for
every pull request, without building the model. It can also be run manually.
Some tests exercise Linux shell scripts and the `file` command; macOS results
may differ.

## Authors

- GEOS SI-Team

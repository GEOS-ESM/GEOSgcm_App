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

## Stop after a test run

To stop immediately after the first successful model segment, add this at the
top level of the experiment's `run_<expid>.yaml`:

```yaml
stop_after_run: true
```

This skips checkpoint archiving, output moves, post-processing, restart
copy-back, and job resubmission. Model outputs remain in scratch; preserve
them before rerunning the experiment. Model failures still fail the job.
Omitting the setting or using `false` keeps the normal workflow. Use unquoted
YAML booleans, not strings.

The flag does not change the model run duration: configure `CAP.rc` for your
desired test duration (for example, one day with one segment).

## Tests

From the `gcmpy` directory, using Python 3.13:

```sh
python -m pip install -r requirements-test.txt
python -m pytest gcmpy_tests -q
```

The `Python Tests` GitHub Actions workflow runs the full suite on Linux and
macOS for every pull request, without building the model. It can also be run
manually. Both jobs run independently so a failure on one OS does not cancel
the other.

## Authors

- GEOS SI-Team

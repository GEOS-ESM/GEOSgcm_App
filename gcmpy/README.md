# `gcmpy`: A Python tool for GEOS related operations

## Description

 `gcmpy`is a Python tool performing GEOS related operations.
It is meant to provide a flexible and robust way to automate
processes such as the creation of experiment directory, 
the submission of Slurm/PBS jobs, editing experiment related files,
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

## Run-job allocation

`gcm_run.py` is the user entry point. Inside an existing Slurm allocation, run:

```bash
python3 ./gcm_run.py --config run_<expid>.yaml
```

It can also be launched directly with `sbatch` (supply the desired scheduler
resource flags before the script name):

```bash
sbatch [resource flags] ./gcm_run.py --config run_<expid>.yaml
```

Both paths automatically initialize the runtime environment, including
`UDUNITS2_XML_PATH`, through the internal `gcm_run.sh` wrapper. Users do not
need to source `g5_modules.sh` or invoke the wrapper themselves.

`gcm_run.py` uses `sbatch` at NCCS and `qsub` at NAS, as selected by the
experiment YAML's `batch_cmd`. Before each submission or resubmission it reads
the current decomposition from `AGCM.rc` and recalculates the node count:
`ceil(NX * NY / tasks_per_node)`, plus `IOSERVER_NODES` when IOSERVER is enabled.
Coupled runs use `OGCM.NX`/`OGCM.NY`, matching the model-launch resource logic.

For example, changing 4×24 to 10×60 requests five model nodes at 120 tasks per
node instead of one. Reducing the layout also reduces the request. The saved
`atmos.nx/ny` and node count in `run_p` do not override the current layout.
Tasks per node and other resource qualifiers remain configured in `run_p`
(Slurm `--ntasks-per-node`, PBS `mpiprocs`/`ncpus`); NAS processor models and
other select qualifiers are preserved. PBS supports the generated single,
homogeneous `select` request, not heterogeneous `+`-separated chunks.

This does not resize an already-running job. Change the layout before
submission; an insufficient existing allocation still fails the runtime check.

## Print the model-launch environment

To print the environment passed to the MPI launcher, add this at the top
level of the experiment's `run_<expid>.yaml`:

```yaml
print_run_environment: true
```

Before each launch attempt, the log includes a marked, alphabetically sorted
`NAME=value` block, including module setup, `UDUNITS2_XML_PATH`, and any ocean
preloads. This is the environment passed to the launcher, not a dump from
each remote MPI rank. Omitting the setting or using `false` disables it.
New experiments include `print_run_environment: false` explicitly.
Use unquoted YAML booleans.

Values with credential-like names (passwords, tokens, secrets, and keys) are
redacted in the log only; the actual launch environment is unchanged.
Redaction is name-based and cannot catch every possible secret, so review
the log before sharing it. Embedded line breaks are escaped as `\n`/`\r`;
other control characters use `\xNN` notation so terminal color settings
and other escape sequences are displayed rather than executed.

## Stop after a test run

To stop immediately after the first successful model segment, add this at the
top level of the experiment's `run_<expid>.yaml`:

```yaml
stop_after_run: true
skip_initial_restart_archive: true
```

This skips checkpoint archiving, output moves, post-processing, restart
copy-back, and job resubmission. Model outputs remain in scratch; preserve
them before rerunning the experiment. Model failures still fail the job.
Omitting the setting or using `false` keeps the normal workflow. Use unquoted
YAML booleans, not strings.

`skip_initial_restart_archive` independently skips the pre-launch archive of
the input restarts. Omitting it or using `false` preserves that archive.
Restart staging and binary-reader configuration still run. This flag alone
does not skip post-model checkpoint archiving; use both flags above for a
test that leaves outputs in scratch without either restart archive.

Neither flag changes the model run duration: configure `CAP.rc` for your
desired test duration (for example, one day with one segment).

## Restart staging

Generated run YAML includes `stage_restarts_in_scratch: copy`. Older YAML
without this key also copies restart inputs into scratch. Set it to `link`
to use symlinks instead, including monthly checkpoints, face restarts,
WaveWatch restarts, and coupled `INPUT/` files. Other values fail before
restart staging or archiving.

Link mode assumes the model reads, but does not modify, its input restarts:
writes through these links would change the originals. It does not change
boundary-condition staging, archiving, or restart copy-back. Coupled outputs
replace the initial `INPUT/` links with copies for subsequent segments.

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

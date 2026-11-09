# gcmpy Documentation

`gcmpy` is the Python replacement for the csh `gcm_setup` / `gcm_run.j`
workflow. It creates GEOS GCM experiment directories and runs the model under
Slurm (NCCS) or PBS (NAS).

These pages are the only copy of the gcmpy documentation. The
[GEOSgcm_App wiki](https://github.com/GEOS-ESM/GEOSgcm_App/wiki) just links
here. Update these pages in the same pull request as the code change (see
[`AGENTS.md`](../../AGENTS.md)).

## For users

1. [Quick Start](Quick-Start.md): build, create an experiment, run it.
2. [Creating an Experiment](Creating-an-Experiment.md): the setup questions,
   YAML-driven setup, cloning, and what the experiment directory contains.
3. [Running the Model](Running-the-Model.md): submission, node sizing,
   segments, resubmission, and test runs.
4. [Run YAML Reference](Run-YAML-Reference.md): every user-editable setting in
   `run_<expid>.yaml`.
5. [Troubleshooting](Troubleshooting.md).

## For developers

- [Developer Guide](Developer-Guide.md): code layout, setup and run
  pipelines, catalogs, configuration files, and tests.
- [New Features (PR #935)](New-Features-PR-935.md): what changed in PR #935
  and how to migrate existing experiments.

## Further reading

- [`gcmpy/README.md`](../../gcmpy/README.md): a short reference for the run-time
  settings.
- The [Developer Guide](Developer-Guide.md#run-pipeline) maps each
  `run_tools` module to the part of `gcm_run.j` it replaces.

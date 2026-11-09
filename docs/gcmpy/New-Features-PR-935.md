# New Features in PR #935

[GEOS-ESM/GEOSgcm_App#935](https://github.com/GEOS-ESM/GEOSgcm_App/pull/935)
was merged into `feature/gcm_scripts`. It contains 16 commits touching 39
files (+2,165 / −579). This page explains each change, the files involved,
and what you need to do (if anything).

## Summary

| Feature | User action needed? |
| --- | --- |
| [Smaller, focused run YAML](#1-focused-run-yaml-from-a-field-catalog) | No |
| [Per-node MPI rank override (`cpus_per_node`)](#2-per-node-mpi-rank-override) | Optional |
| [Node count recalculated from `AGCM.rc` (Slurm and PBS)](#3-node-count-recalculated-from-agcmrc) | No |
| [Structured `mpi_environment`](#4-structured-mpi-environment-settings) | Optional (migrate old blocks) |
| [Restart staging by copy or link, and skipping the initial archive](#5-restart-staging-modes-and-initial-archive-skip) | Optional |
| [`stop_after_run` for test runs](#6-stop-after-the-first-segment) | Optional |
| [`gcm_run.py` sets up its own environment; `print_run_environment`](#7-runtime-initialization-and-environment-diagnostics) | No |
| [Setup no longer writes `cap_restart`](#8-cap_restart-is-now-provided-by-the-user) | **Yes** |
| [Timestep, heartbeat and IOSERVER defaults moved to YAML; coupled C180 fixed](#9-resolution-defaults-moved-to-the-atmosphere-catalog) | No |
| [NCCS Cascade Lake removed](#10-nccs-cascade-lake-removed) | Yes, if you used `cas` at NCCS |
| [`gcm_run.sh` moved; macOS stack limit](#11-gcm_runsh-moved-into-gcmpy-macos-stack-limit) | No |
| [Setup prints the experiment location](#12-setup-reports-where-the-experiment-is) | No |
| [CI: pytest on Linux and macOS](#13-ci-pytest-on-linux-and-macos) | No |

---

## 1. Focused run YAML from a field catalog

*Commit `222e413`. Files: `setup_tools/setup_main.py` (`template()`),
`setup_tools/config/run_yaml_fields.yaml`.*

Before this change, `run_<expid>.yaml` was a dump of the entire Jinja
template context. Now `template()` loads `run_yaml_fields.yaml`, which has
two lists:

- `top_level`: keys copied as they are, when present;
- `nested`: for `expConfig`, `atmos`, `ocean`, `land` and `gocart`, only the
  listed sub-keys are copied.

The catalog is checked strictly. It must have exactly the keys `top_level`
and `nested`, the lists must be non-empty lists of non-empty strings, and no
key may appear in both. Anything else raises `ValueError`. The full context
is still used to render the templates.

**Developers:** if the runner (or a tool that reads the run YAML) needs a
new field, **add it to `run_yaml_fields.yaml`**. Otherwise it will be
missing from new experiments.

## 2. Per-node MPI rank override

*Commit `0cc6327`. Files: `gcm_run.py` (`_cpus_per_node_override`,
`build_sbatch_args`, `build_qsub_args`).*

Setup writes `cpus_per_node: null`. If it is set to a positive integer, it
replaces Slurm `--ntasks-per-node`, or PBS `ncpus` and `mpiprocs`, in every
runner-managed submission. Other PBS qualifiers such as `model=` are kept.
This controls MPI ranks per node, not OpenMP threads.

## 3. Node count recalculated from `AGCM.rc`

*Commit `b2f524a`. Files: `gcm_run.py` (`submission_nodes`).*

At every submission and resubmission, the node count is recalculated from
the **current** `AGCM.rc`:
`ceil(NX*NY / per_node) + IOSERVER_NODES` (if IOSERVER is on). Coupled runs
use `OGCM.NX`/`OGCM.NY`. If `run_p` includes `--ntasks`, it is recalculated
too. PBS accepts only one homogeneous `select=` chunk. A missing `AGCM.rc`,
a non-positive layout, or negative `IOSERVER_NODES` raises an error. You no
longer need to edit `run_p` after changing the layout.

## 4. Structured MPI environment settings

*Commit `a1f6eca`. Files: `setup_tools/config/mpi_config.yaml`,
`setup_tools/setup_main.py` (`mpistacksettings`, `template`),
`run_tools/mpi_env.py`, `utils/yaml_ops.py`.*

- `mpi_config.yaml` is now plain YAML mappings for each stack (`openmpi`,
  `mvapich`, `mpich`, `mpt`, `intelmpi`, `intelmpi_NASA`) instead of csh
  text.
- Setup builds `self.mpi_environment` from it. Intel MPI at NCCS or NAS adds
  `intelmpi_NASA`; coupled MPT runs add `MPI_SHEPHERD: "true"`. The result is
  written to `run_<expid>.yaml` as `mpi_environment`.
- `apply_mpi_environment()` checks the whole mapping first, then applies the
  legacy `mpi_config` and `ocean.mpt_shepherd` blocks, and finally applies
  the mapping on top. `null` unsets a variable.
- The legacy block parser now accepts `setenv VAR` with no value (empty
  value) and `unsetenv VAR`. It still does no shell expansion.
- The csh templates (post, archive, forecast) still receive an `mpi_config`
  string. It is now *generated* from the same mapping, using
  `shlex.quote`, so the defaults live in one place.
- `write_dict_into_yaml()` writes multi-line strings as literal `|` blocks,
  so legacy text stays readable and round-trips exactly.

See [Run YAML Reference › mpi_environment](Run-YAML-Reference.md#mpi_environment).

## 5. Restart staging modes and initial archive skip

*Commit `376bcb4`. Files: `run_tools/stage_restarts.py`,
`run_tools/archiver.py`, `gcm_run.py`.*

- `stage_restarts_in_scratch: copy | link` controls how `fetch_restarts()`
  stages restarts, checkpoints, `restart.ww3` and coupled `INPUT/` files. Any
  other value raises `ValueError`.
- `_stage_restart_file()` always removes an existing symlink before copying,
  so a copy never writes through a link left by an earlier run.
- `archive_initial_restarts()` resolves links, so the tarball holds the real
  data.
- `archive_checkpoints()` removes the `INPUT/` links before copying in new
  restarts, so later segments never write into your original files.
- `skip_initial_restart_archive: true` skips only the archive made before
  the launch.

## 6. Stop after the first segment

*Commit `f2d9390`. Files: `gcm_run.py` (`run_one_segment`).*

`stop_after_run: true` calls `sys.exit(0)` right after the model run and
OpenMP teardown, leaving output in scratch. Use it together with
`skip_initial_restart_archive` for fast test runs.

## 7. Runtime initialization and environment diagnostics

*Commit `1bf8e9c`. Files: `gcm_run.py` (`ensure_runtime_environment`),
`gcm_run.sh`, `run_tools/mpi_runner.py` (`_print_run_environment`).*

- Inside an allocation, `gcm_run.py` re-runs itself through
  `$GEOSBIN/gcm_run.sh` unless `GEOS_RUN_ENV_READY` equals `GEOSBIN`. This
  means `sbatch ./gcm_run.py ...` and running it interactively both get the
  module environment, library paths, stack limit and `UDUNITS2_XML_PATH`.
- PBS submissions pass the config through `-v GEOS_RUN_CONFIG=...`, because
  `qsub` does not accept script arguments. Paths containing commas are
  rejected.
- `print_run_environment: true` logs the launch environment, with
  credential-like names redacted and control characters escaped.

## 8. `cap_restart` is now provided by the user

*Commit `b62b8eb`. Files: `setup_tools/setup_main.py`.*

Setup used to write `cap_restart` from the begin date. It no longer does.
**You must create `cap_restart` yourself**, along with copying the
restarts, before the first run.

## 9. Resolution defaults moved to the atmosphere catalog

*Commits `f740442`, `f5b0dad`. Files: `setup_tools/catalogs/atmosphere_catalog.yaml`,
`setup_tools/atmosphere.py`, `setup_tools/process_questions.py`,
`geos_settings/horizontal_resolution.py`.*

- The hard-coded `heartbeat_dict` and `atmos_hres_settings` (about 300 lines)
  were removed from `horizontal_resolution.py`. The list of resolutions is
  now read from the catalog.
- Each resolution in the catalog has `heartbeat` and `io_server` defaults.
  `get_heartbeat_default()` uses the microphysics override first
  (`BACM_1M: 450`), then the resolution's value. `get_io_server_default()`
  supplies the IOSERVER prompt default (off for c12–c90, on for c180 and
  above).
- **Coupled C180 fix:** the heartbeat, `conv_dt` and `chem_dt` now depend on
  the ocean (MOM6/MIT 450/450/900, data ocean 600/600/1800), and `dt_ocean` is
  3600 for data-ocean runs.
- For coupled runs, `atmos['dt_ocean']` is now set to the actual heartbeat
  (including a value you typed in), and `config_heartbeat()` writes that
  same value into the CICE6 and MOM6 inputs.

## 10. NCCS Cascade Lake removed

*Commit `db1f118`. Files: `batch_tools/computing_sites.py`,
`setup_tools/setup_main.py`.*

`site_nodes_dict['NCCS']` now contains only `mil` (120). Any other value
gives an "unknown processor" error. The NAS options are unchanged.

## 11. `gcm_run.sh` moved into `gcmpy/`; macOS stack limit

*Commits `53ad0b1`, `6ff9068`. Files: `gcmpy/gcm_run.sh`,
`gcmpy/CMakeLists.txt`, `CMakeLists.txt`.*

The wrapper's source moved from the repository root to `gcmpy/gcm_run.sh`,
but it is still installed to `$INSTALL/bin/gcm_run.sh`. On macOS, the
wrapper raises the soft stack limit to the hard limit, instead of trying to
set it to `unlimited` (which fails there).

## 12. Setup reports where the experiment is

*Commit `53ad0b1`. Files: `gcm_setup.py`.*

A successful setup (including a clone) now ends by printing
`Experiment can be found in <exp_dir>`.

## 13. CI: pytest on Linux and macOS

*Commits `2ad38c8`, `346a91e`, `955816c`. Files:
`.github/workflows/python-tests.yml`, `gcmpy/requirements-test.txt`.*

The **Python Tests** workflow runs `python -m pytest gcmpy_tests -q` from
`gcmpy/` with Python 3.13 on `ubuntu-slim` and `macos-latest`, for every
pull request and on demand. The model is not built. The two jobs are
independent (`fail-fast: false`). See the
[Developer Guide](Developer-Guide.md#tests).

---

## Migrating an existing experiment

Nothing is required: old run YAML files keep working. To use the new
options, add them at the top level of `run_<expid>.yaml`:

```yaml
cpus_per_node: null
stage_restarts_in_scratch: copy
print_run_environment: false
# stop_after_run: true
# skip_initial_restart_archive: true
mpi_environment: {}
```

To move from the legacy MPI blocks to `mpi_environment`, convert each
`setenv NAME value` line to `NAME: "value"`, each `setenv NAME` to
`NAME: ""`, and each `unsetenv NAME` to `NAME: null`. Then **delete**
`mpi_config` and `ocean.mpt_shepherd`. If you leave them in place, they are
still applied before the mapping.

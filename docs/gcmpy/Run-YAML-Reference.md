# `run_<expid>.yaml` Reference

`run_<expid>.yaml` is the configuration read by `gcm_run.py`. Setup creates
it with only the fields listed in
`gcmpy/setup_tools/config/run_yaml_fields.yaml`. Everything else from setup
goes into the generated RC files and scripts, or into the setup receipt
`<expid>.yaml`.

Changes take effect the next time `gcm_run.py` runs (at the next
submission, or at the start of the next job). There is no need to run setup
again.

## Settings you are expected to edit

All of these are top-level keys.

### `cpus_per_node`

| | |
| --- | --- |
| Type | positive integer, or `null` |
| Generated value | `null` |
| Effect | The number of MPI ranks per node used to size submissions. `null` keeps the value from `run_p`. |

See [Running the Model › How many nodes are requested](Running-the-Model.md#how-many-nodes-are-requested).

### `mpi_environment`

| | |
| --- | --- |
| Type | a mapping from variable name to a string or `null` |
| Generated value | the defaults for your MPI stack and site (from `config/mpi_config.yaml`) |
| Effect | Environment variables set (or removed) just before `bundleParser.py` and the model launch, in every segment |

```yaml
mpi_environment:
  I_MPI_ADJUST_ALLREDUCE: "12"
  I_MPI_FABRICS: "shm:ofi"
  MPI_MEMMAP_OFF: ""        # set to an empty value
  PMI_RANK: null            # remove an inherited variable
  # I_MPI_DEBUG: "9"        # uncomment to enable
```

Rules:

- Values must be **strings**. Quote numbers and words like `"true"` and
  `"yes"`; an unquoted `12` or `true` is rejected.
- `""` sets an empty value. `null` unsets the variable. Deleting the entry
  leaves the inherited value alone. Use `mpi_environment: {}` for no
  overrides.
- Values are used exactly as written: no `$VAR` expansion, no shell quoting,
  no `setenv`/`export`.
- Names must match `[A-Za-z_][A-Za-z0-9_]*`. The whole mapping is checked
  before anything is changed.
- These are environment variables, not `mpirun` flags.

**Older experiments** that have `mpi_config` (a csh `setenv`/`unsetenv`
text block) and/or `ocean.mpt_shepherd` are still supported. Those blocks
are applied first, then `mpi_environment` overrides them. An empty mapping
does **not** turn the old blocks off; delete them when you migrate.

### `stage_restarts_in_scratch`

| | |
| --- | --- |
| Type | `copy` or `link` |
| Generated value | `copy` (and a missing key also means `copy`) |
| Effect | How the input restarts, monthly checkpoints, face restarts, `restart.ww3` and coupled `INPUT/` files are put into `scratch/` |

`link` uses symlinks, which saves time and space for large restarts. It is
only safe if the model never writes to its input restarts, because writing
through a link changes the original file. It does not affect boundary
condition staging, archiving, or copying restarts back. In coupled runs,
the `INPUT/` links are replaced with real copies before later segments. Any
other value fails before staging starts.

### `skip_initial_restart_archive`

| | |
| --- | --- |
| Type | boolean (unquoted) |
| Default | `false` (not written by setup) |
| Effect | Skips the archive of the input restarts made before the launch. Staging and the binary-reader check still run. |

### `stop_after_run`

| | |
| --- | --- |
| Type | boolean (unquoted) |
| Default | `false` (not written by setup) |
| Effect | Exits successfully right after the first successful model segment. Skips checkpoint archiving, moving output, post-processing, copying restarts back and resubmitting. Output stays in `scratch/`. |

It does not change the run length; set that in `CAP.rc`. A model failure
still fails the job.

### `print_run_environment`

| | |
| --- | --- |
| Type | boolean (unquoted) |
| Generated value | `false` |
| Effect | Logs the launcher environment before each launch attempt, with credential-like names redacted. |

## Fields written by setup (normally left alone)

| Key | Meaning |
| --- | --- |
| `exp_dir`, `bin_path`, `etc_path`, `install_path` | Experiment and install locations. `bin_path` is `GEOSBIN`. |
| `batch_cmd` | `sbatch` or `qsub`. |
| `batch_time`, `run_t` | Walltime header and value. |
| `batch_jobname`, `run_n` | Job name header and value (`<expid>_RUN`). |
| `run_q` | Queue or constraint (`SBATCH --constraint=mil`, `PBS -q normal`). |
| `run_p` | The node/rank request saved at setup. **The node count in it is recalculated** at every submission. |
| `batch_group_full` | Account / group list. |
| `batch_outputname`, `batch_joinouterr` | Output file settings. |
| `num_CPUs` | Cores per node for the processor chosen at setup. |
| `run_command` | The MPI launcher prefix (`$GEOSBIN/esma_mpirun -np `, or `mpirun -np ` on macOS). |
| `bc_base`, `bcs_dir`, `coupled_dir`, `sst_dir`, `grid_dir`, `abcs_dir` | Boundary condition locations. |
| `GCMEMIP`, `RSTDATE` | GCMEMIP mode and restart date. |
| `several_tries` | Set at NAS with MPT; allows up to 3 launch attempts. |
| `singularity_build` | Container build (Singularity runs are not yet implemented). |
| `expConfig.experiment_id`, `expConfig.io_server` | Experiment ID; whether IOSERVER is on (adds `IOSERVER_NODES` to the node count). |
| `atmos.*` | `res`, `nx`, `ny`, `mp_turn_off_wsub_extdata`, `RRTMGP_RADIATION`. |
| `ocean.*` | `coupled`, `out`, `MOM6`, `CICE6`, `DATAATM`, `res`, `data`, `model`, `preload`, `mit_remap_name`. `mit_remap_name` is the data-atmosphere remap file name for MIT (empty for other oceans); it is checked before each MIT segment starts. |
| `land.*` | `bcs`, `GCMRUN_catchment`. |
| `gocart.emissions` | `AMIP_EMISSIONS` or `OPS_EMISSIONS`. |

`atmos.nx`/`atmos.ny` are a record of the layout at setup time. Submission
sizing uses the current `AGCM.rc`, not these values.

## Older, larger run YAML files

Experiments created before this change have a run YAML holding the whole
setup context. These files are still read as they are and are never
rewritten. The new options are all optional, so you can add any of them by
hand.

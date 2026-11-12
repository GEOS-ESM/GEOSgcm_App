# Running the Model

`gcm_run.py` is the only command you need. It replaces `gcm_run.j`, and it
behaves differently depending on where it is run:

| Where you run it | What happens |
| --- | --- |
| Login node (neither `SLURM_JOB_ID` nor `PBS_JOBID` is set) | Works out the batch resources and **submits** a job. |
| Inside a Slurm or PBS allocation | **Runs** the model segments, then resubmits itself if `END_DATE` has not been reached. |

## Submitting from a login node

```bash
cd /path/to/experiment
python3 ./gcm_run.py --config run_<expid>.yaml
```

The command it runs is printed (`Submitting: sbatch ...`). The scheduler
comes from `batch_cmd` in the run YAML: `sbatch` at NCCS, `qsub` at NAS.
The job script is always the static wrapper `$GEOSBIN/gcm_run.sh`; there is
no longer a templated `gcm_run.j` to edit.

Slurm job output goes to `gcm_run.o<jobid>` in the directory you submitted
from.

## Running inside an allocation

Both of these work, and neither requires sourcing `g5_modules.sh` first:

```bash
# inside an interactive allocation (salloc / qsub -I)
python3 ./gcm_run.py --config run_<expid>.yaml

# or submit the Python script directly; you supply the resource flags
sbatch [resource flags] ./gcm_run.py --config run_<expid>.yaml
```

When `gcm_run.py` finds a job ID but not `GEOS_RUN_ENV_READY`, it re-runs
itself through `$GEOSBIN/gcm_run.sh`. The wrapper sources `g5_modules.sh`,
sets `GEOSDIR`, `GEOSETC`, `SITE`, the library paths and the stack limit,
then execs `$GEOSBIN/gcmpy/gcm_run.py`.

> In-job runs always use the **installed** copy of `gcm_run.py` and
> `gcmpy`, not the copy in the experiment directory. Rebuild and install
> to pick up changes to the run code.

## How many nodes are requested

Before **every** submission and resubmission, the runner reads the current
`AGCM.rc` (or `OGCM.NX`/`OGCM.NY` for coupled runs) and calculates:

```
nodes = ceil(NX * NY / ranks_per_node) + IOSERVER_NODES (if IOSERVER is on)
```

`ranks_per_node` is taken from the first of these that is set:

1. `cpus_per_node` in `run_<expid>.yaml`
2. `--ntasks-per-node` (Slurm) or `mpiprocs`, then `ncpus` (PBS), from `run_p`
3. `num_CPUs`

This means you can change the layout in `AGCM.rc` between jobs, and the
next submission is resized to match. You do **not** need to edit `run_p`.

```yaml
# run_<expid>.yaml — use 60 MPI ranks per node instead of the site default
cpus_per_node: 60
```

With `NX=4`, `NY=24`, that requests 2 nodes at 60 ranks per node.
`cpus_per_node` must be a positive, unquoted integer, or `null`. With PBS,
it replaces both `ncpus` and `mpiprocs` and keeps the other `select`
qualifiers (for example `model=rom`). Only a single, homogeneous `select=`
chunk is supported.

The override only affects runner-managed submissions. It does not resize a
job that is already running, and it does not apply when you call
`sbatch`/`qsub` yourself.

## What a job does

`run_segments()` in `gcm_run.py` ports the `gcm_run.j` segment loop:

1. **Job setup** (once): check resources, create `HISTORY` collection
   directories, recreate `<exp_dir>/scratch`, copy the RC files, pick the
   `GEOSgcm.x` executable, **stage restarts** (copied by default; see
   `stage_restarts_in_scratch`), and archive the initial restarts (unless
   `skip_initial_restart_archive` is set).
2. **For each segment, up to `NUM_SGMT`**: copy `CAP.rc`, compute the segment
   end time, stage emissions, WaveWatch dates, heartbeat sync, ExtData and
   linkbcs, then (MIT only) check the MIT/CICE4 inputs, run the pre-run checks,
   **apply `mpi_environment`**, run
   `bundleParser.py`, set up REPLAY and OpenMP, **launch `GEOSgcm.x`**
   (logging the environment first if `print_run_environment` is set), then
   archive checkpoints, move HISTORY output to holding, and run
   post-processing.
3. **After the loop**: copy the restarts and `cap_restart` back to the
   experiment directory.
4. **Resubmit** if `cap_restart` is still before `END_DATE`.

A failed model launch (non-zero exit, or no `EGRESS` file) fails the job.
At NAS with MPT, `several_tries` allows up to 3 launch attempts.

## Controlling the run length

The run length is set in `CAP.rc`, exactly as before:

| Key | Meaning |
| --- | --- |
| `END_DATE` | The experiment stops once `cap_restart` reaches this date. |
| `JOB_SGMT` | The length of one model segment. |
| `NUM_SGMT` | The number of segments run in one batch job before it resubmits. |

## A quick test run

```yaml
# top level of run_<expid>.yaml
stop_after_run: true               # exit 0 right after the first segment
skip_initial_restart_archive: true # don't tar up the input restarts
```

With these set, the job runs one segment and then exits without archiving,
moving output, post-processing, copying restarts back or resubmitting.
Output stays in `scratch/`. **Copy anything you want to keep before running
again**, because the next run recreates `scratch/`. Combine this with a short
`JOB_SGMT` in `CAP.rc`.

## Debugging the environment

```yaml
print_run_environment: true
```

Before each launch attempt, the log shows a sorted `NAME=value` block,
delimited by `=== BEGIN model-launch environment ...` and
`=== END model-launch environment ===`. Values whose names look like
credentials are redacted, and control characters are escaped. Redaction is
based on names only, so review the log before you share it.

## Legacy csh helper scripts

`post/gcm_post.j`, `archive/gcm_archive.j`, `forecasts/` and `plot/` are
still csh scripts rendered at setup time. Their MPI `setenv` lines are
created from the same `mpi_environment` defaults, but **editing the run YAML
later does not change these scripts.**

# Quick Start

This page goes from a built model to a running experiment in the fewest
steps. Each step links to a page with more detail.

## 0. Prerequisites

- A GEOSgcm build that includes this version of `GEOSgcm_App`, installed
  in the usual way (for example with `parallel_build.csh`, or with
  `cmake --build ... --target install`). Everything below refers to that
  install tree as `$INSTALL`, so `$INSTALL/bin` is `GEOSBIN`.
- A Python 3.10 or later with `PyYAML`, `questionary` and `Jinja2` on your
  `PATH`. Sourcing `$INSTALL/bin/g5_modules.sh` (or the `.csh` version)
  provides one at NCCS and NAS. The system `python3` on login nodes is
  often too old.
- Initial restart files for the resolution you plan to run.

## 1. Create the experiment

The setup script must be run **from** `$INSTALL/bin/gcmpy`, because it finds
the rest of the install relative to the current directory.

```bash
source $INSTALL/bin/g5_modules.sh
cd $INSTALL/bin/gcmpy
./gcm_setup.py
```

Answer the prompts. When setup finishes, the last line it prints is:

```
Experiment can be found in /path/to/your/experiment
```

See [Creating an Experiment](Creating-an-Experiment.md) for what each
question means and for non-interactive (YAML-driven) setup.

## 2. Provide restarts and `cap_restart`

Setup does **not** create `cap_restart` or copy restarts. Put them in the
experiment directory yourself:

```bash
cd /path/to/your/experiment
cp /path/to/restarts/*_rst .            # names must match AGCM.rc
echo "20000414 210000" > cap_restart    # YYYYMMDD HHMMSS start time
```

Coupled (MOM6) runs also need the ocean restarts in `RESTART/`.

## 3. Check the run length

Edit `CAP.rc`: set `END_DATE`, `JOB_SGMT` (the length of one segment) and
`NUM_SGMT` (segments per batch job). For a first test, one day and one
segment is a good choice.

## 4. Submit

From a login node:

```bash
python3 ./gcm_run.py --config run_<expid>.yaml
```

This works out the number of nodes from the current `AGCM.rc` layout and
submits the job with `sbatch` (NCCS) or `qsub` (NAS). The job resubmits
itself until `END_DATE` is reached. See
[Running the Model](Running-the-Model.md).

## 5. (Optional) A quick test that stops after one segment

Add these lines at the top level of `run_<expid>.yaml` before submitting:

```yaml
stop_after_run: true
skip_initial_restart_archive: true
```

The job then stops right after the first successful model segment and
leaves all output in `scratch/`. See
[Run YAML Reference](Run-YAML-Reference.md#stop_after_run).

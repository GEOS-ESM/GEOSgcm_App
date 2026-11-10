# Creating an Experiment

There are three entry points in `$INSTALL/bin/gcmpy`:

| Script | What it does |
| --- | --- |
| `gcm_setup.py` | Asks the setup questions, or reads a YAML driver with `--yaml`, then builds the experiment. **This is the recommended entry point.** |
| `gcm_configure.py` | Only asks the questions and writes `<expid>.yaml` (to the current directory, or to `--output DIR`). It does not create an experiment. |
| `gcm_create.py` | Builds an experiment from a YAML driver (`--yaml FILE`). See the warning below. |

> **Run these scripts from `$INSTALL/bin/gcmpy`.** `set_env_dicts()` finds
> the configuration, `bin`, `etc` and install directories relative to the
> *current working directory*, not relative to the script's location.

> **Warning:** at present `gcm_create.py` does not call `stage_bcs()` or
> `stage_run_code()`. Experiments it creates have no `linkbcs.yaml`, no
> local `gcm_run.py`, and no local `gcmpy/` package. Use
> `gcm_setup.py --yaml FILE` instead.

## Interactive setup

```bash
source $INSTALL/bin/g5_modules.sh
cd $INSTALL/bin/gcmpy
./gcm_setup.py
```

The questions are defined in `gcmpy/setup_tools/questions/*.yaml` and are
asked in this order:

| File | Questions |
| --- | --- |
| `exp_setup.yaml` | Experiment ID, one-line description, clone an existing experiment?, experiment directory |
| `atmospheric_model.yaml` | Horizontal resolution (`c12` … `c2160`), vertical levels (default 72), microphysics (`BACM_1M`, `GFDL_1M`, `MGB2_2M`), use IOSERVER?, processor type |
| `ocean_model.yaml` | Coupled ocean?; if not, the data-ocean resolution; if so, the ocean model (MOM6/MIT), its resolution, sea ice model, and data atmosphere; and `HEARTBEAT_DT` |
| `land_model.yaml` | Land boundary conditions (`NL3`, `v12`, `v14`, `ICA`), land model (Catchment / CatchmentCN-CLM4.0) |
| `gocart.yaml` | GOCART aerosols (actual or climatological), emissions (AMIP or OPS) |
| `directory_setup.yaml` | HISTORY template, group ID (batch account) |

Several defaults depend on earlier answers:

- **IOSERVER**: the default comes from the `io_server` entry for the
  selected resolution in `catalogs/atmosphere_catalog.yaml`. It is off for
  c12–c90 and on for c180 and above. Your answer always wins.
- **Heartbeat**: the default comes from the catalog. `BACM_1M` recommends
  450 s at any resolution; otherwise the value is set per resolution, and
  for c180 it also depends on the ocean (450 s coupled, 600 s with a data
  ocean). Whatever you enter is used, and coupled runs also use it as the
  ocean/sea ice time step.
- **Processor**: NCCS offers `mil` (Milan, 120 ranks per node). Cascade Lake
  (`cas`) has been retired. At NAS the choices follow the OS the model was
  **built** on, read from `<install>/etc/OS.rc` (written by ESMA_cmake at
  configure time): RHEL 8 (TOSS4) offers `rom` (default), `mil`, `cas`, `bro`
  and `sky`; RHEL 9 (TOSS5) offers only `tur` (Turin, 240 ranks per node).
  Only the distro ID and major version are compared, so 8.10 and 8.11 are
  equivalent. This lets you set up a Turin experiment from a TOSS4 login node
  with a TOSS5 build. If `OS.rc` is missing (older installs) setup prints a
  warning and uses the OS running setup instead; a malformed `OS.rc` is an
  error. YAML-driven setup validates the same choices. Existing experiments
  and clones are not checked.
- **Experiment directory**: the parent directory you used last time (saved in
  `~/.EXPDIRroot`), or `/discover/nobackup/$USER/<expid>` at NCCS and
  `/nobackup/$USER/<expid>` at NAS.

## YAML-driven (non-interactive) setup

Every setup writes a receipt, `<exp_dir>/<expid>.yaml`, holding the answers
to the setup questions. To create a similar experiment without the prompts:

```bash
cp /path/to/old_exp/old_expid.yaml new_expid.yaml
# edit experiment_id, exp_dir, and anything else you want to change
cd $INSTALL/bin/gcmpy
./gcm_setup.py --yaml /full/path/to/new_expid.yaml
```

`gcm_configure.py` can also produce a driver file without creating an
experiment. With `--yaml`, cloning is always turned off.

## Cloning

Answer **yes** to *"Would you like to CLONE an old experiment?"* and give the
path of the old experiment. Cloning copies the old experiment and swaps in
the new experiment ID and description; it skips the rest of setup. Cloning
is only available interactively.

## What setup produces

When setup succeeds, the last line it prints is
`Experiment can be found in <exp_dir>`. That directory contains:

| Item | Purpose |
| --- | --- |
| `run_<expid>.yaml` | **The run configuration** read by `gcm_run.py`. See [Run YAML Reference](Run-YAML-Reference.md). |
| `<expid>.yaml` | The setup receipt (your answers). It is not read at run time. |
| `AGCM.rc`, `CAP.rc`, `HISTORY.rc`, `fvcore_layout.rc`, … | Model configuration. Run-time node sizing reads `NX`/`NY` and `IOSERVER_NODES` from `AGCM.rc`. |
| `linkbcs.yaml`, `linkbcs_config.yaml` | Boundary-condition manifest, applied at the start of every segment. |
| `gcm_run.py`, `gcmpy/` | An experiment-local copy of the run code (setup-only modules are left out). |
| `RC/` | Component resource files. |
| `archive/`, `forecasts/`, `plot/`, `post/`, `regress/` | The csh helper scripts (`gcm_post.j`, `gcm_archive.j`, …). |
| `RESTART/` | Coupled runs only: ocean and sea ice restarts go here. |

## Before the first run

Setup does **not** provide initial conditions. You must:

1. Copy the restart files named in `AGCM.rc` (`*_RESTART_FILE` entries) into
   the experiment directory. Either all tile-layout restarts or all
   face-layout restarts (`fvcore_internal_face_1_rst`, …) may be used, but
   not a mix: if both `fvcore_internal_rst` and a face file are present,
   the run aborts.
2. Create `cap_restart` with the start time, for example
   `echo "20000414 210000" > cap_restart`.
3. For coupled runs, put the MOM6/CICE6 restarts in `RESTART/`.
4. Check `CAP.rc` (`END_DATE`, `JOB_SGMT`, `NUM_SGMT`) and, if you like,
   adjust the layout in `AGCM.rc`.

Then continue with [Running the Model](Running-the-Model.md).

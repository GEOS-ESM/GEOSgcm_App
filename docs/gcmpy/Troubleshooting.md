# Troubleshooting

| Symptom | Likely cause / fix |
| --- | --- |
| `SyntaxError` / `TypeError: unsupported operand type(s) for \|` when running `gcm_setup.py` or `gcm_run.py` | The system `python3` is too old (3.6 on Discover login nodes). Run `source $INSTALL/bin/g5_modules.sh` first, or use any Python 3.10 or later that has PyYAML. |
| `FileNotFoundError` for `.../gcmpy/setup_tools/config/...` or `SITE.rc` during setup | Setup was not run from `$INSTALL/bin/gcmpy`. Change to that directory and run it again. |
| Setup exits right after the group ID question | The site is neither NCCS nor NAS. Group selection is only supported at those sites. |
| `ERROR: unknown processor 'cas' for NCCS` | Cascade Lake has been retired at NCCS. Use `mil`. |
| `Cannot identify NAS build OS` / `Cannot identify NAS setup OS` | The OS from `<install>/etc/OS.rc` (build) or `/etc/os-release` (setup, when `OS.rc` is missing) is not RHEL 8 (TOSS4) or RHEL 9 (TOSS5). |
| `WARNING: <install>/etc/OS.rc not found` | The install predates build-OS metadata; setup used the OS running setup. Make sure it matches the OS the model was built on. |
| `OS.rc is malformed` | `etc/OS.rc` needs `OS_ID` and a numeric `OS_VERSION`. Rebuild the install. |
| `processor ... is not supported on this NAS build OS` | Change the YAML processor to a choice listed in the error, or use an installation built for the OS you want. |
| `this install was built on rhel N but you are submitting from rhel M` | Submit from a NAS login node with the build's OS (TOSS5 for Turin). |
| `run_p model '...' cannot run on rhel N` | Fix `model=` in `run_p` in the run YAML: `tur` needs a TOSS5 build, the other NAS processors need TOSS4. |
| Run fails because `cap_restart` or a restart is missing | Setup no longer creates `cap_restart`. Create it (`YYYYMMDD HHMMSS`) and copy in the restarts. |
| Run aborts saying both grid and face restarts exist | Remove either `fvcore_internal_rst` or the `*_face_*_rst` files. Don't mix the two layouts. |
| `Cannot size batch allocation: .../AGCM.rc is missing` | `exp_dir` in the run YAML is wrong, or `AGCM.rc` was moved. |
| `cpus_per_node must be a positive YAML integer or null` | It was quoted (`"60"`), or set to zero or a negative number. Use `cpus_per_node: 60`. |
| `Expected one homogeneous PBS select request in run_p` | `run_p` has `+` chunks, or more than one `select=`. Only a single chunk is supported. |
| `mpi_environment[...] must be a string ...` | A value is unquoted. Write `"12"` rather than `12`, and `"true"` rather than `true`. |
| `Invalid mpi_environment variable name` | A key is not a valid environment variable name. |
| MPI variables you deleted from `mpi_environment` still show up | The old `mpi_config` / `ocean.mpt_shepherd` blocks are still in the run YAML. Delete them. |
| Changing `mpi_environment` has no effect on post or archive jobs | Those csh scripts are rendered at setup time. Edit them directly. |
| `stage_restarts_in_scratch must be 'copy' or 'link'` | The value is misspelled or not one of those two words. |
| Original restarts changed after a `link` run | The model wrote through a link. Use `copy` for that configuration. |
| `gcm_run.sh: cannot locate g5_modules.sh` | `gcm_run.sh` was submitted by hand without `GEOSBIN`. Submit with `gcm_run.py` instead. |
| `Cannot initialize runtime: .../gcm_run.sh not found` | `bin_path` in the run YAML doesn't point at an install that has `gcm_run.sh` (installs from before this change don't have it). |
| `PBS environment paths must not contain commas` | Move or rename the experiment or install path so it has no commas. |
| Changes to `$EXPDIR/gcmpy/` have no effect in batch jobs | In-job runs use `$GEOSBIN/gcmpy/gcm_run.py`. Make the change in the source, then rebuild and install. |
| Output disappeared after a `stop_after_run` test | `scratch/` is recreated at the start of every job. Copy the output out first. |
| An experiment made with `gcm_create.py` has no `gcm_run.py` or `linkbcs.yaml` | This is a known gap. Use `gcm_setup.py --yaml FILE`. |
| `You MUST select c90 or c180 atmospheric resolution for MIT ocean!` | MIT is only offered for c90 and c180. Pick one of those, or choose another ocean. |
| `MIT <grid> requires <llc> with its matching compiled configuration.` | The LLC code does not match the atmosphere (`llc90` with c90, `llc270` with c180). Setup asks again, and a driver YAML with the wrong code stops setup with this message. |
| `These MIT profiles are compiled with Nr=50; choose 50 levels.` | `LM` was changed from 50. Setup asks again. |
| `ERROR: Atmosphere and compiled MIT ocean rank counts must agree.` | The atmosphere NX×NY does not equal the MIT rank count (360 for llc90, 768 for llc270). Check the `atmos` layout in the driver YAML, or the catalog. |
| `ERROR: GEOS_MIT_BCS_PROFILE must be standard or G12_t4_MITC4.` | The variable has another value. Use one of the two names. |
| `ERROR: G12_t4_MITC4 boundary conditions require C180 on NCCS/Discover.` | The G12 profile was forced for a grid or site it does not support. Unset `GEOS_MIT_BCS_PROFILE`. |
| `ERROR: MIT tile grids in <file> do not match the selected atmosphere/ocean profile.` | The tile file's header names other grids. Check `GEOS_MIT_TILE` and the land tag (`GEOS_MIT_LAND_BCRSLV`). |
| `ERROR: Invalid MIT tile header in <file>` or `MIT tile grids do not match the AGCM.rc grids` (run time) | The tile linked for this segment is not the one for this profile. Check the link in `scratch/tile.data`. |
| `Missing MIT/CICE4 input(s): ...` (run time) | A file the MIT run needs is absent: a linked input, `libCICE4`, or `<exp_dir>/mit_input/data` / `data.exch2`. Restore it and run again. Remap is needed only with a data atmosphere. |
| `MIT_SETUP.txt` shows `(stagebcs default)` for a path | That input is not overridden, so the standard file for the site is used. Set the `GEOS_MIT_*` variable if you need another. |

## Getting more information

- Set `print_run_environment: true` to see the exact environment passed to
  the MPI launcher.
- Uncomment `I_MPI_DEBUG: "9"` (Intel MPI) or `MPI_DISPLAY_SETTINGS: "1"`
  (MPT) under `mpi_environment`.
- Each submission prints the full `sbatch`/`qsub` command. Check the node
  count and ranks per node there.

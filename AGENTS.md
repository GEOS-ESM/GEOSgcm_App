# Agent Instructions for GEOSgcm_App

## Documentation lives in the source tree

The technical documentation is in `docs/`. The GitHub wiki only links to
these files and contains no copies, so **never edit the wiki to document a
change. Edit `docs/` instead.**

When you change behaviour that users or developers can see, update the
matching documentation **in the same change**. Do this without being asked,
and say which pages you updated in your summary. If a change doesn't need a
documentation update, say so.

### gcmpy (`gcmpy/` → `docs/gcmpy/`)

| If you change… | Update… |
| --- | --- |
| `gcmpy/gcm_setup.py`, `gcm_configure.py`, `gcm_create.py`, `setup_tools/questions/*.yaml`, `setup_tools/process_questions.py`, or the files `SetupGCM` writes to the experiment directory | `docs/gcmpy/Creating-an-Experiment.md` (and `Quick-Start.md` if the basic steps change) |
| `gcmpy/gcm_run.py`, `gcmpy/gcm_run.sh`, node sizing, segment flow, resubmission | `docs/gcmpy/Running-the-Model.md` |
| Any key read from `run_<expid>.yaml`, `setup_tools/config/run_yaml_fields.yaml`, or the defaults written by `SetupGCM.template()` | `docs/gcmpy/Run-YAML-Reference.md` |
| `setup_tools/config/mpi_config.yaml`, `run_tools/mpi_env.py` | `docs/gcmpy/Run-YAML-Reference.md` (`mpi_environment`) |
| `setup_tools/catalogs/*.yaml` defaults (heartbeat, IOSERVER, timesteps) | `docs/gcmpy/Creating-an-Experiment.md`, `docs/gcmpy/Developer-Guide.md` |
| `batch_tools/computing_sites.py` (sites, processors) | `docs/gcmpy/Creating-an-Experiment.md` |
| Package layout, a new or renamed `run_tools` module, test layout, CI | `docs/gcmpy/Developer-Guide.md` |
| New error messages or failure modes | `docs/gcmpy/Troubleshooting.md` |
| Short run-time settings reference | `gcmpy/README.md` |

When you add a new page, link it from `docs/gcmpy/README.md`. If the page
should be reachable from the wiki, say so in your summary; adding the wiki
link is done by hand.

`docs/gcmpy/New-Features-PR-935.md` is a historical record of PR #935. Don't
rewrite it for later changes.

## gcmpy tests

Run the tests from `gcmpy/` (on NCCS, source `$INSTALL/bin/g5_modules.sh` first
to get a recent Python):

```bash
cd gcmpy
python -m pytest gcmpy_tests -q
```

Add or update tests in the `gcmpy/gcmpy_tests/` module that mirrors the code
you changed. The tests must not need a model build or a batch scheduler.

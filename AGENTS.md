# AGENTS.md

**Audience:** coding agents and developers working on the `GEOSgcm_App` repository.
**Describes:** the MAPL2 line, branch `develop`.
**Verified against:** `GEOSgcm_App` @ `9ef207f` (2026-11-05).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): `src/Applications/@GEOSgcm_App`, entry `GEOSgcm_App` (`tag: v3.1.0`, `develop: develop`).
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

`GEOSgcm_App` holds the `GEOSgcm.x` main program (`GEOSgcm.F90`), a small
`idfupd.x` utility, the interactive setup scripts, and the resource-file and
job-script templates from which an experiment directory is made
(`CMakeLists.txt`). It holds no model physics. The repository is flat: no
source subdirectories.

## Documents

- `docs/ARCHITECTURE.md`: directory map, how `gcm_setup` builds an experiment,
  who reads each template, resource keys, the `gcm_run.j` job flow, variants,
  traps, observations. Read first.
- `docs/decisions.md`: why setup, templates and the run script are arranged
  this way.
- Fixture: AGENTS.md (`../../../AGENTS.md` in the fixture) and
  docs/ARCHITECTURE.md (`../../../docs/ARCHITECTURE.md` in the fixture) (section 5 summarises
  this repository).
- Components that read the generated resource files:
  GEOSgcm_GridComp AGENTS.md (`../../Components/@GEOSgcm_GridComp/AGENTS.md` in the fixture).

## Rules

- Templates hold `@NAME` placeholders. `gcm_setup` replaces them with `sed`
  from one list (`sedfile`); a placeholder with no entry stays in the output.
  Add or rename a placeholder in the template and in the `sedfile` of every
  setup script that processes that file (`gcm_setup`, "Create Local Scripts and
  Resource Files").
- `gmichem_setup`, `geoschemchem_setup` and `stratchem_setup` are copies of
  `gcm_setup`. *Inferred:* a change to common logic in `gcm_setup` usually
  needs the same change in the three copies (ARCHITECTURE.md, section 7.1).
- At run time the model reads files in the scratch directory, not the
  templates; a template change affects only experiments made afterwards
  (ARCHITECTURE.md, section 10).
- Note bugs and surprises under "Observations" in `docs/ARCHITECTURE.md`; do
  not fix them unless asked. Keep the documents current when a script,
  template or placeholder changes. Cite files and routines, not line numbers.

## Build and test

- Built as part of the fixture; see the fixture's `AGENTS.md`. `CMakeLists.txt`
  defines two executables and installs templates to `etc/` and scripts to
  `bin/`; the setup scripts are processed with `configure_file(... @ONLY)`.
- `gcm_setup` must be run from the `bin/` of an installation (it checks for
  `GEOSgcm.x` next to itself; `gcm_setup`, "Build Directory Locations").
- No test target. CI: builds with several compilers in
  `.github/workflows/workflow.yml`; YAML lint in `.github/workflows/validate_yaml_files.yml`;
  `.circleci/config.yml` builds, then runs a one-hour AMIP case and a one-hour
  coupled MOM6 case (no ExtData).
- Changing a template or script cannot be checked by a build alone: run
  `gcm_setup` from an install and look at the files it writes.

## gcmpy

### gcmpy documentation

The technical documentation for gcmpy is in `docs/gcmpy/`. The GitHub wiki only
links to these files and contains no copies, so **never edit the wiki to
document a change. Edit `docs/` instead.**

When you change behaviour that users or developers can see, update the
matching documentation **in the same change**. Do this without being asked,
and say which pages you updated in your summary. If a change doesn't need a
documentation update, say so.

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

### gcmpy tests

Run the tests from `gcmpy/` (on NCCS, source `$INSTALL/bin/g5_modules.sh` first
to get a recent Python):

```bash
cd gcmpy
python -m pytest gcmpy_tests -q
```

Add or update tests in the `gcmpy/gcmpy_tests/` module that mirrors the code
you changed. The tests must not need a model build or a batch scheduler.

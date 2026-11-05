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
- Fixture: [AGENTS.md](../../../AGENTS.md) and
  [docs/ARCHITECTURE.md](../../../docs/ARCHITECTURE.md) (section 5 summarises
  this repository).
- Components that read the generated resource files:
  [GEOSgcm_GridComp AGENTS.md](../../Components/@GEOSgcm_GridComp/AGENTS.md).

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

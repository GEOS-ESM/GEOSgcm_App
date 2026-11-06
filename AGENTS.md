# AGENTS.md

**Audience:** coding agents and developers working on the `GEOSgcm_App` repository.
**Describes:** the MAPL3 line, branch `release/MAPL-v3`.
**Verified against:** `GEOSgcm_App` @ `e65517a` (2026-11-06).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `f50622d`): `src/Applications/@GEOSgcm_App`, entry `GEOSgcm_App` (`branch: release/MAPL-v3`, `develop: develop`).
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

`GEOSgcm_App` holds a small `idfupd.x` utility (`idfupd.F90`), the
interactive setup scripts, and the resource-file and
job-script templates from which an experiment directory is made
(`CMakeLists.txt`). It holds no model physics. The repository is flat: no
source subdirectories. On the MAPL2 line it also holds the `GEOSgcm.x` main
program; that file is deleted on this line.

> **Build status on this line:** `GEOSgcm.x` is not built, and nothing in this
> repository is built. `src/CMakeLists.txt` in the fixture has
> `# add_subdirectory(Applications)     # not yet ported to MAPL3 - restore when ported`,
> so this repository's `CMakeLists.txt` is not read. The file itself no longer
> builds `GEOSgcm.x`: it defines only `idfupd.x`, and `GEOSgcm.F90` is deleted
> on `release/MAPL-v3` (the MAPL2-line `CMakeLists.txt` target `GEOSgcm.x` is
> removed). The setup scripts and templates are the MAPL2-line files, unchanged
> (the diff from the MAPL2 line is `.circleci/config.yml`, the workflow,
> `CMakeLists.txt`, `GEOSgcm.F90` and `idfupd.F90`); they name components
> (`GEOS_GcmGridComp`, ocean, surface, moist, ...) that the fixture does not
> build on this line (see the fixture's `docs/ARCHITECTURE.md`). On this line
> the parent's documents are not written yet; the MAPL2-line versions are on
> its `develop` branch.

## Documents

- `docs/ARCHITECTURE.md`: directory map, how `gcm_setup` builds an experiment,
  who reads each template, resource keys, the `gcm_run.j` job flow, variants,
  traps, observations. Read first.
- `docs/decisions.md`: why setup, templates and the run script are arranged
  this way.
- Fixture (on this line the parent's documents are not written yet; the
  MAPL2-line versions are on its `develop` branch): AGENTS.md (`../../../AGENTS.md` in the fixture) and
  docs/ARCHITECTURE.md (`../../../docs/ARCHITECTURE.md` in the fixture).
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

- Not built on this line (status above). `CMakeLists.txt` defines one
  executable, `idfupd.x`, and installs templates to `etc/` and scripts to
  `bin/`; the setup scripts are processed with `configure_file(... @ONLY)`.
- `gcm_setup` must be run from the `bin/` of an installation (it checks for
  `GEOSgcm.x` next to itself, which no build of this line provides; `gcm_setup`, "Build Directory Locations").
- No test target. CI: builds with several compilers in
  `.github/workflows/workflow.yml`; YAML lint in `.github/workflows/validate_yaml_files.yml`;
  `.circleci/config.yml` builds, then runs a one-hour AMIP case and a one-hour
  coupled MOM6 case (no ExtData).
- Changing a template or script cannot be checked by a build alone: run
  `gcm_setup` from an install and look at the files it writes.

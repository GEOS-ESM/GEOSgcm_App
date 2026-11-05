# GEOSgcm_App: previous architectural decisions

**Audience:** coding agents and developers working on the `GEOSgcm_App` repository.
**Describes:** the MAPL2 line, branch `develop`.
**Verified against:** `GEOSgcm_App` @ `9ef207f` (2026-11-05).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`).
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Only what is easy to see now; see [ARCHITECTURE.md](ARCHITECTURE.md) for
context. The fixture-level decision on templates is in the
[fixture decisions](../../../../docs/decisions.md).

### Experiment files made by `sed` placeholders and `#DELETE` line markers
- Pattern: templates hold `@NAME` placeholders; `gcm_setup` writes a `sedfile`
  of `s?@NAME?$VALUE?g` lines and runs `sed -f` on each template. A value of
  `#DELETE` marks a line, and an `awk` pass then drops lines whose first field
  contains it (`gcm_setup`, "Create Local Scripts and Resource Files" and
  "Delete or Enable EXP Configuration Variables").
- Likely reason: *Inferred:* one template family serves atmosphere-only,
  data-ocean, coupled and replay runs, and csh scripts have no template
  engine; text substitution needs no extra tools at the site.
- Question for maintainers: `@COUPLED` is a prefix of `@COUPLEDIR`, and
  substitution has no word boundary. Is the order of `sedfile` lines the
  intended way to handle such pairs?

### Two directories per experiment, plus an `RC/` snapshot
- Pattern: HOMDIR receives scripts and resource files; EXPDIR receives output
  and restarts. `gcm_setup` copies everything under `etc/` into
  `$EXPDIR/RC`, removes `*.tmpl` and `fvcore_layout.rc`, and `gcm_run.j`
  copies `RC/*` into scratch, then HOMDIR `*.rc`, `*.nml` and `*.yaml` on top
  (`gcm_setup`, "Copy Model Executable and RC Files to Experiment Directory";
  `gcm_run.j`, scratch set-up).
- Likely reason: *Inferred:* HOMDIR is small and editable, often on a backed-up
  file system; EXPDIR is large and on scratch storage. The `RC/` copy freezes
  the defaults installed at setup time, so later installs do not change a
  running experiment.
- Question for maintainers: is the `RC/` copy meant to be the record of an
  experiment's configuration, or only a source of defaults?

### One job script that resubmits itself, with a segment loop
- Pattern: `gcm_run.j` loops `NUM_SGMT` times inside one batch job; each pass
  runs `GEOSgcm.x` for `JOB_SGMT`, renames checkpoints to restarts, and
  archives. After the loop, if `cap_restart` is before the end date, it
  submits `gcm_run.j` again with `@BATCH_CMD`. `NUM_SGMT` and `JOB_SGMT` live
  in `CAP.rc`, read by the script (`NUM_SGMT` is not read by the cap)
  (`gcm_run.j`, "Re-Submit Job"; `CAP.rc.tmpl`).
- Likely reason: *Inferred:* batch queues limit wall time; several short
  segments per job avoid queue waits, and resubmission continues the run
  without a driver process.
- Question for maintainers: is `NUM_SGMT` in `CAP.rc` kept there only for
  convenience, since the cap does not use it?

### Per-resolution defaults in one sourced file
- Pattern: `gcm_setup` sources `set_resolution_params.csh <IM> <CLDMICRO>
  <OGCM> <OCNMODEL>`, which sets time steps, layout, History grid, readers,
  segment length and a few scheme flags per horizontal resolution
  (`gcm_setup`; `set_resolution_params.csh`).
- Likely reason: *Inferred:* the tuned values for each grid were gathered from
  inline `if` chains in `gcm_setup` into one file that is easy to compare.
- Question for maintainers: `set_resolution_params.csh` has a `c4320` block
  that the `gcm_setup` menu does not offer. Is it reached another way?

### Chemistry setup variants are copies of `gcm_setup`
- Pattern: `gmichem_setup`, `geoschemchem_setup` and `stratchem_setup` hold
  the whole of `gcm_setup` with other chemistry prompts and History defaults
  (`CMakeLists.txt`, `setup_scripts`; `diff gcm_setup <variant>`).
- Likely reason: *Inferred:* each chemistry group needed its own prompts and
  defaults without a menu choice in the common script, and a copy was the
  quickest route.
- Question for maintainers: is the separation of the three copies intended to
  stay?

### Setup scripts configured by CMake with `@ONLY`
- Pattern: `CMakeLists.txt` runs `configure_file(${file} ${file} @ONLY)` for
  each setup script and installs the result to `bin/`; variables such as
  `CMAKE_SHARED_LIBRARY_SUFFIX` appear in scripts as `@NAME@`
  (`CMakeLists.txt`; `gcm_setup`, `OCEAN_PRELOAD`).
- Likely reason: *Inferred:* the shared-library suffix and install-specific
  values are known at build time, not at the user's setup time. Using `@ONLY`
  leaves `${...}` of csh untouched.
- Question for maintainers: because `gcm_setup` also uses `@NAME` for its own
  placeholders, is there a rule for telling build-time `@NAME@` from setup-time
  `@NAME`?

### `ExtData.rc` stub plus a generated `extdata.yaml`
- Pattern: `gcm_run.j` `touch`es an empty `ExtData.rc` ("MAPL expectation ...
  not actually used") and runs `construct_extdata_yaml_list.py
  GEOS_ChemGridComp.rc` to build `extdata.yaml` from the enabled chemistry
  components; `CAP.rc.tmpl` sets `USE_EXTDATA2G: .TRUE.` (`gcm_run.j`;
  `construct_extdata_yaml_list.py`; `CAP.rc.tmpl`).
- Likely reason: *Inferred:* the second-generation ExtData reads YAML; the YAML
  list depends on which chemistry components are on, so it is assembled per
  experiment rather than stored.
- Question for maintainers: is the `ExtData.rc` stub expected to go away when
  the cap stops loading `EXTDATA_CF`?

### Site settings inside `gcm_setup`
- Pattern: `gcm_setup` chooses boundary-data roots, batch commands and queue
  syntax by `SITE` (read from `SITE.rc`), in a block of `setenv` lines per
  site (`gcm_setup`, "Architecture Specific Variables").
- Likely reason: *Inferred:* the same script is used at several centres, and
  each needs its own paths and batch dialect.

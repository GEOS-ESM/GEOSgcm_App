# Developer Guide

## Source layout

```
gcmpy/
├── gcm_setup.py            # entry point: questions or --yaml, then SetupGCM
├── gcm_configure.py        # entry point: questions only -> <expid>.yaml
├── gcm_create.py           # entry point: --yaml -> experiment (see caveat)
├── gcm_run.py              # entry point: submit (login node) / execute (in job)
├── gcm_run.sh              # internal job wrapper, installed to $INSTALL/bin
├── requirements-test.txt   # pytest, PyYAML, questionary, Jinja2, netCDF4
├── gcmpy_doc/              # these documents
├── gcmpy_tests/gcmpy/      # pytest suite, mirrors the package layout
└── gcmpy/
    ├── batch_tools/        # site/processor tables, Slurm/PBS env, IOSERVER math
    ├── geos_settings/      # resolution lists (now catalog-derived)
    ├── run_tools/          # gcm_run.j ported to Python, one module per concern
    ├── setup_tools/
    │   ├── setup_main.py   # SetupGCM: the setup pipeline
    │   ├── atmosphere.py, ocean.py, land.py, gocart.py
    │   ├── catalogs/       # *_catalog.yaml: model defaults per resolution, etc.
    │   ├── config/         # mpi_config.yaml, run_yaml_fields.yaml
    │   ├── questions/      # question definitions (questionary)
    │   └── process_questions.py, generate_question.py, clone.py, setup_envs.py
    └── utils/              # yaml, catalog, path, regex, clock, file helpers
```

CMake installs everything under `$INSTALL/bin/gcmpy/`, and installs
`gcm_run.sh` to `$INSTALL/bin/`. `setup_envs.py` contains `@MPI_STACK@`,
which `configure_file` fills in, so **an uninstalled source tree cannot run
setup**. Tests mock this where they need to.

## Setup pipeline

`gcm_setup.py:main()` steps:

1. `set_env_dicts(cwd)` fills `pathdict` (config, bin, install, etc, …) and
   `envdict` (site from `etc/SITE.rc`, MPI stack, arch, run command). The
   working directory **must** be `$INSTALL/bin/gcmpy`.
2. `ask_questions()`, or `read_yaml_file(--yaml)`. If cloning, run
   `clone_exp()` and exit.
3. `SetupGCM(expConfig)`, then in order: `config_models` → `set_num_CPUs` →
   `config_models` → `set_atmos_params` → `set_nodes` → `set_batch_params` →
   dotfiles → `RC_setup` → `yaml_receipt` → `mpistacksettings` →
   `copy_files_into_exp` → `restarts` → `mod_RC_dir_for_pchem` →
   `config_chemGridComp` / `config_surfaceGridComp` / `config_gocartGridComp` →
   `config_heartbeat` → **`template`** → `stage_bcs` → `stage_run_code` →
   `organize_exp_dir`.

Key points:

- **`config_models`** builds the atmosphere, ocean, land and GOCART dicts
  from `catalogs/*.yaml` using `utils/catalog_ops.CatalogSearch`. The search
  follows `expConfig` answers as keys (for example
  `resolutions → c180 → MOM6`). When a value is a dict keyed by the ocean
  type, it picks the matching entry and falls back to `data_ocean`.
- **`template`** builds one large Jinja context. It writes
  `run_<expid>.yaml` using **only** the keys listed in
  `config/run_yaml_fields.yaml`, then renders the templates (with csh-style
  upper-case aliases) from the full context. The csh `mpi_config` string for
  the legacy scripts is generated here from `self.mpi_environment`.
- **`stage_bcs`** writes `linkbcs_config.yaml` and runs
  `$INSTALL/bin/stagebcs.py` to produce `linkbcs.yaml`. The MIT ocean is not
  implemented yet (it raises `NotImplementedError`).
- **`stage_run_code`** copies `gcm_run.py` and the run-related subpackages
  into the experiment directory.

> `gcm_create.py` currently skips `stage_bcs`, `stage_run_code` and the
> "Experiment can be found" message. Keep it in step with `gcm_setup.py`, or
> switch it to calling a shared function.

### Adding a setup question

1. Add an entry to the right file in `setup_tools/questions/`, with `type`
   (`text`/`confirm`/`select`/`path`), `prompt`, `choices`,
   `default_answer` and `follows_up` (conditions for showing it). The
   format is documented at the top of `ocean_model.yaml`. The order of the
   files is set in `load_questions.py`.
2. If the default or choices depend on earlier answers, add a
   `Handle.<name>` static method in `process_questions.py` and call it in
   `ask_questions()` **before** `load_question()`. Validation goes after.
3. Use the answer from `expConfig[...]` in `setup_main.py` or a model
   module.

### Changing resolution defaults

Edit `catalogs/atmosphere_catalog.yaml`. Resolution entries can be plain
values, or dicts keyed by `MOM6`/`MIT`/`data_ocean`. `microphysics` entries
override them (for example `BACM_1M: heartbeat: 450`). `heartbeat` and
`io_server` are only *prompt defaults*; the user's answers become
`expConfig['heartbeat']` / `expConfig['io_server']`. `test_atmosphere_config.py`
and `test_catalog_tokens.py` cover these.

### Adding a field to the run YAML

1. Make sure the field is in the `template()` context (`jinja_dict`).
2. Add it to `config/run_yaml_fields.yaml`, under `top_level` or under the
   right `nested` section.
3. Read it in `gcm_run.py` with a default (`run_config.get(key, default)`)
   so that older run YAML files still work.
4. Add tests. `test_template_vars.py` and `test_gcm_run_submit.py` show the
   patterns.

### Changing MPI defaults

Edit `config/mpi_config.yaml`. Values **must be strings or `null`**. Site
and ocean-specific additions (`intelmpi_NASA`, `MPI_SHEPHERD`) are in
`SetupGCM.mpistacksettings()`. `test_mpi_env.py` and
`test_csh_template_aliases.py` cover the Python and csh paths.

## Run pipeline

```
login node                         compute node
──────────                         ────────────
gcm_run.py main()
 └─ submit_job()
     ├─ build_sbatch_args() / build_qsub_args()
     │    └─ submission_nodes()  ← AGCM.rc NX/NY, IOSERVER_NODES, cpus_per_node
     └─ sbatch|qsub $GEOSBIN/gcm_run.sh  ───►  gcm_run.sh
                                             (g5_modules, LD paths, ulimit,
                                              GEOS_RUN_ENV_READY)
                                              └─ exec $GEOSBIN/gcmpy/gcm_run.py
                                                  main() → execute()
                                                   └─ run_segments()
                                                       ├─ run_job_setup()
                                                       ├─ run_one_segment() × NUM_SGMT
                                                       ├─ copy_restarts_to_exp_dir()
                                                       └─ submit_job() if not done
```

Each `run_tools` module replaces a block of `gcm_run.j`. The docstrings and
comments give the original `gcm_run.j:<lines>` references, which is the
quickest way to compare behaviour with the csh script.

| Module | Responsibility |
| --- | --- |
| `run_resources.py` | Resource resolution and the CPU/node over-subscription check |
| `scratchdir_ops.py`, `scratch_setup.py` | Create `scratch/` and fill it from the experiment directory |
| `executable_setup.py` | Choose `GEOSgcm.x` (Singularity is `NotImplementedError`) |
| `stage_restarts.py` | Parse restart names, detect face layout, copy or link, initial archive |
| `segment_time.py` | Segment end time and `END_DATE` clamping |
| `emissions.py`, `extdata.py`, `linkbcs_ops.py`, `ww3_ops.py`, `heartbeat_sync.py` | Per-segment input staging |
| `pre_run.py` | GWD, `tile.bin`, CICE6 ponds, openwater check, MERRA2OX, REPLAY |
| `mpi_env.py` | `apply_mpi_environment()` (legacy blocks, then mapping) |
| `env_builder.py` | OpenMP setup and teardown |
| `mpi_runner.py` | Build and run the MPI command, IOSERVER arguments, retries, `EGRESS` check, environment printing |
| `archiver.py` | Checkpoint renaming and restart tarballs |
| `post_run.py` | HISTORY to holding, ocean output, post-processing, forecasts, copy-back |

Not yet ported: Singularity execution, and the MIT and MOM5 ocean variants.
These are marked where they are called.

## Tests

```bash
cd gcmpy
python -m pip install -r requirements-test.txt   # or: source $INSTALL/bin/g5_modules.sh
python -m pytest gcmpy_tests -q
```

Run the tests from `gcmpy/`, so that `import gcm_run` / `import gcm_setup`
and `import gcmpy` resolve. No model build or scheduler is needed: the tests
mock subprocesses, the scheduler, `AGCM.rc` contents and the file system
(`tmp_path`). On Discover, the Python from `g5_modules.sh` (3.12) runs the
whole suite. CI uses 3.13.

Test layout mirrors the package:

| Area | Main tests |
| --- | --- |
| Submission and sizing | `run_tools/test_gcm_run_submit.py` |
| Segment control flow, `stop_after_run` | `run_tools/test_gcm_run_stop.py` |
| Wrapper and runtime initialization | `run_tools/test_gcm_run_sh_env.py` |
| MPI environment | `run_tools/test_mpi_env.py`, `run_tools/test_mpi_runner.py` |
| Restart staging | `run_tools/test_stage_restarts.py`, `run_tools/test_archiver.py` |
| Setup and templates | `setup_tools/test_template_vars.py`, `test_csh_template_aliases.py`, `test_atmosphere_config.py`, `test_setup_completion.py` |
| YAML I/O | `utils/test_yaml_ops.py` |

CI (`.github/workflows/python-tests.yml`) runs the same command on Linux and
macOS for every pull request.

## Conventions

- Keep the original `gcm_run.j:<lines>` references when porting or changing
  run logic.
- New run YAML keys must be optional, with defaults that match the old
  behaviour. Old run YAML files are never rewritten.
- Check user input before changing anything (see `apply_mpi_environment`).
- Booleans in the run YAML are real YAML booleans. MPI environment values
  are strings.
- Add or update tests in the matching `gcmpy_tests/` module, and update
  these docs and `gcmpy/README.md` when behaviour visible to users changes.

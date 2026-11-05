# GEOSgcm_App: run-time setup, templates and the run script

**Audience:** coding agents and developers working on the `GEOSgcm_App` repository or arriving at an experiment directory and asking where its files came from.
**Describes:** the MAPL2 line, branch `develop`.
**Verified against:** `GEOSgcm_App` @ `9ef207f` (2026-11-05). Fixture neighbours were read in the GEOSgcm fixture checkout (`GEOSgcm` @ `0f2e0fb`, `MAPL` @ `dc0eec14`).
**Placement:** as in the GEOSgcm fixture (`components.yaml` @ `0f2e0fb`): `src/Applications/@GEOSgcm_App`, entry `GEOSgcm_App` (`tag: v3.1.0`, `develop: develop`).
Other fixtures (for example GEOSadas, GEOSldas) may place this repository
elsewhere; paths inside this repository do not change.

Paths are relative to this repository unless stated. "Fixture" paths start
with `src/`. Unlabelled statements are Verified and carry a citation.
Terms: **EXPID** experiment identifier; **HOMDIR** the experiment's home
directory (scripts and resource files); **EXPDIR** the experiment directory
(output, restarts); **SCRDIR** the per-job scratch directory; **heartbeat**
the time step of the application clock; **IM, JM, LM** number of points in
two horizontal directions and of vertical levels; **restart** a file read at
start; **checkpoint** a file written at the end of a run (renamed to a restart
for the next segment); **segment** the stretch of model time one `GEOSgcm.x`
invocation covers; **BCS** boundary-condition datasets; **IAU** incremental
analysis update; **GOCART** Goddard Chemistry, Aerosol, Radiation, and Transport
model; **GF** Grell-Freitas convection; **UW** University of Washington shallow
convection; **SHOC** Simplified Higher-Order Closure; **RRTMG(P)** Rapid Radiative
Transfer Model for GCMs (with Parallel).

## 0. Where to look for X

Installed files: section 2. Building an experiment: section 3. Which file reads
which template: section 4. Keys that select schemes or grids: section 5. The
batch job: section 6. Surprises: sections 7 to 11.

## 1. What the repository is

- A collection of one main program, one small utility program, setup scripts,
  job-script templates and resource-file templates. It holds no model physics
  (`CMakeLists.txt`, `GEOSgcm.F90`).
- `GEOSgcm.x` (`GEOSgcm.F90`, program `GEOS5_Main`) builds a MAPL `MAPL_Cap`
  named `'GCM'` with `GEOS_GcmGridCompMod`'s `SetServices` as the root and
  calls `cap%run`. All run-time behaviour comes from files in the run directory
  (sections 4 and 5). The root component is documented in the
  [GEOSgcm_GridComp architecture](../../../Components/@GEOSgcm_GridComp/docs/ARCHITECTURE.md).
- `idfupd.x` (`idfupd.F90`) is a separate MAPL/ESMF application that reads
  `IDFUPD.rc` and updates restart fields with an incremental digital-filter
  correction (`idfupd.F90`, header comment; `ESMF_ConfigLoadFile ... 'IDFUPD.rc'`).
  *Unverified:* who calls it; nothing in this repository does.
- Parent: the GEOSgcm fixture (`src/Applications/CMakeLists.txt`:
  `esma_add_subdirectories(GEOSgcm_App UMD_Etc CPLFCST_Etc)`); see its
  [AGENTS.md](../../../../AGENTS.md) and
  [docs/ARCHITECTURE.md](../../../../docs/ARCHITECTURE.md) (section 5).
- Children: none. Components that consume the resource keys are in other
  repositories (section 5 links to their documents).

## 2. Directory map (files, by role)

The repository is flat: no source subdirectories. `CMakeLists.txt` decides what
is installed (`*.tmpl` and three resource files to `etc/`; listed scripts to `bin/`).

| Role | Files | Installed to |
|---|---|---|
| Programs | `GEOSgcm.F90` -> `GEOSgcm.x` (links `GEOSgcm_GridComp`, ESMF, OpenMP; jemalloc when found); `idfupd.F90` -> `idfupd.x` | `bin/` |
| Setup scripts (csh; `configure_file(... @ONLY)`) | `gcm_setup`, `gmichem_setup`, `geoschemchem_setup`, `stratchem_setup`, `set_resolution_params.csh` | `bin/` |
| Other setup scripts | `gcm_forecast.setup`, `gcm_emip.setup` (csh), `scm_setup` (bash) | `bin/` (copied verbatim) |
| Job scripts filled by `gcm_setup` | `gcm_run.j`, `gcm_regress.j`, `gcm_post.j`, `gcm_archive.j`, `gcm_quickplot.csh`, `gcm_moveplot.j` (`gcm_setup`, `FILES` list) | `bin/` |
| Job scripts filled elsewhere | `scm_run.j` (by `scm_setup`); `gcm_quickstat.j` (by `quickstat` in fixture `src/Shared/@GMAO_Shared/@GEOS_Util/plots`, which copies it to `gcm_quickstat.tmpl`) | `bin/` |
| Job-script templates | `gcm_plot.tmpl`, `gcm_forecast.tmpl`, `linkbcs.tmpl` | `etc/` (`*.tmpl` glob) |
| Core resource templates | `CAP.rc.tmpl`, `AGCM.rc.tmpl` | `etc/` |
| History templates | `HISTORY.AGCM.rc.tmpl`, `HISTORY.AOGCM.rc.tmpl`, `HISTORY.AOGCM-MOM5.rc.tmpl`, `HISTORY.AOGCM_MITgcm.rc.tmpl`, `HISTORY.DATAATM.rc.tmpl`, `HISTORY.rc.tmpl`, `HISTORY.BENCHMARK.rc.tmpl`, `HISTORY.RADVAL.rc.tmpl`, `HISTORY_*.rc.tmpl` (FP, GEOSIT, MERRA2, MERRA2-DD, MERRA_CFIO, S2S, GMICHEM, GEOSCHEMCHEM, STRATCHEM) | `etc/` |
| Other `*.rc.tmpl` | `IDFUPD.rc.tmpl`, `GCMPROG.rc.tmpl`, `GCMTRAJ.rc.tmpl`, `HISTAENS.rc.tmpl`, `HISTFDDA.rc.tmpl`, `HISTFDDAL.rc.tmpl` | `etc/` |
| Static files | `fvcore_layout.rc` (has `@FV_*` placeholders), `saverst.rc`, `logging.yaml`, `.AGCM_VERSION` (configured: `GEOSgcm-v@CMAKE_PROJECT_VERSION@`) | `etc/` |
| Helper | `construct_extdata_yaml_list.py` | `bin/` |
| Not installed | `gcm_run.j-new_rst_approach`, `gcm_run_benchmark.j`, `run.ogcm.std`, `restarts_ssd_to_exp.j`, `Saverst.pm`, `gcm_*_CPLFCST*.j`, `averageMOM_CPLFCST.j`, `README_CPLFCST`, `README.agcm` | (not in the `programs` list of `CMakeLists.txt`) |

## 3. How setup builds an experiment directory

Entry point: `gcm_setup` (csh), run from `<install>/bin`; it stops when
`GEOSgcm.x` is not beside it (`gcm_setup`, "Build Directory Locations"). The
chemistry variants (section 7.1) are copies with their own prompts; they differ
in step 7.

1. **Prompts** (`gcm_setup`, "Enter Experiment Specific Run Parameters" onward):
   EXPID and optional clone; resolution (`c12` ... `c5760`, stretched `c270` ...
   `c2160`); LM (default 72); microphysics; I/O server; processor type; coupled
   ocean or data-ocean grid; land boundary conditions and model; GOCART mode and
   emissions; heartbeat; History template; HOMDIR; EXPDIR; batch group.
2. **Per-resolution defaults**: `source set_resolution_params.csh <IM> <CLDMICRO> <OGCM> <OCNMODEL>`
   sets the time steps (`DT`, `CONV_DT`, `CHEM_DT`, `SOLAR_DT`, `IRRAD_DT`,
   `OCEAN_DT`), layout `NX`/`NY`, History grid, `NUM_READERS`, `JOB_SGMT`,
   `NUM_SGMT`, `ATMOS_RES`, `POST_NDS`, `USE_SHMEM`, stretched-grid values,
   `CONVPAR_OPTION` (`NONE` at `c2880`, `c5760`) and `RUN_HYDROSTATIC`;
   `BACM_1M` forces `DT`, `CONV_DT`, `CHEM_DT` to 450 (`set_resolution_params.csh`).
3. **Two directories.** HOMDIR gets scripts and resource files; EXPDIR gets
   output and restarts. Both paths must end in the EXPID; default roots are
   remembered in `$HOME/.HOMDIRroot`, `$HOME/.EXPDIRroot`, `$HOME/.GROUProot`
   (`gcm_setup`, "Create Paths for HOME and EXP Directories").
4. **Resource-file snapshot.** `cp -RP $GEOSDIR/etc/* $EXPDIR/RC`, then remove
   `*.tmpl` and `fvcore_layout.rc` there; `GEOSgcm.x` is copied (or linked with
   `--link`) into EXPDIR unless running in a Singularity sandbox (`gcm_setup`,
   "Copy Model Executable and RC Files to Experiment Directory"). `etc/` also
   holds component resource files installed by other repositories (for example
   `GEOS_ChemGridComp.rc`, `GEOS_SurfaceGridComp.rc`, `AMIP/`, `MOM6/`, `CICE6/`;
   `DESTINATION etc` in their `CMakeLists.txt`), so `RC/` is the run-time
   configuration of every component.
5. **MPI-stack settings.** Depending on `@MPI_STACK@` (filled by CMake), a file
   `$HOMDIR/SETENV.commands` is written and later spliced into job scripts at
   the `@SETENVS` line (`gcm_setup`, "Set Recommended MPI Stack Settings").
6. **Text substitution.** A `sed` script `$HOMDIR/sedfile` of lines
   `s?@NAME?value?g` is applied to each file of a list: `gcm_run.j`, `gcm_post.j`,
   `gcm_archive.j`, `gcm_regress.j`, `gcm_plot.tmpl`, `gcm_quickplot.csh`,
   `gcm_moveplot.j`, `gcm_forecast.tmpl`, `gcm_forecast.setup`, `gcm_emip.setup`,
   `CAP.rc.tmpl`, `AGCM.rc.tmpl`, `HISTORY.rc.tmpl`, `logging.yaml`,
   `linkbcs.tmpl`, `fvcore_layout.rc`, plus ocean files (`input.nml`,
   `diag_table`, `MOM_input`, `MOM_override`, `data_table`, `ice_in`,
   `plotocn.j`, ...) when coupled. Each file is taken from `bin/`, else `etc/`,
   else a temporary ocean directory; `HISTORY.rc.tmpl` is replaced by the chosen
   History template (`gcm_setup`, "Create Local Scripts and Resource Files").
7. **Line markers.** A placeholder replaced by `""` keeps the rest of the line;
   by `#` comments it; by `#DELETE` removes it. After substitution, in ten files
   (`AGCM.rc.tmpl`, `gcm_run.j`, `gcm_forecast.tmpl`, `gcm_forecast.setup`,
   `HISTORY.rc.tmpl`, `gcm_regress.j`, `gcm_post.j`, `gcm_emip.setup`,
   `gcm_plot.tmpl`, `linkbcs.tmpl`) an `awk` pass drops every line whose first
   field contains `#DELETE` (`gcm_setup`, "Delete or Enable EXP Configuration
   Variables"). A flag may be glued to the key it guards, for example
   `@MODELATMDYN_INTERNAL_RESTART_FILE:` becomes `DYN_INTERNAL_RESTART_FILE:` or
   `#DELETE...` (`AGCM.rc.tmpl`; `gcm_setup`, `MODELATM`). Before the `awk` pass,
   `gcm_setup` comments restart entries for `H2O`, `MAM`, `CARMA`, `GMICHEM`,
   `STRATCHEM` and `ROUTE` unless the matching flag `LH2O` ... `LROUTE` is `TRUE`
   (`gcm_setup`, "Produce Final script and .rc files"). Only `gcm_setup` handles
   `RUN_ROUTE`/`LROUTE`. `gmichem_setup` loops over component names (`RSNAMES`:
   `TR PCHEM ... HEMCO`); `geoschemchem_setup` and `stratchem_setup` loop over
   flags `LH2O` ... `LGOCART_DATA` (`RSNAMES` in each).
8. **Placement.** EXPDIR gets `post/`, `plot/`, `archive/`, `regress/`,
   `forecasts/`, `linkbcs`, `RC/`, `RESTART/` (coupled only) and `.HOMDIR`. HOMDIR
   keeps `CAP.rc`, `AGCM.rc`, `HISTORY.rc` (renamed from `.tmpl`), `gcm_run.j`,
   `gcm_emip.setup`, `logging.yaml`, `fvcore_layout.rc` and the ocean files.
   `plot.rc` and `post.rc` come from `$GEOSUTIL/post/` (`gcm_setup`, "Finalizing
   Experiment Directories and Chem Registry").
9. **Edits to `RC/`** (`gcm_setup`, "Modify RC Directory for LM and GOCART.data/GOCART Options"):
   when LM /= 72, `/L72/`, `z72`, `_72_` are rewritten in `RC/*.rc` and `RC/*.yaml`;
   `ENABLE_TR` is set true in `GEOS_ChemGridComp.rc`, and `ENABLE_PCHEM` only if
   `RATS_PROVIDER == PCHEM`; `ENABLE_GOCART_DATA` is set false for `AERO_PROVIDER ==
   GOCART2G`; `# GEOSagcm=>` lines of `GEOS_SurfaceGridComp.rc` are uncommented and
   land parameters changed for land choices `CN_CLM40` and `ICA`/`NL3`;
   climatological aerosols add a `.data` suffix to `ACTIVE_INSTANCES_*` in
   `GOCART2G_GridComp.rc`; L72 tuning lines in `DU2G_instance_DU.rc` and
   `SS2G_instance_SS.rc` are switched.
10. **Ocean time steps.** `MOM_override` (MOM6) or `input.nml` (MOM5) and `ice_in`
    (CICE6) get the ocean time step `OCEAN_DT`, which equals the heartbeat in a
    coupled run (`gcm_setup`, "Modify MOM input files to match HEARTBEAT").
11. The user must then place initial restarts and `cap_restart` in EXPDIR
    (`gcm_setup`, "Echo Settings and Messages").

Cloning (`gcm_setup`, "Clone old Experiment") copies the processed files of an
existing experiment and rewrites the EXPID, paths and job names with `sed`; it
does not rerun the prompts.

## 4. Templates and who reads them at run time

Files in `$EXPDIR/RC` and HOMDIR are copied into SCRDIR by `gcm_run.j`: first
`$EXPDIR/RC/*`, then `$HOMDIR/*.rc`, `*.nml`, `*.yaml` with `cp -f`, so a HOMDIR
file overrides an `RC/` file of the same name (`gcm_run.j`, "Move to Scratch
Directory and Copy RC Files from Home Directory").

| Generated file | From | Read by (at run time) |
|---|---|---|
| `CAP.rc` | `CAP.rc.tmpl` | `GEOSgcm.x` main driver: default name `CAP.rc` (`Shared/@MAPL/gridcomps/Cap/CapOptions.F90`; `--cap_rc`, `FargparseCLI.F90`); also `gcm_run.j`, which greps `END_DATE`, `NUM_SGMT`, `FCST_SEGMENT`, `USE_SHMEM`, `JOB_SGMT`, `HEARTBEAT_DT` |
| `AGCM.rc` | `AGCM.rc.tmpl` | the cap loads it as the root configuration (`CAP.rc`, `ROOT_CF: AGCM.rc`; `MAPL_CapGridComp.F90`, `initialize_gc`); components read it through `MAPL_GetResource` and `ESMF_ConfigGetAttribute`; `gcm_run.j` greps and edits the scratch copy |
| `HISTORY.rc` | a History template | the MAPL History component (`CAP.rc`, `HIST_CF`; `MAPL_CapGridComp.F90`, `initialize_gc`); `gcm_run.j` (collection directories, `.monthly:` checkpoint names); `bundleParser.py` (appends tracer-increment tables to `AGCM.rc`); `gcm_setup` (counts collections for I/O server sizing) |
| `extdata.yaml` | built by `construct_extdata_yaml_list.py` from `GEOS_ChemGridComp.rc` | the ExtData component (`ExtDataGridCompNG.F90`, `EXTDATA_YAML_FILE:` default `extdata.yaml`) |
| `ExtData.rc` | `touch` in `gcm_run.j` ("kept ... MAPL expectation ... not actually used") | the cap loads `EXTDATA_CF` (default `ExtData.rc`) |
| `input.nml` | `fvcore_layout.rc` appended by `gcm_run.j` | the FV dynamical core ([FVdycoreCubed](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSsuperdyn_GridComp/@FVdycoreCubed_GridComp/docs/ARCHITECTURE.md); namelists `fv_core_nml`, ...). *Unverified:* the reader; `FV_StateMod.F90` has a comment that `fvcore_layout.rc` is "linked to input.nml" |
| `linkbcs` | `linkbcs.tmpl` | executed by `gcm_run.j` (`./linkbcs`): creates `ExtData/` links, `tile.data`, `tile.bin`/`tile.nc4`, `species.data`, `visdf.dat`, `lai.data`, ... |
| `logging.yaml` | `logging.yaml` | `GEOSgcm.x --logging_config 'logging.yaml'` (`gcm_run.j`) |
| `gcm_post.j`, `gcm_archive.j`, `gcm_plot.tmpl` | same | filled again by `$GEOSUTIL/post/gcmpost.script` (`@COLLECTION`, `@YYYYMM`) (`gcm_post.j`; fixture `src/Shared/@GMAO_Shared/@GEOS_Util/post/gcmpost.script`) |
| `gcm_forecast_*.j` | `gcm_forecast.tmpl` | made by `gcm_forecast.setup` with `&NAME` placeholders (`&DATE`, `&FSEGMENT`, ...) |
| `gcm_regress.j` | same | self-contained regression driver (start/stop, layout, OpenMP tests) |
| `RC/*.rc`, `RC/*.yaml` | installed by component repositories | the component named in the file, copied into SCRDIR |

Templates not processed by `gcm_setup` and not referenced by any script in the
fixture: `GCMPROG.rc.tmpl`, `GCMTRAJ.rc.tmpl`, `HISTAENS.rc.tmpl`,
`HISTFDDA.rc.tmpl`, `HISTFDDAL.rc.tmpl` (fixture-wide search). `IDFUPD.rc.tmpl`
is read only by `idfupd.x` by name `IDFUPD.rc`. A History template other than
the five defaults can be chosen by typing its name at the prompt, because
`gcm_setup` looks it up in `etc/` (`gcm_setup`, "Create Desired HISTORY template").
Three placeholder styles occur: `@NAME` (`gcm_setup`), `&NAME`
(`gcm_forecast.setup`), and `>>>NAME<<<` (`RECORD_*`, `REPLAY_REF_*` and
`>>>ANA0YYYYMMDDHHMN<<<` in `AGCM.rc.tmpl`; `HISTORY.rc.tmpl` and several
`HISTORY_*` templates; `grep '>>>'`). None of `gcm_setup`, the three chemistry
setups, `scm_setup`, `gcm_forecast.setup` or `gcm_emip.setup` fills `>>>NAME<<<`;
`gcm_setup` comments the `RECORD_` lines out.

## 5. Resource keys that select structure or schemes

The key is in the file; the consumer reads it. For the consumer's own
description follow the link. "Set by" is the placeholder or script that fills
the value.

### 5.1 `CAP.rc` (read by the MAPL cap)

| Key | Meaning | Set by |
|---|---|---|
| `ROOT_NAME: GCM`, `ROOT_CF: AGCM.rc`, `HIST_CF: HISTORY.rc` | names of root component and config files (`MAPL_CapGridComp.F90`, `initialize_gc`) | fixed in `CAP.rc.tmpl` |
| `BEG_DATE`, `END_DATE`, `JOB_SGMT` | start and end of the experiment; segment length (`JOB_SGMT` read first, else `JOB_DURATION`) (`MAPL_CapGridComp.F90`, `MAPL_ClockInit`) | `gcm_setup`: `18910301 000000`, `29990302 210000`, `JOB_SGMT` from the resolution |
| `HEARTBEAT_DT` | application time step; the cap writes it as `RUN_DT` into the root and History configs and fails when `AGCM.rc` has a different `RUN_DT` (`MAPL_CapGridComp.F90`, `initialize_gc`) | `@DT` |
| `NUM_SGMT`, `FCST_SEGMENT`, `BEG_REPDATE`, `END_REPDATE` | read by the scripts, not by the cap (no occurrence in `gridcomps/Cap` for `NUM_SGMT`) | `@NUM_SGMT`, literal |
| `USE_EXTDATA2G` | selects the second-generation ExtData (`MAPL_CapGridComp.F90`, default false) | `.TRUE.` |
| `USE_SHMEM` | shared-memory option (`MAPL_CapGridComp.F90`) | `@USE_SHMEM` (1 from `c720` up and in the stretched-grid blocks from `c270`; `set_resolution_params.csh`) |

### 5.2 `AGCM.rc.tmpl`: grid, layout, I/O

| Key | Effect and consumer | Set by |
|---|---|---|
| `NX`, `NY` | process layout; read from the root config by the cap (`MAPL_CapGridComp.F90`, `initialize_gc`) | `@NX`, `@NY` |
| `AGCM_IM/JM/LM`, `AGCM.GRID_TYPE`, `AGCM.GRIDNAME`, `AGCM.IM_WORLD`, `AGCM.LM`, `AGCM.NF`/`AGCM.JM_WORLD` (one of the two kept by `@CUBE_AGCM`/`@LATLON_AGCM`) | atmosphere grid; prefixed keys are read by MAPL grid creation (`Shared/@MAPL/generic/MAPL_Generic.F90`, `MAPL_GridCreate`) | `@AGCM_*`, `@GRID_TYPE` |
| `AGCM.STRETCH_FACTOR`, `AGCM.TARGET_LAT/LON` (line prefix `@CONUS`) | stretched grid | `CONUS` in `set_resolution_params.csh` |
| `OGCM.GRID_TYPE`, `OGCM.GRIDNAME`, `OGCM.IM_WORLD`, `OGCM.JM_WORLD`, `OGCM.LM`, `OGCM.NX/NY` (`@COUPLED`), `OGCM.GRIDSPEC` (`@MOM5`, `@MOM6`, `@MIT`), `OGCM.POLE`/`DATELINE`/`NF` (`@DATAOCEAN`) | ocean grid | `@OGCM_*` |
| `IOSERVER_NODES`, `NUM_BACKEND_PES` | read by `gcm_run.j`, passed as `--nodes_output_server`, `--npes_backend_pernode` (`gcm_run.j`, `IOSERVER_OPTIONS`) | `gcm_setup`, "Architecture Specific Variables" |
| `NUM_READERS`, `NUM_WRITERS`, `WRITE_RESTART_BY_OSERVER`, `READ_RESTART_BY_FACE`, `WRITE_RESTART_BY_FACE` | restart I/O (`MAPL_Generic.F90`, `MAPL_GenericInitialize`: first three; the `*_BY_FACE` keys: *Unverified:* reader; `gcm_run.j` edits them) | `@NUM_READERS`, `@RESTART_BY_OSERVER` |
| `MAPL_ENABLE_BOOTSTRAP` | allows missing restarts to be bootstrapped (`MAPL_Generic.F90`, `MAPL_ESMFStateReadFromFile`) | `@BOOT` = `YES` |
| `*_RESTART_FILE`, `*_CHECKPOINT_FILE`, `*_CHECKPOINT_TYPE` per component | which internal and import states are checkpointed and under what name (read in `MAPL_GenericInitialize`, `MAPL_GenericFinalize`); `gcm_run.j` builds its restart list from them | prefix flags such as `@MODELATM`, `@COUPLED`, `@CICE6` |

### 5.3 `AGCM.rc.tmpl`: schemes and components

| Key | Consumer and document | Set by |
|---|---|---|
| `DYCORE` | `GEOS_SuperdynGridComp.F90`, `SetServices` (default `FV3`) -> [Agcm](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/ARCHITECTURE.md) | `@DYCORE` (`FV3`) |
| `CONVPAR_OPTION` | `GEOS_MoistGridComp.F90`, `SetServices` -> [Moist](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSmoist_GridComp/ARCHITECTURE.md) | `GF`; `NONE` at `c2880`, `c5760` |
| `CLDMICR_OPTION` (lines `@BACM_1M_`, `@GFDL_1M_`, `@MGB2_2M_`) and `GFDL_MP_KLID` | Moist, same file | the prompt; `KLID` from LM |
| `SHALLOW_OPTION` (lines `@SHALLOW_UW`, `@SHALLOW_SHOC`) | Moist; with SHOC also `TURBULENCE_DO_SHOC`, `EDMF_DOMF`, `TURBULENCE_SHC_LENFAC*` (`GEOS_TurbulenceGridComp.F90`) -> [Turbulence](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSturbulence_GridComp/ARCHITECTURE.md) | UW if LM = 72, else SHOC (`gcm_setup`, "Set shallow cumulus option") |
| `USE_RRTMGP_IRRAD`, `USE_RRTMGP_SORAD` (`@RRTMGP_RADIATION`), `USE_RRTMG_IRRAD`, `USE_RRTMG_SORAD` (`@RRTMG_RADIATION`) | `rad_utils.F90`, `choose_irrad_scheme`, `choose_solar_scheme`: RRTMGP if its flag is non-zero, else RRTMG if set, else the third scheme (`USE_CHOU`) -> [Radiation repository](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSradiation_GridComp/docs/ARCHITECTURE.md) | RRTMG if LM = 72, else RRTMGP (`gcm_setup`, "Set Radiation model") |
| `SOLAR_DT`, `IRRAD_DT`, `GF_DT`, `UW_DT`, `GOCART2G_DT`, `GOCART_DT`, `HEMCO_DT`, `ACHEM_DT`, `PCHEM_DT` | component time steps (examples: `GEOS_GF_InterfaceMod.F90`, `GF_DT:`; surface children, `SOLAR_DT:`) | `@SOLAR_DT`, `@IRRAD_DT`, `@CONV_DT`, `@CHEM_DT` |
| `NCAR_NRDG` | `GEOS_GwdGridComp.F90` -> [GWD](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/ARCHITECTURE.md) | `gcm_setup`: 16, or 0 when the ridge file is absent; `gcm_run.j` may reset to 0 |
| `LSM_CHOICE`, `TILING_FILE`, `*_FILE` boundary datasets | `GEOS_SurfaceGridComp.F90` and children -> [Surface](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSsurface_GridComp/ARCHITECTURE.md) | `@LSM_CHOICE`; files created by `linkbcs` |
| `RATS_PROVIDER`, `ANALYSIS_OX_PROVIDER`, `AERO_PROVIDER`, `<species>_PROVIDER` (`CH4`, `CO2`), `*_FRIENDLIES`, `pchem_clim_years`, `OX_RELAXTIME` | `GEOS_ChemGridComp.F90`, `SetServices`; `GEOS_PChemGridComp.F90`; `GEOS_PhysicsGridComp.F90` (`CO2_PROVIDER`) -> [Chemistry repository](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/docs/ARCHITECTURE.md), [GOCART](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/@GOCART/docs/ARCHITECTURE.md) | `PCHEM`, `GOCART2G`, `none`, ...; variants change them (7.1) |
| `OCEAN_NAME`, `SEAICE_NAME`, `USE_CICE_Thermo`, `USE_DATASEA`, `USE_DATASEAICE` (`@COUPLED` lines), `USE_DATA_ATM4OCN`, `USE_WAVES` | `GEOS_GcmGridComp.F90`, `GEOS_OgcmGridComp.F90`, `GEOS_SeaIceGridComp.F90`, `GEOS_OceanGridComp.F90` -> [GEOSgcm_GridComp](../../../Components/@GEOSgcm_GridComp/docs/ARCHITECTURE.md), [Ocean repository](../../../Components/@GEOSgcm_GridComp/GEOSogcm_GridComp/@GEOS_OceanGridComp/docs/ARCHITECTURE.md) | `@OCEAN_NAME`, `@CICE4`, `@CICE6`, `@USE_DATA_ATM4OCN`; `USE_WAVES: 0` literal |

Keys read by components but absent from the template are listed in the
[fixture document](../../../../docs/ARCHITECTURE.md) (section 5).

### 5.4 Menu choices in `gcm_setup` that change structure

- LM = 72 versus other: radiation (RRTMG or RRTMGP), shallow convection (UW or
  SHOC), default microphysics, default land boundary conditions, `RUN_HYDROSTATIC`
  (`gcm_setup`; `set_resolution_params.csh`).
- Ocean: data ocean (`DATAOCEAN`) or coupled (`COUPLED`, with `MOM5`/`MOM6`/`MIT`
  and `CICE4`/`CICE6`; CICE6 requires MOM6; MIT requires `c90`) (`gcm_setup`,
  "Check for COUPLED Ocean"). Data atmosphere: `MODELATM` becomes `#DELETE`; History
  template `HISTORY.DATAATM.rc.tmpl`.
- Land boundary conditions (`ICA`, `NL3`, `v12`, `v14`) set `GWD_IN_BCS`, `LSM_BCS`,
  `EMIP_*`; land model 2 enables `CATCHCN` entries (`HIST_CATCHCN`, `GCMRUN_CATCHCN`).
- Aerosols: climatological or actual; emissions `AMIP` or `OPS` choose the
  `species.data` link and `pchem_clim_years` (1 or 48), `OX_RELAXTIME`.

## 6. The run script: job flow of `gcm_run.j`

csh script generated into HOMDIR. Sections are the banner titles.

1. **Batch header and environment**: scheduler lines from `@BATCH_*`; source `$GEOSBIN/g5_modules`.
2. **Experiment sub-directories**: create `$EXPDIR/restarts`, `holding`,
   `archive`, `post`, `plot`. SCRDIR is `$EXPDIR/scratch` (or a job-specific
   directory under `$TSE_TMPDIR`, linked from EXPDIR) and is emptied first.
3. **Run parameters**: `NX`, `NY`, `AGCM_IM/JM/LM`, `OGCM_IM/JM` are grepped
   from `$HOMDIR/AGCM.rc`; in a coupled run `NX`, `NY` come from `OGCM.NX`,
   `OGCM.NY`. I/O-server sizing and a check that allotted cores cover the request.
4. **Copy configuration** into SCRDIR (section 4); append `fvcore_layout.rc` to
   `input.nml`; read `END_DATE`, `NUM_SGMT`, `FCST_SEGMENT`, `USE_SHMEM` from `CAP.rc`.
5. **History directories**: one `$EXPDIR/<collection>` and `holding/<collection>`
   per collection named in `HISTORY.rc`.
6. **Boundary datasets and executable**: set `BCSDIR`, `BCRSLV`, `BCTAG`,
   `EMISSIONS`; copy `GEOSgcm.x` into SCRDIR (or use the installed one under
   Singularity).
7. **Restarts in**: names come from the non-comment `RESTART_FILE` lines of
   `AGCM.rc` (excluding `VEGDYN`), `+`/`-` bootstrap prefixes stripped; files are
   copied from `$EXPDIR`. If no archive exists for the start date, the initial
   restarts are copied to `$EXPDIR/restarts` as
   `<EXPID>.<name>.e<date>_<hh>z.<version>.<bcs>` and tarred. Face-split restarts
   are detected and `READ_RESTART_BY_FACE`, `NUM_READERS` adjusted in the scratch `AGCM.rc`.
8. **Segment loop** `while counter <= NUM_SGMT`:
   a. refresh `CAP.rc` from HOMDIR; read current time from `cap_restart`;
   b. compute the segment end = current time + `JOB_SGMT` (with `tick`), clamp to
      `END_DATE`; for non-Reynolds SST also clamp to the year boundary by editing
      the scratch `CAP.rc` `END_DATE`;
   c. prepare inputs: emission and wave-model files, coupled time-step sync,
      `/L72/` -> `/L<LM>/` in `*.yaml`, `construct_extdata_yaml_list.py`, RRTMG ->
      RRTMGP in `*_instance*.rc`, `./linkbcs`, tile and MERRA2OX checks,
      `@SETENVS`, `bundleParser.py`, replay links, MITgcm set-up when `@MIT`;
   d. run `$RUN_CMD $TOTAL_PES GEOSgcm.x [I/O-server options] --logging_config 'logging.yaml'`;
      fail on a non-zero status or a missing `EGRESS` file (written by the cap at finalize);
   e. **restarts out**: intermediate `*_checkpoint.*` go to `$EXPDIR/restarts`;
      each final `*_checkpoint` is renamed to
      `<EXPID>.<name>.e<date>_<hh>z.<version>.<bcs>.<ext>`, copied to
      `$EXPDIR/restarts`, then renamed to `<name>_rst` for the next segment; tarred
      when `FCST_SEGMENT` is 00000000;
   f. move History output `*.<collection>.*` to `$EXPDIR/holding/<collection>`,
      MOM/CICE output to `MOM_Output`, `CICE_Output`;
   g. run `$GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles`; with
      `FCST_SEGMENT` /= 0 inside `BEG_REPDATE`..`END_REPDATE`, call `forecasts/gcm_forecast.setup`;
   h. continue while the date in `cap_restart` is before the `END_DATE` date.
9. **After the loop**: copy the final `*_rst` and `cap_restart` back to `$EXPDIR`
   (replacing the old ones), copy `RESTART/` (coupled), and resubmit
   `$HOMDIR/gcm_run.j` with `@BATCH_CMD` when the date is before `END_DATE`.

`GCMEMIP` (set by `gcm_emip.setup`, which makes `gcm_run.j<YYYYMM>` copies) switches the
restart directory to `$EXPDIR/restarts/<YYYYMM>` and adds a site-specific regridding block.

## 7. Variants and companion scripts

### 7.1 Chemistry setup variants
`gmichem_setup`, `geoschemchem_setup` and `stratchem_setup` are copies of
`gcm_setup` with other chemistry prompts and History defaults
(`HISTORY_GMICHEM`, `HISTORY_GEOSCHEMCHEM`, `HISTORY_STRATCHEM`). They set
`AERO_PROVIDER` (`gmichem_setup`: none, `GOCART2G`, `GMICHEM`, `CARMA`),
`RATS_PROVIDER` (`GMICHEM`) and `OANA_PROVIDER` (`GMICHEM`, `STRATCHEM`), and
turn `ENABLE_GMICHEM`, `ENABLE_STRATCHEM`, `ENABLE_GEOSCHEM`, `ENABLE_ACHEM`,
`ENABLE_HEMCO`, `ENABLE_GOCART` on or off in `RC/GEOS_ChemGridComp.rc`.
`diff gcm_setup <variant>` differs in about 575, 256 and 181 lines. Each also
contains the Open MPI `coll ^han` setting (one occurrence in each of the four;
none in `scm_setup`).

### 7.2 Other scripts
- `scm_setup` (bash): single-column-model experiments, with its own `sedfile.SCM`; fills `scm_run.j`.
- `gcm_regress.j`: regression tests selected with `--startstop`, `--layout`, `--openmp`.
- `gcm_post.j`, `gcm_archive.j`, `gcm_plot.tmpl`, `gcm_moveplot.j`, `gcm_quickplot.csh`:
  post-processing and plots via `gcmpost.script` (`GEOS_Util`); `gcm_forecast.setup`
  and `.tmpl`: replay-forecast jobs that read `REPLAY_*` from `AGCM.rc`.

## 8. Interfaces and dependencies

- **MAPL cap**: `GEOSgcm.x` takes `--npes_model`, `--nodes_output_server`,
  `--oserver_type`, `--npes_backend_pernode`, `--logging_config`, `--cap_rc`,
  `--egress_file` (`Shared/@MAPL/gridcomps/Cap/FargparseCLI.F90`). The cap reads
  `CAP.rc`; the script reads the same file with `grep`.
- **GEOS_Util** (`src/Shared/@GMAO_Shared/@GEOS_Util/post`): `gcmpost.script`,
  `plot.rc`, `post.rc`, `binarytile.x`, `rs_numtiles.x`, `tick`.
- **GMAO_Shared/GMAO_etc**: `stripname`, `RmShmKeys_sshmpi.csh`.
- **GEOSchem_GridComp**: `bundleParser.py` (`Shared/Chem_Shared`) and the
  `GEOS_ChemGridComp.rc` that `construct_extdata_yaml_list.py` parses for
  `ENABLE_*` keys (it maps enabled components to ExtData YAML file names listed
  in the script).
- **ESMA_env**: `g5_modules`, `SITE.rc`. *Inferred:* `SITE.rc` is installed to
  `etc/` (`gcm_setup` reads `$ETCDIR/SITE.rc`).
- **Ocean repositories** install `MOM6/`, `CICE6/`, `MOM5/` input directories under
  `etc/` that `gcm_setup` copies by ocean grid; `coupled_diagnostics/` is from `GEOS_Util`.

## 9. Build and CI

`CMakeLists.txt`: two executables, the install lists of section 2, and
`configure_file(... @ONLY)` for the setup scripts and `.AGCM_VERSION`. No test
target. CI workflows are listed in [AGENTS.md](../AGENTS.md).

## 10. Known traps

1. **Placeholder order.** Substitution is textual `sed` without word boundaries,
   in the order of `sedfile`. `@COUPLEDIR` is listed before `@COUPLED`, and
   `@COUPLED` is a prefix of it (`gcm_setup`, "Create Local Scripts and Resource
   Files"). *Inferred:* the order matters for this pair.
2. **Which file is read.** At run time the model reads the scratch copies. Edits
   in `gcm_run.j` (`AGCM.rc`: `NUM_READERS`, `READ_RESTART_BY_FACE`,
   `OGCM_RUN_DT`, `NCAR_NRDG`; `CAP.rc`: `END_DATE`) apply to SCRDIR only.
3. **Override order.** HOMDIR `*.rc` override `RC/*.rc` of the same name.
4. **The template is not re-read.** Changing a template in the source tree
   affects only experiments made afterwards.
5. **Restart list.** Restarts are found by grepping `RESTART_FILE` in `AGCM.rc`;
   a restart not named there is not copied in.
6. **Coupled layout.** Job size is computed from `OGCM.NX`, `OGCM.NY` (section 6,
   step 3). *Inferred:* the atmosphere layout product equals the ocean one in the
   tabulated cases (`set_resolution_params.csh`; `gcm_setup`, MOM6 table).
7. **MERRA2OX stop.** With `pchem_clim_years` 48 a start month after `202608`
   makes `gcm_run.j` exit with status 8 (`gcm_run.j`, "Check for MERRA2OX Consistency").
8. **Heartbeat in several places**: `CAP.rc` `HEARTBEAT_DT`; (coupled) `OGCM_RUN_DT`
   in `AGCM.rc`; the ocean file, which is `MOM_override` for MOM6 and `input.nml`
   for MOM5 in `gcm_setup` (but `MOM_override` for both in `gcm_run.j`; Observation
   9); `ice_in` (CICE6). `gcm_run.j` re-syncs them from `CAP.rc` each segment.
9. **Site data.** Boundary-data and analysis paths for NCCS, NAS and others are
   in `gcm_setup`, "Architecture Specific Variables".
10. **Optional blocks are off by default.** Replay and IAU keys (`REPLAY_*`,
    `MKIAU_*`, `ASSIMILATION_CYCLE`, `CORRECTOR_DURATION`, `AINC_FILE`,
    `PRECIP_FILE*`; consumers: [GEOSagcm_GridComp](../../../Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/ARCHITECTURE.md)),
    bias-correction keys (`AGCM_INTERNAL_*`, `AGCM_ALPHA/BETA` under `@FORCEGCM`,
    `AGCM_IMPORT_RESTART_FILE` under `@FORCEDAS`) and `DUAL_OCEAN` are comments
    or deleted lines in the generated `AGCM.rc`; replay is switched on by editing it.

## 11. Observations (noted, not fixed)

1. `gcm_run.j` calls `./strip GWD_GridComp.rc` twice; no file named `strip` was
   found in the fixture (`gcm_regress.j` writes one at run time). `gcm_run.j` sets
   `OMP_NUM_THREADS 1` right before the `if ($OMP_NUM_THREADS > 1)` guard
   ("Set OMP_NUM_THREADS"), so the guarded call cannot run; the second call
   ("Reset OpenMP Threads to 1", in the segment loop) is unguarded.
2. `INTERPOLATE_SST` is set in `gcm_setup` (from `HIST_IM` versus `OGCM_IM`) and
   substituted as `@INTERPOLATE_SST`, but `AGCM.rc.tmpl` has no such placeholder,
   only comments. Other substitutions with no placeholder in the files `gcm_setup`
   processes: `@BCSRES`, `@TILEDATA`, `@TILEBIN`, `@PLOT_N` (named only in the
   setup scripts; `grep` over the repository's top-level files), and
   `@RES_DATELINE`, `@FVCUBED` (named additionally only in `gcm_run_benchmark.j`
   and two `gcm_run_CPLFCST*.j` scripts, which `gcm_setup` does not process).
3. `set_resolution_params.csh` has a `c4320` block; the `gcm_setup` menu does not offer it.
4. The vertical-resolution prompt says convection changes to `GF2020` for LM 91,
   137, 181; `gcm_setup` sets `CONVPAR_OPTION` to `GF` for every LM. What does
   `GF2020` refer to? *Inferred:* the key `USE_GF2020` (`GEOS_GF_InterfaceMod.F90`,
   `GF_Initialize`: read with default 0 when LM = 72, 1 otherwise).
5. `gcm_setup` reads `RUN_ROUTE` from `RC/GEOS_SurfaceGridComp.rc` (to decide the
   `ROUTE` restart entries) before it uncomments the `# GEOSagcm=>` lines; the
   installed file has only commented `RUN_ROUTE` lines. Is that order intended?
6. In `gcm_run.j`, `rc` is set once, to 0, after `EGRESS` is found (the `else`
   branch exits with status 10), and a failed `GEOSgcm.x` exits earlier. *Inferred:*
   the later `$rc != 0` branches cannot be reached.
7. `saverst.rc` (installed to `etc/` and copied into `RC/`) contains `@SAVERSTHRS`,
   which no `gcm_setup` substitution fills; `Saverst.pm` (not installed) is the
   only other file that names it. What reads it?
8. No script in the fixture names the `GCMPROG`, `GCMTRAJ`, `HISTAENS`, `HISTFDDA`,
   `HISTFDDAL` templates (`ChangeLog-PreJason.txt` at the fixture root names
   `GCMPROG.rc.tmpl`). Are they used by other fixtures?
9. For MOM5, `gcm_setup` edits `input.nml` for the ocean time step ("Modify MOM
   input files to match HEARTBEAT"); `gcm_run.j` edits `MOM_override` under `@MOM5`
   (`dt_cpld`, `dt_atmos`), but copies `MOM_override` into scratch only under
   `@MOM6`. Which file is the MOM5 edit in `gcm_run.j` meant to act on?
10. `.github/workflows/push-to-v12.yml` runs on pushes to
    `feature/sdrabenh/gcm_v12` and opens a pull request to `release/MAPL-v3`.

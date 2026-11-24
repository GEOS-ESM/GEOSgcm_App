# Component Plug-ins

**Status:** pilot implemented (GOCART). This page records the design we
agreed on, what the pilot actually did, and the follow-on work.

## The problem

Today, a gridded component ("gridcomp") that needs a new setup question or
new setup-time logic requires editing gcmpy, a different repository from
the component itself. Component authors cannot add or change their own
setup behaviour without a GEOSgcm_App change, and gcmpy accumulates
per-component special cases (`setup_tools/gocart.py`,
`SetupGCM.config_gocartGridComp()`, `SetupGCM.config_chemGridComp()`, and
so on) that only make sense to someone who already knows which component
they belong to.

## The target shape

A gridded component owns a `gcmpy/` directory (name intentionally the same
as the tool) with:

```
<component>/gcmpy/
  component.yaml   # manifest: name, api_version, requires, file paths, class
  questions.yaml   # this component's setup questions (same format as
                    # gcmpy/setup_tools/questions/*.yaml)
  catalog.yaml     # this component's own defaults/resolution table
  component.py     # a GridCompBase subclass, only for what can't be data
```

gcmpy itself becomes unaware of any particular component: it loads
whatever manifests it finds, in a generic way, and calls their methods.
Resource-file templates already work this way (a component's `.rc.tmpl`
lives with the component); this extends the same idea to questions,
defaults and setup-time file edits.

## Framework (`gcmpy.components.base`)

- **`GridCompBase`**: the methods a component may override --
  `defaults()`, `validate()`, `post_rc_copy()`, `configure()`,
  `mpi_environment()`, and `run_segment()` (reserved; see
  [Run-time hooks](#run-time-hooks-deferred)). Every method defaults to a
  no-op, so a component with nothing to customize needs no Python at all.
- **`ComponentManifest`**: reads one `component.yaml`; rejects a missing
  `name` or an `api_version` gcmpy does not support.
- **`discover_components(components_dir)`**: scans
  `<components_dir>/*/component.yaml`, sorted by name; raises on a
  duplicate name. Default `components_dir` is wherever `base.py` itself
  lives (`gcmpy/components/`).
- **`insert_question_blocks(core_blocks, components)`**: every
  component's question block is asked after all of gcmpy's own ("core")
  question files and before the last one (`directory_setup`, which never
  depends on a component's answers). `requires` is a *validation*, not a
  positioning rule: a component can declare the `expConfig` keys it reads
  that some other question block must set first (GOCART:
  `requires: [OCEAN_data_atmosphere]`), and it is an error if nothing
  defines that key -- but meeting `requires` never moves a component's
  questions earlier than "right before the last block". This reproduces
  the question order gcmpy already had with GOCART hardcoded in, which
  the pilot's tests pin down.

Discovery is a directory scan at **setup run time**, not an index built at
CMake configure time. This is deliberate: it makes no difference to the
loader whether a `<Name>/` directory under `gcmpy/components/` was checked
into this repository or copied in at install time from a different
repository's CMake rules (see [Phase 2](#phase-2-components-from-their-own-repositories)) --
the same code handles both, and the pilot could use the simpler one
without the loader changing later.

## Ownership of shared files

Most files belong to exactly one component. Some do not: for example,
`GEOS_ChemGridComp.rc` carries `ENABLE_PCHEM`/`ENABLE_TR` (not
component-specific) alongside `ENABLE_GOCART_DATA`. **Default ownership is
the central, shared `setup_tools`/`run_tools` code** -- a file only moves
into a component's `configure()` when that component is its one real
owner. A file with more than one owner is not assigned to whichever
component got to it first; it is documented as shared and left alone. The
GOCART pilot hit exactly this case (`SetupGCM.config_chemGridComp()`) and
left it in place; see the Developer Guide's
[Component plug-ins](Developer-Guide.md#component-plug-ins) section.

## The pilot: GOCART

`gcmpy/components/GOCART/` moved:

- the two questions (`GOCART_aerosols`, `GOCART_emission`) out of
  `setup_tools/questions/gocart.yaml`,
- the catalog out of `setup_tools/catalogs/gocart_catalog.yaml`,
- `get_gocart_config()` (now `GOCARTComponent.defaults()`),
- `adjust_tuning_coefficients()` (now `GOCARTComponent.post_rc_copy()`,
  called at the same point in `RC_setup()`),
- `SetupGCM.config_gocartGridComp()`'s body (now
  `GOCARTComponent.configure()`; the `SetupGCM` method became a one-line
  wrapper so `gcm_setup.py`'s call sequence did not change).

`self.gocart` in `SetupGCM` is unchanged in shape and content: templates,
`run_yaml_fields.yaml`, and the other `config_*` methods that read it did
not need to change. This was verified by:

- the existing `test_template_vars.py`, `test_csh_template_aliases.py` and
  `test_config_key_coverage.py`, which build `self.gocart` through the
  full `SetupGCM` pipeline and continued to pass unchanged, and
- new tests in `gcmpy_tests/gcmpy/components/` covering the framework
  itself and every case the old GOCART functions handled.

One pre-existing bug was found while porting `configure()` (three
space-separated `CA.*` instances on one `ACTIVE_INSTANCES_CA:` line, but
the replacement regex only ever matches the first) and was *kept*, not
fixed, so the move is behavior-preserving; it is called out in a code
comment and in the matching test. Fixing it is a separate, ordinary change.

## Phase 2: components from their own repositories

The pilot keeps GOCART's `gcmpy/` directory as an overlay inside
`GEOSgcm_App` rather than inside the GOCART repository, because that needs
no coordination with GOCART's maintainers and could be done immediately.
The target shape has each component's `gcmpy/` directory in its own
repository. For components we do not control (HEMCO, MOM, geos-chem, ...),
that means a GEOS branch carrying the addition, kept in sync with whatever
tag `components.yaml` pins -- acceptable per earlier discussion, but real
ongoing maintenance, so do it where it earns its keep, not everywhere at
once.

Once a component's `gcmpy/` directory lives in its own repository, it
needs a CMake rule that copies it into the installed
`bin/gcmpy/gcmpy/components/<Name>/` tree -- the same destination the
pilot's `gcmpy/components/GOCART/CMakeLists.txt` installs to today. A
`gcmpy_register_component(NAME DIR)` CMake function (to be added to
`ESMA_cmake` or to this repository) can do that `install(...)` call and,
as a build-time nicety, fail fast on a duplicate name using a global
property -- the runtime check in `discover_components()` stays regardless,
since it is the one place both paths are guaranteed to go through.

Suggested order for migrating more components, each as its own change:

1. Write `gcmpy_register_component()` once the first real cross-repo case
   exists (do not build it speculatively).
2. Pick the next component by how much hardcoded, component-specific logic
   it has in gcmpy today (land and the chemistry/aerosol components are
   the next largest after GOCART).
3. For each: move its questions/catalog/class, verify with a before/after
   comparison of a generated experiment directory across a representative
   matrix of its answers (not just unit tests -- see the GOCART pilot's
   tests for the pattern), update the Developer Guide's table, remove the
   old hardcoded path once migrated.
4. Only then take on a component outside our control, starting with a GEOS
   branch.

Some logic will not cleanly belong to one component. Expect this, and use
the "shared" rule above rather than forcing an owner.

## Run-time hooks (deferred)

Only one program (`GEOSgcm.x`) is actually compiled and run; "run-time"
here means `gcmpy`'s own Python driver (`gcm_run.py`), which does a
sequence of steps around each model segment, some of which are specific to
particular components today: heartbeat sync into ocean/sea-ice inputs and
moving their output (ocean), renaming RRTMGP instance files and staging
AMIP emissions (radiation/GOCART), WaveWatch3 date injection (waves),
open-water tile validation (land/surface), the GWD ridge restart (moist).

Move as much of this into setup as possible -- most of it is deciding
*what* to do once, which setup can already do, not something that must be
re-decided every segment. `GridCompBase.run_segment(job_context,
run_config)` is reserved for whatever turns out not to be movable, called
from `gcm_run.py` once a first component actually needs it. Not wired in
yet: do not add call sites for it speculatively.

## Open questions for the next component

- Does `requires` ever need to depend on another *component's* presence,
  not just an `expConfig` key? (Not needed by GOCART; likely needed once a
  component's questions are conditional on whether another component is
  even built.)
- Should `gcmpy_register_component()` also install a component's resource
  templates (today handled separately, already working), unifying the two
  mechanisms? Worth revisiting once two or three components have made the
  move, not before.

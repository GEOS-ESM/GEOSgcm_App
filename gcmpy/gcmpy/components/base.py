"""
gcmpy.components.base

Framework for gridded-component ("gridcomp") plug-ins to gcmpy.

A component contributes some or all of:

- setup questions (``questions.yaml``, same format as
  ``setup_tools/questions/*.yaml``),
- resolution/configuration defaults (``catalog.yaml``, read by the
  component's own Python class -- gcmpy does not interpret its contents),
- a Python class (``component.py``) for anything that cannot be expressed
  as data.

A component needs no Python at all: an empty ``component.yaml`` with no
``class:`` entry gets the no-op ``GridCompBase`` defaults.

Discovery is a directory scan, not an index built at CMake time: every
``<components_dir>/<Name>/component.yaml`` is a component, whether that
directory was checked into this repository (today: an overlay, e.g.
``GOCART/``, for a component whose own repository does not yet carry a
``gcmpy/`` directory) or copied in at install time from another
repository's CMake rules (the intended end state -- see
docs/gcmpy/Component-Plugins.md). The loader does not need to know which.

Ownership that does not belong to exactly one component (a file that more
than one component's behaviour depends on, or that predates this scheme)
stays in the central ``setup_tools``/``run_tools`` code -- the implicit
"shared" owner. Prefer giving such logic to the component that most owns
the *file being edited*, and leave genuinely shared files centralized
rather than picking an arbitrary component to own them.
"""

from __future__ import annotations

import importlib
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml

#: Bumped on any incompatible change to this module's public API
#: (method names/signatures on GridCompBase, or the component.yaml schema).
#: A component declares the version it was written against in
#: ``component.yaml``; a mismatch is a loud error, not a silent skip.
API_VERSION = 1


class GridCompBase:
    """Base class for a gridded component's gcmpy integration.

    Every method has a default that does nothing (or returns an empty
    mapping), so a component needs to override only what it actually
    customizes. All methods are called with the component's own manifest
    available as ``self.manifest``.
    """

    manifest: "ComponentManifest"

    def defaults(self, expConfig: dict, catalog: dict) -> dict:
        """Return this component's flat configuration dict (what
        ``self.<name>`` holds in ``SetupGCM``), built from the setup
        answers and this component's own ``catalog.yaml`` (already loaded
        and passed in as ``catalog``).

        The default implementation returns ``catalog.get('defaults', {})``
        unchanged -- enough for a component whose configuration is static.
        Override for anything that depends on ``expConfig`` or on more than
        one catalog section.
        """
        return dict(catalog.get('defaults', {}))

    def validate(self, expConfig: dict) -> None:
        """Raise ``ValueError`` on an invalid combination of answers.
        Default: no-op.
        """
        return None

    def post_rc_copy(self, exp_dir: Path, expConfig: dict, component_config: dict) -> None:
        """Edit this component's own files immediately after ``RC_setup()``
        copies ``install/etc`` into ``<exp_dir>/RC`` (and the executable is
        staged), before any other setup step. Default: no-op.
        """
        return None

    def configure(self, exp_dir: Path, expConfig: dict, component_config: dict) -> None:
        """Edit this component's own RC files in the experiment directory,
        at the point in ``SetupGCM`` where the old per-component
        ``config_*GridComp`` methods ran. Default: no-op.
        """
        return None

    def mpi_environment(self, expConfig: dict, component_config: dict) -> dict:
        """Extra ``mpi_environment`` entries this component needs, merged
        in by ``SetupGCM.mpistacksettings()``. Default: ``{}``.
        """
        return {}

    def run_segment(self, job_context: dict, run_config: dict) -> None:
        """Per-segment run-time hook, called from ``gcm_run.py``. Default:
        no-op. Prefer moving behaviour into setup instead of here; use this
        only for what genuinely cannot be decided once at setup time (see
        docs/gcmpy/Component-Plugins.md). Not yet wired into
        ``gcm_run.py`` -- reserved for the first component that needs it.
        """
        return None


@dataclass
class ComponentManifest:
    name: str
    directory: Path
    api_version: int
    requires: list = field(default_factory=list)
    questions_file: Any = None
    catalog_file: Any = None
    class_path: Any = None

    @classmethod
    def from_yaml(cls, component_yaml: Path) -> "ComponentManifest":
        data = yaml.safe_load(component_yaml.read_text()) or {}
        directory = component_yaml.parent

        name = data.get('name')
        if not name or not isinstance(name, str):
            raise ValueError(f"{component_yaml}: 'name' is required and must be a string")

        api_version = data.get('api_version')
        if api_version != API_VERSION:
            raise ValueError(
                f"{component_yaml}: api_version {api_version!r} is not supported by "
                f"this gcmpy (API_VERSION={API_VERSION}). Update the component, or "
                "gcmpy, so the two agree."
            )

        def _optional_path(key):
            value = data.get(key)
            return (directory / value) if value else None

        return cls(
            name=name,
            directory=directory,
            api_version=api_version,
            requires=list(data.get('requires') or []),
            questions_file=_optional_path('questions'),
            catalog_file=_optional_path('catalog'),
            class_path=data.get('class'),
        )

    def load_catalog(self) -> dict:
        if self.catalog_file is None:
            return {}
        return yaml.safe_load(self.catalog_file.read_text()) or {}

    def load_questions(self) -> dict:
        if self.questions_file is None:
            return {}
        return yaml.safe_load(self.questions_file.read_text()) or {}

    def load_instance(self) -> GridCompBase:
        if not self.class_path:
            instance = GridCompBase()
        else:
            module_name, _, class_name = self.class_path.rpartition('.')
            if not module_name or not class_name:
                raise ValueError(
                    f"{self.directory}/component.yaml: 'class' must be "
                    f"'module.path.ClassName', got {self.class_path!r}"
                )
            module = importlib.import_module(module_name)
            instance = getattr(module, class_name)()
        instance.manifest = self
        return instance


def discover_components(components_dir: Path = None) -> list:
    """Find every ``component.yaml`` directly under ``<name>/`` directories
    of ``components_dir`` (default: the directory this module lives in)
    and return their manifests, sorted by name.

    Raises ``ValueError`` on a duplicate component name -- discovery is the
    single place that rule is enforced, regardless of whether the
    duplicate came from this repository or from another one's install
    rules.
    """
    if components_dir is None:
        components_dir = Path(__file__).parent

    manifests = []
    seen = {}
    for component_yaml in sorted(components_dir.glob('*/component.yaml')):
        manifest = ComponentManifest.from_yaml(component_yaml)
        if manifest.name in seen:
            raise ValueError(
                f"Duplicate gcmpy component name {manifest.name!r}: "
                f"{seen[manifest.name]} and {component_yaml}"
            )
        seen[manifest.name] = component_yaml
        manifests.append(manifest)

    return sorted(manifests, key=lambda m: m.name)


def insert_question_blocks(core_blocks: list, components: list) -> list:
    """Splice each component's question block into a list of
    ``(name, questions_dict)`` core blocks, respecting ``requires``.

    Every component's questions are inserted right before the last core
    block (by convention, ``directory_setup``, which has no dependency on
    any component's answers and must stay last), in the order
    ``components`` lists them -- this matches where the previously
    hardcoded, central question list asked component-specific questions
    (after every core question group, before directory setup).

    ``requires`` is a validation, not a positioning rule: it is an error if
    a key a component requires is not defined by an earlier block (a core
    block, or an earlier component already inserted), but meeting it never
    moves the component earlier than "before the last block".
    """
    blocks = list(core_blocks)
    tail = [blocks.pop()] if blocks else []

    defined_keys = set()
    for _, block_questions in blocks:
        defined_keys.update(block_questions)

    for manifest in components:
        questions = manifest.load_questions()

        missing = [key for key in manifest.requires if key not in defined_keys]
        if missing:
            raise ValueError(
                f"Component {manifest.name!r} requires {missing!r}, but no "
                "question block asked earlier defines it"
            )

        if not questions:
            continue
        blocks.append((manifest.name, questions))
        defined_keys.update(questions)

    return blocks + tail

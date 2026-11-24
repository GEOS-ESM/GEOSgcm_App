"""
Component plug-in framework (gcmpy.components.base).

Covers discovery (including the real, installed GOCART pilot), the
duplicate-name and api_version error paths, and the `requires`-based
question-ordering rule -- see gcmpy.components.base's module docstring and
docs/gcmpy/Component-Plugins.md for the design these enforce.
"""

import pytest

from gcmpy.components.base import (
    API_VERSION,
    ComponentManifest,
    GridCompBase,
    discover_components,
    insert_question_blocks,
)


def _write_component(tmp_path, name, api_version=API_VERSION, requires=None,
                      questions=None, catalog=None, class_path=None):
    directory = tmp_path / name
    directory.mkdir()

    manifest = {'name': name, 'api_version': api_version}
    if requires is not None:
        manifest['requires'] = requires
    if questions is not None:
        (directory / 'questions.yaml').write_text(questions)
        manifest['questions'] = 'questions.yaml'
    if catalog is not None:
        (directory / 'catalog.yaml').write_text(catalog)
        manifest['catalog'] = 'catalog.yaml'
    if class_path is not None:
        manifest['class'] = class_path

    import yaml
    (directory / 'component.yaml').write_text(yaml.safe_dump(manifest))
    return directory


def test_discover_components_finds_the_gocart_pilot():
    manifests = discover_components()
    names = [m.name for m in manifests]
    assert 'GOCART' in names
    assert names == sorted(names)


def test_gocart_manifest_loads_catalog_questions_and_class():
    manifest = next(m for m in discover_components() if m.name == 'GOCART')
    assert manifest.requires == ['OCEAN_data_atmosphere']

    questions = manifest.load_questions()
    assert 'GOCART_aerosols' in questions
    assert 'GOCART_emission' in questions

    catalog = manifest.load_catalog()
    assert 'defaults' in catalog and 'aerosol' in catalog

    instance = manifest.load_instance()
    assert instance.manifest is manifest
    assert type(instance).__name__ == 'GOCARTComponent'


def test_component_with_no_class_gets_the_noop_base(tmp_path):
    _write_component(tmp_path, 'Plain')
    manifest, = discover_components(tmp_path)

    instance = manifest.load_instance()

    assert type(instance) is GridCompBase
    assert instance.defaults({}, {'defaults': {'a': 1}}) == {'a': 1}
    assert instance.validate({}) is None
    assert instance.mpi_environment({}, {}) == {}


def test_discover_components_rejects_duplicate_names(tmp_path):
    _write_component(tmp_path, 'One')
    (tmp_path / 'Two').mkdir()
    import yaml
    (tmp_path / 'Two' / 'component.yaml').write_text(
        yaml.safe_dump({'name': 'One', 'api_version': API_VERSION})
    )

    with pytest.raises(ValueError, match='Duplicate'):
        discover_components(tmp_path)


def test_manifest_rejects_unsupported_api_version(tmp_path):
    directory = _write_component(tmp_path, 'Old', api_version=API_VERSION + 1)

    with pytest.raises(ValueError, match='api_version'):
        ComponentManifest.from_yaml(directory / 'component.yaml')


def test_manifest_requires_a_name(tmp_path):
    directory = tmp_path / 'Nameless'
    directory.mkdir()
    (directory / 'component.yaml').write_text('api_version: 1\n')

    with pytest.raises(ValueError, match="'name'"):
        ComponentManifest.from_yaml(directory / 'component.yaml')


def _manifest(tmp_path, name, requires=None, questions=None):
    directory = _write_component(tmp_path, name, requires=requires, questions=questions)
    return ComponentManifest.from_yaml(directory / 'component.yaml')


def test_insert_question_blocks_with_no_requires_goes_before_the_last_block(tmp_path):
    core = [('a', {'A': {}}), ('z', {'Z': {}})]
    comp = _manifest(tmp_path, 'Comp', questions='X:\n  type: text\n')

    blocks = insert_question_blocks(core, [comp])

    assert [name for name, _ in blocks] == ['a', 'Comp', 'z']


def test_insert_question_blocks_always_goes_before_the_last_block_even_with_requires(tmp_path):
    core = [('a', {'A': {}}), ('b', {'B': {}}), ('z', {'Z': {}})]
    comp = _manifest(tmp_path, 'Comp', requires=['B'], questions='X:\n  type: text\n')

    blocks = insert_question_blocks(core, [comp])

    assert [name for name, _ in blocks] == ['a', 'b', 'Comp', 'z']


def test_insert_question_blocks_accepts_requires_met_by_any_earlier_block(tmp_path):
    core = [('a', {'A': {}}), ('b', {'B': {}}), ('z', {'Z': {}})]
    comp = _manifest(tmp_path, 'Comp', requires=['A', 'B'], questions='X:\n  type: text\n')

    blocks = insert_question_blocks(core, [comp])

    assert [name for name, _ in blocks] == ['a', 'b', 'Comp', 'z']


def test_insert_question_blocks_raises_on_unmet_requires(tmp_path):
    core = [('a', {'A': {}})]
    comp = _manifest(tmp_path, 'Comp', requires=['Nope'], questions='X:\n  type: text\n')

    with pytest.raises(ValueError, match='Nope'):
        insert_question_blocks(core, [comp])


def test_insert_question_blocks_skips_components_with_no_questions(tmp_path):
    core = [('a', {'A': {}}), ('z', {'Z': {}})]
    comp = _manifest(tmp_path, 'Comp')

    blocks = insert_question_blocks(core, [comp])

    assert [name for name, _ in blocks] == ['a', 'z']

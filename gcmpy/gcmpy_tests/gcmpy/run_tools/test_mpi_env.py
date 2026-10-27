"""Apply legacy MPI environment blocks without invoking a shell."""

import os
from pathlib import Path

import pytest
import yaml

from gcmpy.run_tools.mpi_env import apply_mpi_environment, apply_setenv_block
from gcmpy.setup_tools import setup_main


@pytest.mark.parametrize("initial", [None, "inherited"])
def test_setenv_without_value_sets_empty_string(monkeypatch, initial):
    name = "GCMPY_TEST_MPI_EMPTY"
    if initial is None:
        monkeypatch.delenv(name, raising=False)
    else:
        monkeypatch.setenv(name, initial)

    apply_setenv_block(f"  setenv\t{name}  \n")

    assert os.environ[name] == ""


@pytest.mark.parametrize("initial", [None, "inherited"])
def test_unsetenv_removes_variable_even_if_absent(monkeypatch, initial):
    name = "GCMPY_TEST_MPI_UNSET"
    if initial is None:
        monkeypatch.delenv(name, raising=False)
    else:
        monkeypatch.setenv(name, initial)

    apply_setenv_block(f"unsetenv {name}\n")

    assert name not in os.environ


def test_statements_are_applied_in_order_with_literal_values(monkeypatch):
    name = "GCMPY_TEST_MPI_ORDER"
    monkeypatch.setenv(name, "inherited")
    apply_setenv_block(f"setenv {name} first\nunsetenv {name}\nsetenv {name}\n")
    assert os.environ[name] == ""

    # Preserve the existing parser's literal treatment, not shell evaluation.
    apply_setenv_block(f'setenv {name} "$HOME with spaces"\n')
    assert os.environ[name] == '"$HOME with spaces"'
    apply_setenv_block(f"unsetenv {name}\n")
    assert name not in os.environ


@pytest.mark.parametrize("text", [None, "", "\n# comment\n", "setenv\nunsetenv\n"])
def test_empty_or_incomplete_blocks_are_ignored(text):
    before = dict(os.environ)
    apply_setenv_block(text)
    assert dict(os.environ) == before


def test_unsupported_statements_are_not_executed(monkeypatch):
    name = "GCMPY_TEST_MPI_UNSUPPORTED"
    monkeypatch.setenv(name, "unchanged")
    apply_setenv_block(
        f"#setenv {name} commented\n"
        f"export {name}=changed\n"
        f"unsetenv {name} extra_argument\n"
    )
    assert os.environ[name] == "unchanged"


def test_real_mpt_defaults(monkeypatch):
    config = Path(setup_main.__file__).parent / "config" / "mpi_config.yaml"
    defaults = yaml.safe_load(config.read_text())["mpt"]
    empty = ["MPI_COLL_REPRODUCIBLE", "MPI_MEMMAP_OFF"]
    removed = ["MPI_NUM_MEMORY_REGIONS", "SUPPRESS_XPMEM_TRIM_THRESH", "PMI_RANK"]
    values = {
        "SLURM_DISTRIBUTION": "block", "MPI_XPMEM_ENABLED": "yes",
        "MPI_LAUNCH_TIMEOUT": "40", "MPI_COMM_MAX": "1024",
        "MPI_GROUP_MAX": "1024", "MPI_BUFS_PER_PROC": "256",
    }
    for name in empty + removed + list(values):
        monkeypatch.setenv(name, "inherited")
    for name in ("MPI_DISPLAY_SETTINGS", "MPI_VERBOSE", "FOR_IGNORE_EXCEPTIONS"):
        monkeypatch.delenv(name, raising=False)

    apply_mpi_environment({'mpi_environment': defaults})

    for name in empty:
        assert os.environ[name] == ""
    for name in removed:
        assert name not in os.environ
    for name, value in values.items():
        assert os.environ[name] == value
    for name in ("MPI_DISPLAY_SETTINGS", "MPI_VERBOSE", "FOR_IGNORE_EXCEPTIONS"):
        assert name not in os.environ


def test_mapping_overrides_both_legacy_blocks(monkeypatch):
    names = ['GCMPY_TEST_MPI_OVERRIDE', 'GCMPY_TEST_MPI_REMOVE', 'MPI_SHEPHERD']
    for name in names:
        monkeypatch.setenv(name, 'inherited')
    config = {
        'mpi_config': 'setenv GCMPY_TEST_MPI_OVERRIDE legacy\n'
                      'setenv GCMPY_TEST_MPI_REMOVE legacy',
        'ocean': {'mpt_shepherd': 'setenv MPI_SHEPHERD true'},
        'mpi_environment': {
            'GCMPY_TEST_MPI_OVERRIDE': 'mapping',
            'GCMPY_TEST_MPI_REMOVE': None,
            'MPI_SHEPHERD': '',
        },
    }
    apply_mpi_environment(config)
    assert os.environ[names[0]] == 'mapping'
    assert names[1] not in os.environ
    assert os.environ['MPI_SHEPHERD'] == ''
    assert config['mpi_environment']['GCMPY_TEST_MPI_REMOVE'] is None


def test_legacy_config_without_mapping_still_works(monkeypatch):
    monkeypatch.delenv('GCMPY_TEST_MPI_LEGACY', raising=False)
    monkeypatch.delenv('MPI_SHEPHERD', raising=False)
    apply_mpi_environment({
        'mpi_config': 'setenv GCMPY_TEST_MPI_LEGACY legacy',
        'ocean': {'mpt_shepherd': 'setenv MPI_SHEPHERD true'},
    })
    assert os.environ['GCMPY_TEST_MPI_LEGACY'] == 'legacy'
    assert os.environ['MPI_SHEPHERD'] == 'true'


def test_mapping_values_are_literal_and_keep_whitespace(monkeypatch):
    name = 'GCMPY_TEST_MPI_LITERAL'
    monkeypatch.delenv(name, raising=False)
    value = '  $HOME "quotes"; $(not-a-command)\nnext line  '
    apply_mpi_environment({'mpi_environment': {name: value}})
    assert os.environ[name] == value


@pytest.mark.parametrize('settings', [
    None, [], 'setenv NAME value',
    {'BAD=NAME': 'value'}, {'': 'value'}, {1: 'value'},
    {'GCMPY_TEST_MPI_INVALID': 12}, {'GCMPY_TEST_MPI_INVALID': False},
    {'GCMPY_TEST_MPI_INVALID': ['value']}, {'GCMPY_TEST_MPI_INVALID': {}},
    {'GCMPY_TEST_MPI_INVALID': 'embedded\0nul'},
])
def test_invalid_mapping_fails_before_environment_changes(monkeypatch, settings):
    monkeypatch.setenv('GCMPY_TEST_MPI_LEGACY', 'original')
    before = dict(os.environ)
    with pytest.raises(ValueError, match='mpi_environment'):
        apply_mpi_environment({
            'mpi_config': 'setenv GCMPY_TEST_MPI_LEGACY changed',
            'mpi_environment': settings,
        })
    assert dict(os.environ) == before


def test_all_stack_defaults_have_valid_environment_values(monkeypatch):
    config = Path(setup_main.__file__).parent / 'config' / 'mpi_config.yaml'
    stacks = yaml.safe_load(config.read_text())
    for settings in stacks.values():
        for name in settings:
            monkeypatch.delenv(name, raising=False)
        apply_mpi_environment({'mpi_environment': settings})
        for name, value in settings.items():
            if value is None:
                assert name not in os.environ
            else:
                assert os.environ[name] == value

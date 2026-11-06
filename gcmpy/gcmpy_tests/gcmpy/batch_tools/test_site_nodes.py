"""Supported site processors and set_num_CPUs behavior after NCCS cas retirement."""

from types import SimpleNamespace

import pytest

import gcmpy.setup_tools.process_questions as process_questions
import gcmpy.setup_tools.setup_main as setup_main
from gcmpy.batch_tools.computing_sites import site_nodes_dict
from gcmpy.setup_tools.setup_main import SetupGCM
from gcmpy_tests.gcmpy.setup_tools.test_template_vars import _exp_config


def test_site_nodes_dict_supported_processors():
    assert site_nodes_dict['NCCS'] == {'mil': 120}
    assert site_nodes_dict['NAS'] == {
        'bro': 24, 'sky': 40, 'cas': 40, 'rom': 120, 'mil': 120, 'tur': 240,
    }


def test_nccs_processor_choices_only_offer_milan(monkeypatch):
    monkeypatch.setitem(process_questions.envdict, 'site', 'NCCS')
    questions = {'processor': SimpleNamespace(choices=[])}
    process_questions.Handle.processor_choices(questions, 'processor')
    assert questions['processor'].choices == ['mil']


def _make_setup(tmp_path, monkeypatch, site, **exp_overrides):
    etc = tmp_path / 'etc'
    etc.mkdir()
    (etc / '.AGCM_VERSION').write_text('v-test\n')
    monkeypatch.setitem(setup_main.pathdict, 'etc', etc)
    monkeypatch.setitem(setup_main.envdict, 'site', site)

    cfg = _exp_config(tmp_path)
    cfg.update(exp_overrides)
    return SetupGCM(cfg)


@pytest.mark.parametrize('processor, cpus', [('mil', 120)])
def test_nccs_set_num_CPUs_honors_answer(tmp_path, monkeypatch, processor, cpus):
    exp = _make_setup(tmp_path, monkeypatch, 'NCCS', processor=processor)
    exp.set_num_CPUs()
    assert exp.num_CPUs == cpus
    assert exp.expConfig['processor'] == processor   # never clobbered


def test_nccs_set_num_CPUs_defaults_to_mil(tmp_path, monkeypatch):
    exp = _make_setup(tmp_path, monkeypatch, 'NCCS')
    exp.set_num_CPUs()
    assert exp.num_CPUs == 120
    assert exp.expConfig['processor'] == 'mil'


@pytest.mark.parametrize('processor', ['xeon', 'cas'])
def test_nccs_set_num_CPUs_rejects_unsupported_processor(tmp_path, monkeypatch, processor):
    exp = _make_setup(tmp_path, monkeypatch, 'NCCS', processor=processor)
    with pytest.raises(SystemExit) as exc:
        exp.set_num_CPUs()
    assert exc.value.code == 1


@pytest.mark.parametrize('processor, cpus, expected', [
    ('mil', 120, 'mil_ait'),
    ('cas', 40, 'cas_ait'),
    ('cas_ait', 40, 'cas_ait'),   # driver yamls store the suffixed name
])
def test_nas_set_num_CPUs_suffixes_and_lookup(tmp_path, monkeypatch,
                                              processor, cpus, expected):
    exp = _make_setup(tmp_path, monkeypatch, 'NAS', processor=processor)
    exp.set_num_CPUs()
    assert exp.num_CPUs == cpus
    assert exp.expConfig['processor'] == expected


def test_nas_default_is_rom(tmp_path, monkeypatch):
    exp = _make_setup(tmp_path, monkeypatch, 'NAS')
    exp.set_num_CPUs()
    assert exp.num_CPUs == 120
    assert exp.expConfig['processor'] == 'rom_ait'

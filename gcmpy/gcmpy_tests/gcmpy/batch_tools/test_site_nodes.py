"""
Site node-map parity and set_num_CPUs behavior.

Upstream gcm_setup hard-codes cores-per-node (gcm_setup:455/458 for NCCS,
503-516 for NAS).  gcmpy's site_nodes_dict must match: the NCCS entries
were once swapped (mil=40/cas=120), so sbatch asked for 120 CPUs on
46-CPU cas nodes and died with "CPU count per node can not be
satisfied".  set_num_CPUs must also honor the answered 'processor'
question instead of clobbering it with the last map entry (which made
'mil' unreachable).
"""

import pytest

import gcmpy.setup_tools.setup_main as setup_main
from gcmpy.batch_tools.computing_sites import site_nodes_dict
from gcmpy.setup_tools.setup_main import SetupGCM
from gcmpy_tests.gcmpy.setup_tools.test_template_vars import _exp_config


def test_site_nodes_dict_matches_upstream_gcm_setup():
    assert site_nodes_dict['NCCS'] == {'mil': 120, 'cas': 40}
    assert site_nodes_dict['NAS'] == {
        'bro': 24, 'sky': 40, 'cas': 40, 'rom': 120, 'mil': 120, 'tur': 240,
    }


def _make_setup(tmp_path, monkeypatch, site, **exp_overrides):
    etc = tmp_path / 'etc'
    etc.mkdir()
    (etc / '.AGCM_VERSION').write_text('v-test\n')
    monkeypatch.setitem(setup_main.pathdict, 'etc', etc)
    monkeypatch.setitem(setup_main.envdict, 'site', site)

    cfg = _exp_config(tmp_path)
    cfg.update(exp_overrides)
    return SetupGCM(cfg)


@pytest.mark.parametrize('processor, cpus', [('mil', 120), ('cas', 40)])
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


def test_nccs_set_num_CPUs_rejects_unknown_processor(tmp_path, monkeypatch):
    exp = _make_setup(tmp_path, monkeypatch, 'NCCS', processor='xeon')
    with pytest.raises(SystemExit):
        exp.set_num_CPUs()


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

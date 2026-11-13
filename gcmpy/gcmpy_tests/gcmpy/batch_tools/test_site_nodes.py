"""Supported site processors and set_num_CPUs behavior after NCCS cas retirement."""

from types import SimpleNamespace

import pytest

import gcmpy.batch_tools.computing_sites as computing_sites
import gcmpy.setup_tools.process_questions as process_questions
import gcmpy.setup_tools.setup_main as setup_main
from gcmpy.batch_tools.computing_sites import site_nodes_dict
from gcmpy.setup_tools.setup_main import SetupGCM
from gcmpy_tests.gcmpy.setup_tools.test_template_vars import _exp_config


@pytest.fixture(autouse=True)
def nas_rhel8(monkeypatch):
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release',
                        lambda: {'ID': 'rhel', 'VERSION_ID': '8.10'})


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


@pytest.mark.parametrize('version, choices', [
    ('8.10', ['rom', 'mil', 'cas', 'bro', 'sky']),
    ('9.8', ['tur']),
])
def test_nas_processor_choices_follow_os(monkeypatch, version, choices):
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release',
                        lambda: {'ID': 'rhel', 'VERSION_ID': version})
    monkeypatch.setitem(process_questions.envdict, 'site', 'NAS')
    questions = {'processor': SimpleNamespace(choices=[])}
    process_questions.Handle.processor_choices(questions, 'processor')
    assert questions['processor'].choices == choices


@pytest.mark.parametrize('processor', [None, 'tur', 'tur_ath'])
def test_nas_rhel9_accepts_turin_and_defaults_to_it(tmp_path, monkeypatch, processor):
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release',
                        lambda: {'ID': 'rhel', 'VERSION_ID': '9.8'})
    exp = _make_setup(tmp_path, monkeypatch, 'NAS', processor=processor)
    exp.set_num_CPUs()
    assert exp.num_CPUs == 240
    assert exp.expConfig['processor'] == 'tur_ath'


@pytest.mark.parametrize('version, processor', [
    ('8.10', 'tur_ath'), ('9.8', 'rom_ait'), ('9.8', 'mil'),
])
def test_nas_rejects_processor_from_other_os(tmp_path, monkeypatch, capsys, version, processor):
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release',
                        lambda: {'ID': 'rhel', 'VERSION_ID': version})
    exp = _make_setup(tmp_path, monkeypatch, 'NAS', processor=processor)
    with pytest.raises(SystemExit):
        exp.set_num_CPUs()
    assert 'not supported on this NAS build OS' in capsys.readouterr().out


@pytest.mark.parametrize('release', [
    {}, {'ID': 'rhel', 'VERSION_ID': '10'}, {'ID': 'other', 'VERSION_ID': '9.8'},
])
def test_nas_unknown_os_fails_without_guessing(monkeypatch, release):
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release', lambda: release)
    with pytest.raises(SystemExit, match='Cannot identify NAS setup OS'):
        computing_sites.nas_processor_nodes()


def test_nas_missing_os_release_fails_without_guessing(monkeypatch):
    def missing_release():
        raise OSError('missing os-release')
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release', missing_release)
    with pytest.raises(SystemExit, match='Cannot identify NAS setup OS'):
        computing_sites.nas_processor_nodes()


def _write_os_rc(etc, text):
    etc.mkdir(exist_ok=True)
    (etc / 'OS.rc').write_text(text)


def _os_rc(version):
    return f'OS_ID: rhel\nOS_VERSION: {version}\nOS_NAME: Red Hat Enterprise Linux {version}\n'


def test_nas_build_os_overrides_setup_host(tmp_path):
    # Host is RHEL 8 (autouse fixture) but the install was built on RHEL 9.
    _write_os_rc(tmp_path, _os_rc('9.8'))
    assert list(computing_sites.nas_processor_nodes(tmp_path)) == ['tur']


def test_nas_missing_os_rc_warns_and_uses_host(tmp_path, capsys):
    assert 'rom' in computing_sites.nas_processor_nodes(tmp_path)
    assert 'cannot be verified' in capsys.readouterr().out


@pytest.mark.parametrize('text', ['', 'OS_ID: rhel\n', 'OS_ID: rhel\nOS_VERSION: nine\n', 'OS_VERSION: 9.8\n'])
def test_nas_malformed_os_rc_is_an_error(tmp_path, text):
    _write_os_rc(tmp_path, text)
    with pytest.raises(SystemExit, match='malformed'):
        computing_sites.nas_processor_nodes(tmp_path)


@pytest.mark.parametrize('build, host, run_p, error', [
    ('8.10', '8.11', 'model=rom_ait', None),
    ('9.8', '9.8', 'model=tur_ath', None),
    ('9.8', '8.10', 'model=tur_ath', 'built on rhel 9'),
    ('8.10', '9.8', 'model=rom_ait', 'built on rhel 8'),
    ('8.10', '8.10', 'model=tur_ath', "model 'tur'"),
    ('9.8', '9.8', 'select=1:ncpus=240', "model 'None'"),
])
def test_check_nas_submit(tmp_path, monkeypatch, build, host, run_p, error):
    _write_os_rc(tmp_path, _os_rc(build))
    monkeypatch.setattr(computing_sites.platform, 'freedesktop_os_release',
                        lambda: {'ID': 'rhel', 'VERSION_ID': host})
    if error is None:
        computing_sites.check_nas_submit(tmp_path, run_p)
    else:
        with pytest.raises(SystemExit, match=error):
            computing_sites.check_nas_submit(tmp_path, run_p)


def test_check_nas_submit_without_os_rc_checks_model_against_host(tmp_path):
    computing_sites.check_nas_submit(tmp_path, 'model=rom_ait')
    with pytest.raises(SystemExit, match="model 'tur'"):
        computing_sites.check_nas_submit(tmp_path, 'model=tur_ath')

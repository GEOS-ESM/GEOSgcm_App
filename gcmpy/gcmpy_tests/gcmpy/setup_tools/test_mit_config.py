"""
MITgcm LLC ocean setup (csh gcm_setup PR #931): catalog profiles, the
question rules, the layout and profile checks, and the boundary-condition
overrides. Mirrors the csh MIT block without needing a model build.
"""

from pathlib import Path
from types import SimpleNamespace

import pytest

from gcmpy.setup_tools import mit
from gcmpy.setup_tools.atmosphere import get_atmos_config
from gcmpy.setup_tools.ocean import get_ocean_config
from gcmpy.setup_tools.process_questions import Handle
from gcmpy.setup_tools.questions.load_questions import load_yamls


def _mit_config(**overrides):
    cfg = {
        'experiment_id': 'mit_test',
        'experiment_description': 'mit config test',
        'AGCM_grid': 'c90',
        'AGCM_LM': '72',
        'OCEAN_coupled': True,
        'OCEAN_type': 'MIT',
        'MIT_resolution': 'llc90',
        'OCEAN_data_atmosphere': False,
        'DATAOCEAN_resolution': None,
        'MOM6_resolution': None,
        'SEAICE_model': 'CICE4',
        'OGCM_LM': '50',
        'MOIST_version': 'BACM_1M',
        'heartbeat': 450,
        'history_template': 'HISTORY.AOGCM_MITgcm.rc.tmpl',
    }
    cfg.update(overrides)
    return cfg


@pytest.mark.parametrize('grid, llc, expected', [
    ('c90', 'llc90', dict(IM=5400, JM=15, NX=360, NY=1, nprocs=360,
                          gridname='LL5400x15-LL', mit_config_id='c90_llc90_02',
                          kpar_file='SEAWIFS_KPAR_mon_clim.5400x15', res='MITLLC5400x0015')),
    ('c180', 'llc270', dict(IM=23040, JM=30, NX=768, NY=1, nprocs=768,
                            gridname='LL23040x30-LL', mit_config_id='c180_llc270_01',
                            kpar_file='SEAWIFS_KPAR_mon_clim.23040x30', res='MITLLC23040x0030')),
])
def test_mit_ocean_profiles(grid, llc, expected):
    ocean = get_ocean_config(_mit_config(AGCM_grid=grid, MIT_resolution=llc))
    for key, value in expected.items():
        assert ocean[key] == value
    assert ocean['LM'] == 50
    assert ocean['llc'] == llc
    assert ocean['internal_restart'] == '#'
    assert ocean['CICE4'] == ''
    assert ocean['CICE6'] == '#DELETE'


@pytest.mark.parametrize('grid, llc, nx, ny', [
    ('c90', 'llc90', 10, 36),
    ('c180', 'llc270', 8, 96),
])
def test_mit_atmosphere_layout_matches_ocean_ranks(grid, llc, nx, ny):
    cfg = _mit_config(AGCM_grid=grid, MIT_resolution=llc)
    ocean = get_ocean_config(cfg)
    atmos = get_atmos_config(cfg, ocean['NX'], ocean['NY'])
    assert (atmos['nx'], atmos['ny']) == (nx, ny)
    assert atmos['nx'] * atmos['ny'] == ocean['nprocs']


def test_mit_remap_name_default_and_env_override(monkeypatch):
    monkeypatch.delenv('GEOS_MIT_REMAP_NAME', raising=False)
    assert get_ocean_config(_mit_config())['mit_remap_name'] == 'DC0360xPC0181_LL5400x15-LL.bin'
    monkeypatch.setenv('GEOS_MIT_REMAP_NAME', 'custom.bin')
    assert get_ocean_config(_mit_config())['mit_remap_name'] == 'custom.bin'


def test_mit_resolution_defaults_from_grid_for_old_drivers():
    cfg = _mit_config(AGCM_grid='c180')
    del cfg['MIT_resolution']
    assert get_ocean_config(cfg)['llc'] == 'llc270'


def test_non_mit_ocean_has_no_mit_settings():
    ocean = get_ocean_config(_mit_config(OCEAN_type='MOM6', AGCM_grid='c90',
                                         MOM6_resolution='o540', MIT_resolution=None))
    assert ocean['llc'] == ''
    assert ocean['internal_restart'] == ''
    assert ocean['mit_remap_name'] == ''
    assert ocean['MIT'] == '#DELETE'


def test_validate_setup_accepts_matching_profiles():
    mit.validate_setup('c90', 'llc90', 'CICE4', 10, 36, 360, 'NAS', {})
    mit.validate_setup('c180', 'llc270', 'CICE4', 8, 96, 768, 'NCCS', {})


@pytest.mark.parametrize('args, message', [
    (('c12', 'llc90', 'CICE4', 10, 36, 360, 'NAS', {}), 'c90 or c180'),
    (('c90', 'llc270', 'CICE4', 10, 36, 360, 'NAS', {}), 'requires llc90'),
    (('c180', 'llc90', 'CICE4', 8, 96, 768, 'NCCS', {}), 'requires llc270'),
    (('c90', 'llc90', 'CICE6', 10, 36, 360, 'NAS', {}), 'supports only CICE4'),
    (('c90', 'llc90', 'CICE4', 8, 96, 360, 'NAS', {}), 'rank counts must agree'),
])
def test_validate_setup_rejects_bad_choices(args, message):
    with pytest.raises(mit.MITSetupError, match=message):
        mit.validate_setup(*args)


def test_g12_profile_requires_nccs_c180(monkeypatch):
    with pytest.raises(mit.MITSetupError, match='G12_t4_MITC4'):
        mit.bcs_profile('NAS', 'c180', {'GEOS_MIT_BCS_PROFILE': 'G12_t4_MITC4'})
    with pytest.raises(mit.MITSetupError, match='G12_t4_MITC4'):
        mit.bcs_profile('NCCS', 'c90', {'GEOS_MIT_BCS_PROFILE': 'G12_t4_MITC4'})


def test_unknown_bcs_profile_is_rejected():
    with pytest.raises(mit.MITSetupError, match='standard or G12_t4_MITC4'):
        mit.bcs_profile('NCCS', 'c180', {'GEOS_MIT_BCS_PROFILE': 'bogus'})


def test_bcs_profile_defaults_to_g12_on_nccs_c180():
    assert mit.bcs_profile('NCCS', 'c180', {}) == 'G12_t4_MITC4'
    assert mit.bcs_profile('NCCS', 'c90', {}) == 'standard'
    assert mit.bcs_profile('NAS', 'c180', {}) == 'standard'


def _ocean(llc):
    return get_ocean_config(_mit_config(MIT_resolution=llc, AGCM_grid='c90' if llc == 'llc90' else 'c180'))


def test_resolve_bcs_standard_profile_uses_coupled_dir():
    bcs = mit.resolve_bcs('NAS', 'c90', _ocean('llc90'), Path('/root/ocean'), {})
    assert bcs['profile'] == 'standard'
    assert bcs['coupled_dir'] == '/root/ocean/MIT'
    assert bcs['kpar'] == '/root/ocean/MIT/5400x15/SEAWIFS_KPAR_mon_clim.5400x15'
    assert bcs['input_dir'] == '/root/ocean/MIT/5400x15/INPUT'
    assert bcs['cice_grid'] == '/root/ocean/MIT/5400x15/cice/grid_cice.bin'
    assert bcs['cice_kmt'] == '/root/ocean/MIT/5400x15/cice/kmt_cice.bin'
    assert bcs['remap'] == '/root/ocean/MIT/DC0360xPC0181_LL5400x15-LL.bin'
    # None means "stagebcs default" for these
    assert bcs['tile'] is None and bcs['gridspec'] is None and bcs['bcs_dir'] is None


def test_resolve_bcs_g12_profile_on_nccs_c180():
    bcs = mit.resolve_bcs('NCCS', 'c180', _ocean('llc270'), Path('/root/ocean'), {})
    assert bcs['profile'] == 'G12_t4_MITC4'
    assert bcs['bcs_dir'] == mit.G12_T4_MITC4['bcs_dir']
    assert bcs['land_bcrslv'] == 'CF0180x6C_M6TP1440x1080'
    assert bcs['tile'] == mit.G12_T4_MITC4['tile']
    assert bcs['cice_grid'] == mit.G12_T4_MITC4['cice_grid']
    assert bcs['input_dir'] == mit.G12_T4_MITC4['coupled_dir'] + '/1440x1080/INPUT'


def test_resolve_bcs_env_overrides_win():
    env = {
        'GEOS_MIT_CPLDIR': '/mine/cpl',
        'GEOS_MIT_TILE': '/mine/tile.data',
        'GEOS_MIT_INPUTDIR': '/mine/input',
        'GEOS_MIT_REMAP_NAME': 'other.bin',
        'GEOS_MIT_CICE_KMT': '/mine/kmt.bin',
    }
    bcs = mit.resolve_bcs('NAS', 'c90', _ocean('llc90'), Path('/root/ocean'), env)
    assert bcs['coupled_dir'] == '/mine/cpl'
    assert bcs['tile'] == '/mine/tile.data'
    assert bcs['input_dir'] == '/mine/input'
    assert bcs['cice_kmt'] == '/mine/kmt.bin'
    assert bcs['remap'] == '/mine/cpl/other.bin'
    assert bcs['remap_name'] == 'other.bin'


def test_setup_summary_records_the_choices():
    cfg = _mit_config(AGCM_grid='c180', MIT_resolution='llc270')
    ocean = get_ocean_config(cfg)
    atmos = get_atmos_config(cfg, ocean['NX'], ocean['NY'])
    bcs = mit.resolve_bcs('NAS', 'c180', ocean, Path('/root/ocean'), {})
    text = mit.setup_summary('c180', 'llc270', ocean, atmos, bcs)
    assert 'MIT_CONFIG_ID=c180_llc270_01' in text
    assert 'Selected profile: C180 / llc270 / CICE4' in text
    assert 'Atmosphere NX x NY: 8 x 96; ocean NX x NY: 768 x 1' in text


def test_required_build_lines():
    ocean = _ocean('llc90')
    lines = mit.required_build_lines(ocean)
    assert lines[0] == 'Required build: BUILD_MIT_OCEAN=ON, MIT_CONFIG_ID=c90_llc90_02'
    assert 'sNx=sNy=15, nPx=360, nPy=1' in lines[1]


# --- question handlers -----------------------------------------------------

class _Q:
    """Stands in for generate_question.Question: answer plus a re-ask that records calls."""
    def __init__(self, answer=None, choices=None):
        self.answer = answer
        self.choices = choices
        self.default = None
        self.q_type = 'select'
        self.asks = 0

    def load_question(self, question_dict):
        self.asks += 1
        self.answer = self.next_answer.pop(0)


def _questions(agcm_grid, ocean_type, **answers):
    questions = {
        'AGCM_grid': _Q(agcm_grid),
        'OCEAN_type': _Q(ocean_type),
        'MIT_resolution': _Q(),
        'SEAICE_model': _Q(),
        'OGCM_LM': _Q('50'),
    }
    for key, value in answers.items():
        questions[key].answer = value
    return questions


def test_mit_resolution_choices_put_matching_code_first():
    qs = _questions('c180', 'MIT')
    Handle.MIT_resolution_choices(qs, 'MIT_resolution')
    assert qs['MIT_resolution'].choices[0].startswith('llc270')
    qs = _questions('c90', 'MIT')
    Handle.MIT_resolution_choices(qs, 'MIT_resolution')
    assert qs['MIT_resolution'].choices[0].startswith('llc90')


def test_mit_resolution_choices_skipped_for_other_oceans():
    qs = _questions('c12', 'MOM6')
    Handle.MIT_resolution_choices(qs, 'MIT_resolution')
    assert qs['MIT_resolution'].choices is None


def test_mit_resolution_valid_reasks_until_it_matches():
    qs = _questions('c90', 'MIT', MIT_resolution='llc270 (1/3-deg, 23040x30 compact, 768 tiles)')
    qs['MIT_resolution'].next_answer = ['llc90 (1-deg, 5400x15 compact, 360 tiles)']
    Handle.MIT_resolution_valid(qs, 'MIT_resolution')
    assert qs['MIT_resolution'].asks == 1
    assert qs['MIT_resolution'].answer.startswith('llc90')


def test_seaice_is_cice4_only_for_mit():
    qs = _questions('c90', 'MIT')
    Handle.seaice_MIT_choices(qs, 'SEAICE_model')
    assert qs['SEAICE_model'].choices == ['CICE4']


def test_mit_lm_must_be_50():
    qs = _questions('c90', 'MIT', OGCM_LM='72')
    qs['OGCM_LM'].next_answer = ['50']
    Handle.MIT_LM_valid(qs, 'OGCM_LM')
    assert qs['OGCM_LM'].asks == 1
    assert qs['OGCM_LM'].answer == '50'


def test_ocean_type_valid_allows_c180_for_mit():
    qs = _questions('c180', 'MIT')
    Handle.OCEAN_type_valid(qs, 'OCEAN_type')
    qs = _questions('c12', 'MIT')
    with pytest.raises(SystemExit, match='c90 or c180'):
        Handle.OCEAN_type_valid(qs, 'OCEAN_type')


def test_mit_resolution_question_follows_mit_and_precedes_lm():
    questions = list(load_yamls())
    assert questions.index('MOM6_resolution') < questions.index('MIT_resolution') < questions.index('OGCM_LM')
    yaml_questions = load_yamls()
    assert yaml_questions['MIT_resolution']['follows_up'] == [['OCEAN_type', ['MIT']]]


def test_seaice_default_is_not_changed_for_mom6():
    qs = _questions('c90', 'MOM6')
    Handle.seaice_MIT_choices(qs, 'SEAICE_model')
    assert qs['SEAICE_model'].choices is None

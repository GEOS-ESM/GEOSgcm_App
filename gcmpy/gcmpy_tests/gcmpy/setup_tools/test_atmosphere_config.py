"""
Atmosphere config-builder unit tests.

Guards the resolution catalog navigation: plain resolutions must keep the
disabled-conus defaults ('#', stretch '') and the default USE_SHMEM /
NUM_READERS, while stretched (SG) resolutions like c270 must still resolve
their in-section conus/stretch_factor values.
"""

import pytest

from types import SimpleNamespace

from gcmpy.setup_tools.atmosphere import get_atmos_config, get_heartbeat_default
from gcmpy.setup_tools.process_questions import Handle
from gcmpy.setup_tools.questions.load_questions import load_yamls


def _base_config(**overrides):
    cfg = {
        'AGCM_grid': 'c12',
        'AGCM_LM': '72',
        'MOIST_version': 'BACM_1M',
        'heartbeat': 450,
    }
    cfg.update(overrides)
    return cfg


def test_plain_c12_keeps_disabled_conus_and_defaults():
    atmos = get_atmos_config(_base_config())
    assert atmos['conus'] == '#'
    assert atmos['stretch_factor'] == ''
    assert atmos['FV_stretch_fac'] == 'stretch_fac = 1.0'
    assert atmos['schmidt'] == 'do_schmidt  = .false.'
    assert atmos['use_SHMEM'] == 0
    assert atmos['num_readers'] == 1
    assert atmos['res'] == 'CF0012x6C'
    assert atmos['low_res'] is True


def test_stretched_c270_resolves_in_section_conus():
    atmos = get_atmos_config(_base_config(AGCM_grid='c270'))
    assert atmos['conus'] == ''
    assert atmos['stretch_factor'] == 2.5
    assert atmos['FV_stretch_fac'] == 'stretch_fac = 2.5'
    assert atmos['schmidt'] == 'do_schmidt  = .true.'
    assert atmos['res'] == 'CF0270x6C-SG001'


def test_plain_c48_not_poisoned_by_stretched_siblings():
    atmos = get_atmos_config(_base_config(AGCM_grid='c48'))
    assert atmos['conus'] == '#'
    assert atmos['stretch_factor'] == ''
    assert atmos['use_SHMEM'] == 0


# DT, CONV_DT, CHEM_DT, SOLAR_DT, IRRAD_DT, OCEAN_DT from develop's
# set_resolution_params.csh (e4ada343); no CSH files/interpreter needed to run.
RESOLUTION_TIMESTEPS = {
    'c12':   (1200, 1200, 3600, 3600, 3600, 3600),
    'c24':   (1200, 1200, 3600, 3600, 3600, 3600),
    'c48':   (1200, 1200, 3600, 3600, 3600, 3600),
    'c90':   (900, 900, 1800, 3600, 3600, 3600),
    'c180':  (600, 600, 1800, 3600, 3600, 3600),
    'c270':  (600, 600, 1800, 3600, 3600, 3600),
    'c360':  (450, 450, 1800, 3600, 3600, 3600),
    'c540':  (300, 300, 1800, 3600, 3600, 3600),
    'c720':  (300, 300, 1200, 3600, 3600, 3600),
    'c1120': (300, 300, 1200, 3600, 3600, 3600),
    'c1080': (150, 300, 900, 1800, 1800, 1800),
    'c1440': (150, 300, 900, 1800, 1800, 1800),
    'c1536': (75, 300, 900, 1800, 1800, 1800),
    'c2160': (75, 300, 900, 1800, 1800, 1800),
    'c2880': (75, 300, 900, 1800, 1800, 1800),
    'c4320': (75, 300, 900, 1800, 1800, 1800),
    'c5760': (75, 300, 900, 1800, 1800, 1800),
}
TIMESTEP_FIELDS = ('dt', 'conv_dt', 'chem_dt', 'dt_solar', 'dt_irrad', 'dt_ocean')


@pytest.mark.parametrize('resolution,expected', RESOLUTION_TIMESTEPS.items())
@pytest.mark.parametrize('ocean_type', [None, 'MOM6', 'MIT'])
@pytest.mark.parametrize('microphysics', ['GFDL_1M', 'MGB2_2M', 'BACM_1M'])
def test_timestep_defaults_match_csh(resolution, expected, ocean_type, microphysics):
    expected = list(expected)
    if resolution == 'c180' and ocean_type:
        expected[:3] = [450, 450, 900]
    if microphysics == 'BACM_1M':
        expected[:3] = [450, 450, 450]
    if ocean_type:
        # gcm_setup sets OCEAN_DT=HEARTBEAT after set_resolution_params.csh.
        expected[5] = expected[0]

    config = _base_config(AGCM_grid=resolution, MOIST_version=microphysics,
                          OCEAN_coupled=bool(ocean_type), OCEAN_type=ocean_type)
    assert get_heartbeat_default(config) == expected[0]
    questions = {key: SimpleNamespace(answer=value) for key, value in config.items()}
    Handle.heartbeat_default(questions, 'heartbeat')
    assert questions['heartbeat'].default == str(expected[0])

    config['heartbeat'] = questions['heartbeat'].default
    atmos = get_atmos_config(config, ocean_nx=4, ocean_ny=6)
    assert [int(atmos[key]) for key in TIMESTEP_FIELDS] == expected


@pytest.mark.parametrize('heartbeat', ['225', 225])
@pytest.mark.parametrize('ocean_type', [None, 'MOM6', 'MIT'])
@pytest.mark.parametrize('microphysics', ['GFDL_1M', 'MGB2_2M', 'BACM_1M'])
def test_user_heartbeat_overrides_recommendation(heartbeat, ocean_type, microphysics):
    config = _base_config(AGCM_grid='c180', MOIST_version=microphysics,
                          OCEAN_coupled=bool(ocean_type), OCEAN_type=ocean_type,
                          heartbeat=heartbeat)
    atmos = get_atmos_config(config, ocean_nx=4, ocean_ny=6)
    assert atmos['dt'] == heartbeat
    assert atmos['dt_ocean'] == (heartbeat if ocean_type else 3600)
    if microphysics == 'BACM_1M':
        assert (atmos['conv_dt'], atmos['chem_dt']) == (450, 450)
    else:
        assert (atmos['conv_dt'], atmos['chem_dt']) == (
            (450, 900) if ocean_type else (600, 1800))


def test_heartbeat_question_follows_ocean_choices():
    questions = load_yamls()
    order = list(questions)
    for key in ('AGCM_grid', 'MOIST_version', 'OCEAN_coupled', 'OCEAN_type'):
        assert order.index(key) < order.index('heartbeat')
    assert not questions['heartbeat']['follows_up']

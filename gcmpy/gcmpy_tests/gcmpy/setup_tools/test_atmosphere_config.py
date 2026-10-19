"""
Atmosphere config-builder unit tests.

Guards the resolution catalog navigation: plain resolutions must keep the
disabled-conus defaults ('#', stretch '') and the default USE_SHMEM /
NUM_READERS, while stretched (SG) resolutions like c270 must still resolve
their in-section conus/stretch_factor values.
"""

import pytest

from gcmpy.setup_tools.atmosphere import get_atmos_config


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

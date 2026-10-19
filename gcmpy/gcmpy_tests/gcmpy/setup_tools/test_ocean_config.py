"""
Ocean config-builder unit tests.

Guards the AGCM.rc template prefix semantics: ``ocean.data`` /
``ocean.latlon`` / ``ocean.modelatm`` must be '' (empty prefix, line kept)
or '#DELETE' (line removed) -- never None, which Jinja renders as the
literal string "None".  Also guards _build_data_atmosphere navigation:
yaml-null (None) OCEAN_data_atmosphere must select the 'false' catalog
branch, not fall through the catalog search second-pass into 'true'.
"""

import pytest

from gcmpy.setup_tools.ocean import get_ocean_config


def _base_config(**overrides):
    cfg = {
        'experiment_id': 'test_exp',
        'experiment_description': 'ocean config test',
        'AGCM_grid': 'c12',
        'AGCM_LM': '72',
        'OCEAN_type': None,
        'OCEAN_coupled': False,
        'OCEAN_data_atmosphere': None,
        'DATAOCEAN_resolution': 'o1',
        'MOM6_resolution': None,
        'SEAICE_model': None,
        'history_template': 'HISTORY.AGCM.rc.tmpl',
    }
    cfg.update(overrides)
    return cfg


def test_uncoupled_o1_template_prefixes_are_empty_strings():
    ocean = get_ocean_config(_base_config())
    assert ocean['data'] == ''
    assert ocean['latlon'] == ''
    assert ocean['cube'] == '#DELETE'
    assert None not in (ocean['data'], ocean['latlon'])


def test_none_data_atmosphere_selects_false_branch():
    ocean = get_ocean_config(_base_config(OCEAN_data_atmosphere=None))
    assert ocean['modelatm'] == ''
    assert ocean['use_data_ATM4OCN'] == '.FALSE.'
    assert ocean['DATAATM'] == '#DELETE'


def test_true_data_atmosphere_selects_true_branch():
    ocean = get_ocean_config(_base_config(OCEAN_data_atmosphere=True))
    assert ocean['modelatm'] == '#DELETE'
    assert ocean['use_data_ATM4OCN'] == '.TRUE.'
    assert ocean['DATAATM'] == ''


def test_false_data_atmosphere_selects_false_branch():
    ocean = get_ocean_config(_base_config(OCEAN_data_atmosphere=False))
    assert ocean['modelatm'] == ''
    assert ocean['use_data_ATM4OCN'] == '.FALSE.'
    assert ocean['DATAATM'] == '#DELETE'


@pytest.mark.parametrize('res, expected', [
    ('o1', 'DE0360xPE0180'),
    ('o2', 'DE1440xPE0720'),
    ('o3', 'DE2880xPE1440'),
    ('CS', 'CF0012x6C'),
])
def test_uncoupled_resolutions_have_empty_data_prefix(res, expected):
    ocean = get_ocean_config(_base_config(DATAOCEAN_resolution=res))
    assert ocean['res'] == expected
    assert ocean['data'] == ''


@pytest.mark.parametrize('res', ['o1', 'o2', 'o3'])
def test_uncoupled_latlon_configs_keep_latlon_lines(res):
    ocean = get_ocean_config(_base_config(DATAOCEAN_resolution=res))
    assert ocean['latlon'] == ''
    assert ocean['cube'] == '#DELETE'


def test_uncoupled_cs_config_keeps_cube_lines():
    ocean = get_ocean_config(_base_config(DATAOCEAN_resolution='CS'))
    assert ocean['latlon'] == '#DELETE'
    assert ocean['cube'] == ''


def test_coupled_mom6_flags_are_active_strings_not_none():
    # The catalog spells an active flag as a bare (YAML null) key --
    # `coupled:`, `seaice.CICE6.CICE6:`, `coupled.MOM6.MOM6:` -- which the
    # loader turns into None.  Forwarded as-is that (a) renders the literal
    # "None" into AGCM.rc ("None OCEAN_NAME: MOM6", "NoneSEAICE_NAME: CICE6")
    # so the rc parser folds it into the key name and the coupled config
    # never takes effect, and (b) reads as *inactive* to the run-side
    # flag_ops.is_enabled() checks (coupled/MOM6/CICE6 branch selection,
    # heartbeat sync, restart staging).
    ocean = get_ocean_config(_base_config(
        OCEAN_coupled=True,
        OCEAN_type='MOM6',
        MOM6_resolution='o72',
        SEAICE_model='CICE6',
        DATAOCEAN_resolution=None,
    ))

    assert ocean['coupled'] == ''
    assert ocean['MOM6'] == ''
    assert ocean['CICE6'] == ''
    assert ocean['MIT'] == '#DELETE'
    assert ocean['CICE4'] == '#DELETE'
    assert ocean['hist_CICE4'] == '#DELETE'

    # setup-side template render and run-side is_enabled() must agree.
    from gcmpy.utils.flag_ops import is_enabled
    assert is_enabled(ocean['coupled'])
    assert is_enabled(ocean['MOM6'])
    assert is_enabled(ocean['CICE6'])
    assert not is_enabled(ocean['MIT'])
    assert not is_enabled(ocean['CICE4'])

    # Jinja prints None as the literal string "None"; no ocean value may be.
    assert None not in ocean.values()

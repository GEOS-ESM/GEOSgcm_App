"""
GOCARTComponent: the component plug-in framework's pilot
(gcmpy.components.base).

Regression-pins the behaviour that lived in gcmpy/setup_tools/gocart.py
and two SetupGCM methods before the move (see component.py's module
docstring): defaults() picks the right aerosol/emissions combination,
post_rc_copy() does the L72 tuning-coefficient swap, and configure() flips
GOCART2G_GridComp.rc to its DATA_DRIVEN instances.
"""

from pathlib import Path

import pytest

from gcmpy.components.GOCART.component import GOCARTComponent
from gcmpy.components.base import discover_components


@pytest.fixture
def gocart_catalog():
    manifest = next(m for m in discover_components() if m.name == 'GOCART')
    return manifest.load_catalog()


@pytest.mark.parametrize('aerosol, emission_key, data_driven', [
    ('Actual', 'OPS', False),
    ('Actual', 'AMIP', False),
    ('Climatological', 'OPS', True),
    ('Climatological', 'AMIP', True),
])
def test_defaults_actual_vs_climatological(gocart_catalog, aerosol, emission_key, data_driven):
    expConfig = {
        'OCEAN_data_atmosphere': False,
        'GOCART_aerosols': aerosol,
        'GOCART_emission': emission_key,
    }

    gocart = GOCARTComponent().defaults(expConfig, gocart_catalog)

    assert gocart['aerosol'] == aerosol
    assert gocart['emissions'] == f'{emission_key}_EMISSIONS'
    assert gocart['data_driven'] == data_driven


def test_defaults_amip_emissions_set_pchem_clim_years_48(gocart_catalog):
    expConfig = {
        'OCEAN_data_atmosphere': False,
        'GOCART_aerosols': 'Actual',
        'GOCART_emission': 'AMIP',
    }

    gocart = GOCARTComponent().defaults(expConfig, gocart_catalog)

    assert gocart['pchem_clim_years'] == 48
    assert gocart['ox_relaxtime'] == '259200.'


def test_defaults_ops_emissions_set_pchem_clim_years_1(gocart_catalog):
    expConfig = {
        'OCEAN_data_atmosphere': False,
        'GOCART_aerosols': 'Actual',
        'GOCART_emission': 'OPS',
    }

    gocart = GOCARTComponent().defaults(expConfig, gocart_catalog)

    assert gocart['pchem_clim_years'] == 1
    assert gocart['ox_relaxtime'] == '0.00'


def test_defaults_data_atmosphere_ignores_gocart_questions(gocart_catalog):
    # questions.yaml's follows_up skips GOCART_aerosols/GOCART_emission for
    # data-atmosphere runs, so expConfig may not even have them.
    expConfig = {'OCEAN_data_atmosphere': True}

    gocart = GOCARTComponent().defaults(expConfig, gocart_catalog)

    assert gocart['aerosol'] == 'Actual'
    assert gocart['emissions'] == 'OPS_EMISSIONS'


def test_post_rc_copy_swaps_l72_for_l181_at_lm_72(tmp_path):
    rc_dir = tmp_path / 'RC'
    rc_dir.mkdir()
    (rc_dir / 'DU2G_instance_DU.rc').write_text(
        "a: 1 # L181\n#b: 2 # L072\nc: 3\n"
    )
    (rc_dir / 'SS2G_instance_SS.rc').write_text(
        "d: 4 # L181\n#e: 5 # L072\n"
    )

    GOCARTComponent().post_rc_copy(tmp_path, {'AGCM_LM': '72'}, {})

    du_text = (rc_dir / 'DU2G_instance_DU.rc').read_text()
    assert du_text == "#a: 1 # L181\nb: 2 # L072\nc: 3\n"
    ss_text = (rc_dir / 'SS2G_instance_SS.rc').read_text()
    assert ss_text == "#d: 4 # L181\ne: 5 # L072\n"


def test_post_rc_copy_is_noop_away_from_lm_72(tmp_path):
    rc_dir = tmp_path / 'RC'
    rc_dir.mkdir()
    original = "a: 1 # L181\n#b: 2 # L072\n"
    (rc_dir / 'DU2G_instance_DU.rc').write_text(original)
    (rc_dir / 'SS2G_instance_SS.rc').write_text(original)

    GOCARTComponent().post_rc_copy(tmp_path, {'AGCM_LM': '181'}, {})

    assert (rc_dir / 'DU2G_instance_DU.rc').read_text() == original
    assert (rc_dir / 'SS2G_instance_SS.rc').read_text() == original


def test_configure_switches_to_data_driven_instances(tmp_path):
    rc_dir = tmp_path / 'RC'
    rc_dir.mkdir()
    (rc_dir / 'GOCART2G_GridComp.rc').write_text(
        "ACTIVE_INSTANCES_DU: DU\n"
        "ACTIVE_INSTANCES_SS: SS\n"
        "ACTIVE_INSTANCES_SU: SU\n"
        "ACTIVE_INSTANCES_NI: NI\n"
        "ACTIVE_INSTANCES_CA: CA.oc CA.bc CA.br\n"
    )

    GOCARTComponent().configure(tmp_path, {}, {'data_driven': True})

    text = (rc_dir / 'GOCART2G_GridComp.rc').read_text()
    assert "ACTIVE_INSTANCES_DU: DU.data" in text
    assert "ACTIVE_INSTANCES_SS: SS.data" in text
    assert "ACTIVE_INSTANCES_SU: SU.data" in text
    assert "ACTIVE_INSTANCES_NI: NI.data" in text
    # Pre-existing bug (present before this component moved out of
    # setup_main.py; not fixed here to keep the move zero-diff): each
    # ACTIVE_INSTANCES_CA replacement anchors on the text right after the
    # ":", so only the first of three space-separated CA instances on one
    # line is ever switched to .data.
    assert "ACTIVE_INSTANCES_CA: CA.oc.data CA.bc CA.br" in text


def test_configure_is_noop_when_not_data_driven(tmp_path):
    rc_dir = tmp_path / 'RC'
    rc_dir.mkdir()
    original = "ACTIVE_INSTANCES_DU: DU\n"
    (rc_dir / 'GOCART2G_GridComp.rc').write_text(original)

    GOCARTComponent().configure(tmp_path, {}, {'data_driven': False})

    assert (rc_dir / 'GOCART2G_GridComp.rc').read_text() == original

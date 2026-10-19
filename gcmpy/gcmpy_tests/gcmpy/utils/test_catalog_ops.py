"""
CatalogSearch unit tests.

Guards the first-pass/second-pass navigation semantics: the second pass
exists only to cross structural grouping keys (e.g. 'resolution') that are
not config values.  When a config-matched section exists at a level but the
target is absent inside it, the lookup must return "unset" rather than
falling through into sibling sections and leaking another branch's value
(e.g. resolutions -> c12 must not pick up c270's conus/stretch_factor).
"""

from gcmpy.utils.catalog_ops import CatalogSearch


def test_target_found_in_config_matched_section():
    catalog = {'resolutions': {
        'c12': {'im': 12, 'res': 'CF0012x6C'},
        'c270': {'im': 270, 'conus': '', 'stretch_factor': 2.5},
    }}
    cs = CatalogSearch(catalog, {'AGCM_grid': 'c12'})
    assert cs.get_value('im', 'resolutions') == 12
    assert cs.get_value('res', 'resolutions') == 'CF0012x6C'


def test_missing_target_does_not_fall_through_to_sibling_sections():
    catalog = {'resolutions': {
        'c12': {'im': 12},
        'c270': {'im': 270, 'conus': '', 'stretch_factor': 2.5,
                 'num_readers': 6},
    }}
    cs = CatalogSearch(catalog, {'AGCM_grid': 'c12'})
    assert cs.get_value('conus', 'resolutions') is None
    assert cs.get_value('stretch_factor', 'resolutions') is None
    assert cs.get_value('num_readers', 'resolutions') is None


def test_sibling_values_still_resolve_for_matching_config():
    catalog = {'resolutions': {
        'c12': {'im': 12},
        'c270': {'im': 270, 'conus': '', 'stretch_factor': 2.5},
    }}
    cs = CatalogSearch(catalog, {'AGCM_grid': 'c270'})
    assert cs.get_value('conus', 'resolutions') == ''
    assert cs.get_value('stretch_factor', 'resolutions') == 2.5


def test_grouping_key_pass_through_still_works():
    # ocean-style: config-matched branch contains a structural grouping key
    # ('resolution') whose sub-dict is matched by another config value.
    catalog = {'coupled': {
        'MOM6': {
            'gridtyp': 'M6TP',
            'resolution': {
                'o72': {'NX': 3},
                'o540': {'NX': 90},
            },
        },
    }}
    cs = CatalogSearch(catalog, {'OCEAN_type': 'MOM6', 'MOM6_resolution': 'o540'})
    assert cs.get_value('NX', 'coupled') == 90


def test_grouping_pass_through_runs_when_no_config_match():
    # grouping keys themselves never match config values, so the second pass
    # must still cross them when no config-matched sibling exists.
    catalog = {'section': {
        'resolution': {'o1': {'IM': 360}},
    }}
    cs = CatalogSearch(catalog, {'DATAOCEAN_resolution': 'o1'})
    assert cs.get_value('IM', 'section') == 360

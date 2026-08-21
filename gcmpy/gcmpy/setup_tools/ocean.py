from pathlib import Path
import yaml


CATALOG_PATH = Path(__file__).parent / "catalogs" / "ocean_catalog.yaml"

# Sentinel used by _search_recursive to distinguish "found with value None"
# from "key not found at all".
_NOT_FOUND = object()


class OceanCatalog:
    """
    Loads ocean_catalog.yaml and provides catalog lookups driven by expConfig.

    Mirrors the CatalogManager pattern from linkbcs.py: get_value() recursively
    walks a catalog section, using expConfig values as navigation keys to descend
    into matching sub-dicts, until the target key is found.
    """

    def __init__(self, expConfig: dict, catalog_path: Path = CATALOG_PATH):
        self.config = expConfig
        self.catalog = self._load_yaml(catalog_path)

    def _load_yaml(self, path: Path) -> dict:
        with open(path, 'r') as f:
            return yaml.safe_load(f)

    def get_value(self, target: str, section: str):
        """Return the value for target by searching recursively through catalog[section]."""
        result = self._search_recursive(target, self.catalog[section])
        return None if result is _NOT_FOUND else result

    def _search_recursive(self, target: str, catalog_section: dict):
        # Found the target directly in this section — return it only if it is
        # a scalar (not a sub-dict). A dict value means the key is a sub-section
        # header, not a leaf; we keep descending in that case.
        if target in catalog_section and not isinstance(catalog_section[target], dict):
            return catalog_section[target]

        config_values = set(str(v) for v in self.config.values())

        # First pass: descend into sub-dicts whose key matches a config value.
        for section_name, section_data in catalog_section.items():
            if not isinstance(section_data, dict):
                continue
            if section_name not in config_values:
                continue
            result = self._search_recursive(target, section_data)
            if result is not _NOT_FOUND:
                return result

        # Second pass: transparent pass-through for structural grouping keys
        # (e.g. 'resolution') that are not config values but contain sub-dicts
        # that may be navigated by config values one level deeper.
        for section_name, section_data in catalog_section.items():
            if not isinstance(section_data, dict):
                continue
            if section_name in config_values:
                continue
            result = self._search_recursive(target, section_data)
            if result is not _NOT_FOUND:
                return result

        return _NOT_FOUND


# ---------------------------------------------------------------------------
# Private helpers — one per catalog section
# ---------------------------------------------------------------------------

def _build_coupled(ocean: dict, catalog: OceanCatalog, expConfig: dict):
    """Populate ocean dict for coupled (running_ocean=True) configurations."""

    coupled_fields = [
        'coupled', 'tag', 'sst_name', 'sst_file', 'ice_file',
        'ostia', 'out', 'latlon', 'cube', 'data', 'NF',
        # model-specific
        'name', 'gridtyp', 'grid_type', 'preload', 'MOM6', 'MIT',
        # resolution-specific
        'NX', 'NY', 'IM', 'JM', 'LM', 'IMO', 'JMO',
        'res', 'kpar_file', 'gridname', 'nprocs',
    ]

    for field in coupled_fields:
        ocean[field] = catalog.get_value(field, 'coupled')

    # MIT has no preload entry in the catalog; default to empty string so
    # seaice preload concatenation in _build_seaice() works cleanly.
    if ocean.get('preload') is None:
        ocean['preload'] = ''

    ocean['model'] = expConfig['OM_name']

    # Uncoupled-only fields — mark as deleted for template cleanup
    ocean['CICE4']      = '#DELETE'
    ocean['CICE6']      = '#DELETE'
    ocean['hist_CICE4'] = '#DELETE'


def _build_uncoupled(ocean: dict, catalog: OceanCatalog, expConfig: dict):
    """Populate ocean dict for uncoupled (data ocean) configurations."""

    hr_code = expConfig['OM_horizontal_res']

    # Fields that are uniform across all uncoupled configs (top-level uncoupled section)
    common_fields = [
        'name', 'preload', 'LM', 'coupled',
        'MOM6', 'MIT', 'CICE4', 'CICE6', 'hist_CICE4',
        'NX', 'NY', 'nprocs',
    ]
    for field in common_fields:
        ocean[field] = catalog.get_value(field, 'uncoupled')

    # Fields that vary by horizontal resolution sub-key (o1, o2, o3, CS)
    res_fields = [
        'grid_type', 'NF', 'tag', 'sst_name', 'out',
        'gridtyp', 'latlon', 'cube', 'ostia', 'data',
    ]
    for field in res_fields:
        ocean[field] = catalog.get_value(field, 'uncoupled')

    # CS: IM and JM are derived from the atmospheric resolution at runtime
    if hr_code == 'CS':
        ocean['IM'] = int(expConfig['AM_horizontal_res'][1:])
        ocean['JM'] = ocean['IM'] * 6
        IMO = '%04d' % ocean['IM']
        JMO = '%04d' % ocean['JM']
        ocean['IMO']      = IMO
        ocean['JMO']      = JMO
        ocean['res']      = f"CF{IMO}x6C"
        ocean['gridname'] = f"OC{ocean['IM']}x{ocean['JM']}-CF"
    else:
        for field in ['IM', 'JM', 'IMO', 'JMO', 'res', 'gridname']:
            ocean[field] = catalog.get_value(field, 'uncoupled')

    ocean['model'] = f"Data Ocean ({hr_code})"


def _build_seaice(ocean: dict, catalog: OceanCatalog, expConfig: dict):
    """
    Populate seaice fields for coupled configurations.
    For uncoupled configs, seaice fields are already set to #DELETE by
    _build_uncoupled(), so this helper is a no-op in that case.
    """
    if not ocean['running_ocean']:
        return

    seaice_fields = ['seaice_preload', 'CICE4', 'CICE6', 'hist_CICE4']
    for field in seaice_fields:
        ocean[field] = catalog.get_value(field, 'seaice')

    # Append seaice preload to the ocean model preload
    ocean['preload'] = f"{ocean['preload']}:{ocean['seaice_preload']}"


def _build_data_atmosphere(ocean: dict, catalog: OceanCatalog, expConfig: dict):
    """Populate data atmosphere fields from the data_atmosphere catalog section."""

    # The catalog uses string keys 'true'/'false'; normalize the bool expConfig
    # value so _search_recursive can navigate into the correct sub-dict.
    da_str = str(expConfig['OM_data_atmos']).lower()   # 'true' or 'false'

    # Temporarily inject the string key into config values so the recursive
    # search can navigate into the matching 'true'/'false' sub-dict.
    saved = catalog.config
    catalog.config = {**expConfig, '_da_key': da_str}

    for field in ['modelatm', 'use_data_ATM4OCN']:
        ocean[field] = catalog.get_value(field, 'data_atmosphere')

    catalog.config = saved


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

def get_ocean_config(expConfig: dict) -> dict:
    """
    Build and return the ocean configuration dictionary.

    Replaces the Ocean class. Takes the experiment configuration dict and
    returns a flat dict of all ocean-related settings, sourced from
    ocean_catalog.yaml via OceanCatalog, with runtime-derived values
    computed in Python where necessary.

    Parameters
    ----------
    expConfig : dict
        Experiment configuration dictionary (e.g. from gcm_setup questions).

    Returns
    -------
    dict
        Flat ocean configuration dictionary. Keys match the attribute names
        previously set on the Ocean class.
    """
    catalog = OceanCatalog(expConfig)
    ocean = {}

    # Pass-through values from expConfig
    ocean['running_ocean']    = expConfig['OM_coupled']
    ocean['seaice_model']     = expConfig['OM_seaice_model']
    ocean['history_template'] = expConfig['history_template']
    ocean['mpt_shepherd']     = ''   # mutated externally by setup_main if needed

    if ocean['running_ocean']:
        _build_coupled(ocean, catalog, expConfig)
    else:
        _build_uncoupled(ocean, catalog, expConfig)

    _build_seaice(ocean, catalog, expConfig)
    _build_data_atmosphere(ocean, catalog, expConfig)

    return ocean

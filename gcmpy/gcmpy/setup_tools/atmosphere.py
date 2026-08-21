from pathlib import Path
import yaml


CATALOG_PATH = Path(__file__).parent / "catalogs" / "atmosphere_catalog.yaml"

# Sentinel to distinguish "found with value None" from "key not found".
_NOT_FOUND = object()


class AtmosCatalog:
    """
    Loads atmosphere_catalog.yaml and provides catalog lookups driven by expConfig.

    Uses the same recursive search pattern as OceanCatalog: config values are used
    as navigation keys to descend into matching sub-dicts. Structural grouping keys
    (e.g. 'resolutions', 'microphysics') that are not config values are treated as
    transparent pass-throughs in the second pass.
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
        config_values = set(str(v) for v in self.config.values())

        if target in catalog_section:
            value = catalog_section[target]
            # Scalar leaf — return directly.
            if not isinstance(value, dict):
                return value
            # Value-lookup dict (e.g. nx: {MOM6: 5, MIT: 10, data_ocean: 10}):
            # try to resolve via a matching config value, then fall back to data_ocean.
            for k, v in value.items():
                if str(k) in config_values:
                    return v
            if 'data_ocean' in value:
                return value['data_ocean']
            return _NOT_FOUND

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
        # (e.g. 'resolutions', 'microphysics') that are not config values.
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
# Private helpers
# ---------------------------------------------------------------------------

def _build_defaults(atmos: dict, catalog: AtmosCatalog):
    """Apply all defaults from the catalog onto the atmos dict."""
    for key, value in catalog.catalog['defaults'].items():
        atmos[key] = value


def _build_resolution(atmos: dict, catalog: AtmosCatalog, expConfig: dict,
                      ocean_nx=None, ocean_ny=None):
    """Apply resolution-specific fields from the catalog, plus runtime-only fields."""

    # Runtime-only fields — not in catalog
    atmos['dt']          = expConfig['heartbeat']
    atmos['lm']          = int(expConfig['AGCM_LM'])
    atmos['microphysics'] = expConfig['MOIST_version']

    # All other resolution fields come from the catalog.
    # Recursive search navigates: resolutions -> AGCM_grid -> [OCEAN_type sub-dicts]
    res_fields = [
        'im', 'jm', 'hist_im', 'hist_jm',
        'gridfile', 'gridname', 'res_dateline',
        'conv_dt', 'chem_dt', 'dt_solar', 'dt_irrad', 'dt_ocean',
        'job_sgmt', 'num_sgmt', 'post_NDS', 'res',
        'nx_convert', 'ny_convert', 'num_readers',
        'use_SHMEM', 'conus', 'stretch_factor', 'low_res', 'convpar_option',
        'nx', 'ny',
    ]
    for field in res_fields:
        atmos[field] = catalog.get_value(field, 'resolutions')

    # MIT at c180: nx/ny are null in catalog — use runtime ocean values.
    if atmos.get('nx') is None:
        atmos['nx'] = ocean_nx
    if atmos.get('ny') is None:
        atmos['ny'] = ocean_ny


def _build_microphysics(atmos: dict, catalog: AtmosCatalog, expConfig: dict):
    """Apply microphysics fields from the catalog."""
    micro_fields = [
        'BACM_1M', 'GFDL_1M', 'MGB2_2M',
        'RRTMGP_RADIATION', 'RRTMG_RADIATION',
        'conv_dt', 'chem_dt',
    ]
    for field in micro_fields:
        value = catalog.get_value(field, 'microphysics')
        if value is not None:
            atmos[field] = value

    # KLID: only set for BACM_1M; keyed by lm in catalog
    if expConfig['MOIST_version'] == 'BACM_1M':
        lm = atmos['lm']
        if lm >= 181:
            klid_key = '181+'
        elif lm == 137:
            klid_key = '137'
        elif lm == 91:
            klid_key = '91'
        else:
            klid_key = '72'
        atmos['KLID'] = catalog.catalog['microphysics']['BACM_1M']['KLID'][klid_key]


def _build_wsub(atmos: dict, catalog: AtmosCatalog, expConfig: dict):
    """Apply wsub extdata fields from the catalog."""
    for field in ['mp_turn_off_wsub_extdata', 'MP_turnoff_wsub']:
        atmos[field] = catalog.get_value(field, 'wsub_extdata')


def _build_conus(atmos: dict, catalog: AtmosCatalog):
    """Apply conus/stretch grid fields from the catalog."""
    section = 'disabled' if atmos['conus'] == '#' else 'enabled'
    for key, value in catalog.catalog['conus'][section].items():
        atmos[key] = value

    # FV_stretch_fac for enabled case incorporates the runtime stretch_factor value
    if section == 'enabled':
        atmos['FV_stretch_fac'] = f"stretch_fac = {atmos['stretch_factor']}"


def _build_clim(atmos: dict):
    """Set coarse resolution CLIM output dimensions."""
    atmos['CLIM_IM'] = min(576, atmos['hist_im'])
    atmos['CLIM_JM'] = 361 if atmos['CLIM_IM'] == 576 else atmos['hist_jm']


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

def get_atmos_config(expConfig: dict, ocean_nx=None, ocean_ny=None) -> dict:
    """
    Build and return the atmosphere configuration dictionary.

    Parameters
    ----------
    expConfig : dict
        Experiment configuration dictionary.
    ocean_nx : int, optional
        Ocean NX — only needed for MIT coupled at c180.
    ocean_ny : int, optional
        Ocean NY — only needed for MIT coupled at c180.

    Returns
    -------
    dict
        Flat atmosphere configuration dictionary.
    """
    catalog = AtmosCatalog(expConfig)
    atmos = {}

    _build_defaults(atmos, catalog)
    _build_resolution(atmos, catalog, expConfig, ocean_nx, ocean_ny)
    _build_microphysics(atmos, catalog, expConfig)
    _build_wsub(atmos, catalog, expConfig)
    _build_conus(atmos, catalog)
    _build_clim(atmos)

    return atmos

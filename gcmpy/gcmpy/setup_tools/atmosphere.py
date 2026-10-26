from pathlib import Path
import yaml

from gcmpy.utils.catalog_ops import CatalogSearch, load_catalog


CATALOG_PATH = Path(__file__).parent / "catalogs" / "atmosphere_catalog.yaml"


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _build_defaults(atmos: dict, catalog: CatalogSearch):
    """Apply all defaults from the catalog onto the atmos dict."""
    for key, value in catalog.catalog['defaults'].items():
        atmos[key] = value


def _build_resolution(atmos: dict, catalog: CatalogSearch, expConfig: dict,
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
        value = catalog.get_value(field, 'resolutions')
        # None = the resolution section does not define the field; keep the
        # value from defaults (set by _build_defaults) instead of clobbering
        # it.  Also prevents cross-resolution fall-through leaks (e.g. a
        # plain grid inheriting a stretched sibling's conus).
        if value is not None:
            atmos[field] = value

    # Coupled ocean and sea ice must use the actual heartbeat, including user overrides.
    if expConfig.get('OCEAN_coupled'):
        atmos['dt_ocean'] = atmos['dt']

    # MIT at c180: nx/ny are null in catalog — use runtime ocean values.
    if atmos.get('nx') is None:
        atmos['nx'] = ocean_nx
    if atmos.get('ny') is None:
        atmos['ny'] = ocean_ny


def _build_microphysics(atmos: dict, catalog: CatalogSearch, expConfig: dict, catalog_data: dict):
    """Apply microphysics fields from the catalog."""
    micro_fields = [
        'BACM_1M', 'GFDL_1M', 'MGB2_2M',
        'RRTMGP_RADIATION', 'RRTMG_RADIATION',
    ]
    for field in micro_fields:
        value = catalog.get_value(field, 'microphysics')
        if value is not None:
            atmos[field] = value

    # conv_dt/chem_dt overrides exist only under BACM_1M (gcm_setup forces
    # 450 for BACM); other micro choices keep the resolution values. The
    # recursive catalog search would otherwise fall through into the BACM
    # sub-block for every micro choice.
    if expConfig['MOIST_version'] == 'BACM_1M':
        atmos['conv_dt'] = catalog_data['microphysics']['BACM_1M']['conv_dt']
        atmos['chem_dt'] = catalog_data['microphysics']['BACM_1M']['chem_dt']

    # Radiation is keyed off vertical levels (gcm_setup): LM == 72 -> RRTMG,
    # otherwise -> RRTMGP. Overrides the micro-keyed catalog values.
    if atmos['lm'] == 72:
        atmos['RRTMG_RADIATION'] = ''
        atmos['RRTMGP_RADIATION'] = '#DELETE'
    else:
        atmos['RRTMG_RADIATION'] = '#DELETE'
        atmos['RRTMGP_RADIATION'] = ''

    # KLID: only set for GFDL_1M; keyed by lm in catalog
    if expConfig['MOIST_version'] == 'GFDL_1M':
        lm = atmos['lm']
        if lm >= 181:
            klid_key = '181'
        elif lm == 137:
            klid_key = '137'
        elif lm == 91:
            klid_key = '91'
        else:
            klid_key = '72'
        atmos['KLID'] = catalog_data['microphysics']['GFDL_1M']['KLID'][klid_key]


def _build_wsub(atmos: dict, catalog: CatalogSearch, expConfig: dict):
    """Apply wsub extdata fields from the catalog."""
    for field in ['mp_turn_off_wsub_extdata', 'MP_turnoff_wsub']:
        atmos[field] = catalog.get_value(field, 'wsub_extdata')


def _build_conus(atmos: dict, catalog: CatalogSearch, catalog_data: dict):
    """Apply conus/stretch grid fields from the catalog."""
    section = 'disabled' if atmos['conus'] == '#' else 'enabled'
    for key, value in catalog_data['conus'][section].items():
        atmos[key] = value

    # FV_stretch_fac for enabled case incorporates the runtime stretch_factor value
    if section == 'enabled':
        atmos['FV_stretch_fac'] = f"stretch_fac = {atmos['stretch_factor']}"


def _build_fv3_flags(atmos: dict):
    """SHALLOW cumulus and hydrostatic run flags, keyed off LM (gcm_setup)."""
    if atmos['lm'] == 72:
        atmos['SHALLOW_UW'] = ''
        atmos['SHALLOW_SHOC'] = '#'
    else:
        atmos['SHALLOW_UW'] = '#'
        atmos['SHALLOW_SHOC'] = ''

    if atmos['lm'] != 72 and atmos['im'] <= 720:
        atmos['run_hydrostatic'] = 'hydrostatic = .true.'
    else:
        atmos['run_hydrostatic'] = ''


def _build_clim(atmos: dict):
    """Set coarse resolution CLIM output dimensions."""
    atmos['CLIM_IM'] = min(576, atmos['hist_im'])
    atmos['CLIM_JM'] = 361 if atmos['CLIM_IM'] == 576 else atmos['hist_jm']


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

def get_heartbeat_default(expConfig: dict):
    """Return the catalog recommendation; the user's heartbeat remains authoritative."""
    catalog_data = load_catalog(CATALOG_PATH)
    heartbeat = catalog_data['microphysics'][expConfig['MOIST_version']].get('heartbeat')
    if heartbeat is None:
        heartbeat = CatalogSearch(catalog_data, expConfig).get_value('heartbeat', 'resolutions')
    return heartbeat


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
    catalog_data = load_catalog(CATALOG_PATH)
    catalog = CatalogSearch(catalog_data, expConfig)
    atmos = {}

    _build_defaults(atmos, catalog)
    _build_resolution(atmos, catalog, expConfig, ocean_nx, ocean_ny)
    _build_microphysics(atmos, catalog, expConfig, catalog_data)
    _build_wsub(atmos, catalog, expConfig)
    _build_fv3_flags(atmos)
    _build_conus(atmos, catalog, catalog_data)
    _build_clim(atmos)

    return atmos

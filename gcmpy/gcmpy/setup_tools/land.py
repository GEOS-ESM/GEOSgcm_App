from pathlib import Path

from gcmpy.utils.catalog_ops import load_catalog


CATALOG_PATH = Path(__file__).parent / "catalogs" / "land_catalog.yaml"


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _build_defaults(land: dict, catalog: dict):
    """Apply all defaults from the catalog onto the land dict."""
    for key, value in catalog['defaults'].items():
        land[key] = value


def _build_bcs(land: dict, catalog: dict, bcs: str):
    """Apply bcs fields from the catalog."""
    for key, value in catalog['bcs'][bcs].items():
        land[key] = value


def _build_model(land: dict, catalog: dict, model: str):
    """Apply model fields from the catalog."""
    for key, value in catalog['models'][model].items():
        land[key] = value


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

def get_land_config(expConfig: dict) -> dict:
    """
    Build and return the land configuration dictionary.

    Parameters
    ----------
    expConfig : dict
        Experiment configuration dictionary.

    Returns
    -------
    dict
        Flat land configuration dictionary.
    """
    catalog = load_catalog(CATALOG_PATH)
    land = {}

    # Determine bcs and model -- use data_atmos_defaults if data atmosphere is on
    if expConfig['OCEAN_data_atmosphere'] == True:
        bcs   = catalog['data_atmos_defaults']['bcs']
        model = catalog['data_atmos_defaults']['model']
    else:
        bcs   = expConfig['LAND_bcs']
        model = expConfig['LAND_model']

    land['bcs']   = bcs
    land['model'] = model

    _build_defaults(land, catalog)
    _build_bcs(land, catalog, bcs)
    _build_model(land, catalog, model)

    return land

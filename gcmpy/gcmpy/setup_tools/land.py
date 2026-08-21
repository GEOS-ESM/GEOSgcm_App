from pathlib import Path
import yaml


CATALOG_PATH = Path(__file__).parent / "catalogs" / "land_catalog.yaml"


class LandCatalog:
    """
    Loads land_catalog.yaml and provides direct catalog lookups.
    """

    def __init__(self, catalog_path: Path = CATALOG_PATH):
        self.catalog = self._load_yaml(catalog_path)

    def _load_yaml(self, path: Path) -> dict:
        with open(path, 'r') as f:
            return yaml.safe_load(f)


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _build_defaults(land: dict, catalog: LandCatalog):
    """Apply all defaults from the catalog onto the land dict."""
    for key, value in catalog.catalog['defaults'].items():
        land[key] = value


def _build_bcs(land: dict, catalog: LandCatalog, bcs: str):
    """Apply bcs fields from the catalog."""
    for key, value in catalog.catalog['bcs'][bcs].items():
        land[key] = value


def _build_model(land: dict, catalog: LandCatalog, model: str):
    """Apply model fields from the catalog."""
    for key, value in catalog.catalog['models'][model].items():
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
    catalog = LandCatalog()
    land = {}

    # Determine bcs and model — use data_atmos_defaults if data atmosphere is on
    if expConfig['OCEAN_data_atmosphere'] == True:
        bcs   = catalog.catalog['data_atmos_defaults']['bcs']
        model = catalog.catalog['data_atmos_defaults']['model']
    else:
        bcs   = expConfig['LAND_bcs']
        model = expConfig['LAND_model']

    land['bcs']   = bcs
    land['model'] = model

    _build_defaults(land, catalog)
    _build_bcs(land, catalog, bcs)
    _build_model(land, catalog, model)

    return land

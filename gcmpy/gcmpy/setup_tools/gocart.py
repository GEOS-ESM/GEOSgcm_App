from pathlib import Path
import re

from gcmpy.utils.catalog_ops import load_catalog


CATALOG_PATH = Path(__file__).parent / "catalogs" / "gocart_catalog.yaml"


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _build_defaults(gocart: dict, catalog: dict):
    """Apply all defaults from the catalog onto the gocart dict."""
    for key, value in catalog['defaults'].items():
        gocart[key] = value


def _build_aerosol(gocart: dict, catalog: dict, aerosol: str):
    """Apply aerosol fields from the catalog."""
    for key, value in catalog['aerosol'][aerosol].items():
        gocart[key] = value


def _build_emissions(gocart: dict, catalog: dict, emission_key: str):
    """Apply emissions fields from the catalog."""
    for key, value in catalog['emissions'][emission_key].items():
        gocart[key] = value


# ---------------------------------------------------------------------------
# Public entry points
# ---------------------------------------------------------------------------

def get_gocart_config(expConfig: dict) -> dict:
    """
    Build and return the gocart configuration dictionary.

    Note: adjust_tuning_coefficients() is a separate function and must be
    called explicitly by the caller after the experiment RC files exist.

    Parameters
    ----------
    expConfig : dict
        Experiment configuration dictionary.

    Returns
    -------
    dict
        Flat gocart configuration dictionary.
    """
    catalog = load_catalog(CATALOG_PATH)
    gocart = {}

    # Determine aerosol and emission_key -- use data_atmos_defaults if data atmosphere is on
    if expConfig['OCEAN_data_atmosphere'] == True:
        aerosol      = catalog['data_atmos_defaults']['aerosol']
        emission_key = catalog['data_atmos_defaults']['emissions']
    else:
        aerosol      = expConfig['GOCART_aerosols']
        emission_key = expConfig['GOCART_emission']

    gocart['aerosol']   = aerosol
    gocart['emissions'] = f"{emission_key}_EMISSIONS"

    _build_defaults(gocart, catalog)
    _build_aerosol(gocart, catalog, aerosol)
    _build_emissions(gocart, catalog, emission_key)

    return gocart


def adjust_tuning_coefficients(expConfig: dict):
    """
    Switch GOCART2G tuning coefficients to L072 values if AGCM_LM == 72.

    Edits DU2G and SS2G RC files in the experiment directory in-place.
    Must be called after the experiment RC files have been created.

    Parameters
    ----------
    expConfig : dict
        Experiment configuration dictionary. Must contain 'AGCM_LM' and 'exp_dir'.
    """
    if int(expConfig['AGCM_LM']) != 72:
        return

    file_list = [
        Path(f"{expConfig['exp_dir']}/RC/DU2G_instance_DU.rc"),
        Path(f"{expConfig['exp_dir']}/RC/SS2G_instance_SS.rc"),
    ]
    for file in file_list:
        with open(file, 'r') as f:
            content = f.read()

        # Comment out L181 lines
        content = re.sub(r"^(.*# L181)", r"#\1", content, flags=re.MULTILINE)
        # Uncomment L072 lines
        content = re.sub(r"^#(.*# L072)", r"\1", content, flags=re.MULTILINE)

        with open(file, 'w') as f:
            f.write(content)

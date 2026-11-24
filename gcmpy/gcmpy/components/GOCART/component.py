"""
gcmpy.components.GOCART.component

GOCART's gcmpy integration: aerosol/emissions defaults, the L72 tuning-
coefficient swap, and GOCART2G_GridComp.rc's data-driven instances.

This is the pilot for the component plug-in framework
(gcmpy.components.base); see docs/gcmpy/Component-Plugins.md. Everything
here previously lived in gcmpy/setup_tools/gocart.py plus two methods on
SetupGCM (setup_main.py: RC_setup's adjust_tuning_coefficients() call, and
config_gocartGridComp()).

Deliberately left in the central, shared code rather than moved here:
GEOS_ChemGridComp.rc (SetupGCM.config_chemGridComp()) is edited using
self.gocart['rats_provider'], but that file is not GOCART's alone --
ENABLE_PCHEM/ENABLE_TR on the same file are not GOCART settings. A file
with more than one owner stays centralized; see the module docstring of
gcmpy.components.base.
"""

import re
from pathlib import Path

from gcmpy.components.base import GridCompBase
from gcmpy.utils.regex_ops import search_replace_in_string


class GOCARTComponent(GridCompBase):
    def defaults(self, expConfig: dict, catalog: dict) -> dict:
        gocart = {}

        # Data-atmosphere runs skip the GOCART questions (questions.yaml
        # follows_up); use the catalog's recommended combination instead.
        if expConfig['OCEAN_data_atmosphere'] == True:
            aerosol      = catalog['data_atmos_defaults']['aerosol']
            emission_key = catalog['data_atmos_defaults']['emissions']
        else:
            aerosol      = expConfig['GOCART_aerosols']
            emission_key = expConfig['GOCART_emission']

        gocart['aerosol']   = aerosol
        gocart['emissions'] = f"{emission_key}_EMISSIONS"

        for key, value in catalog['defaults'].items():
            gocart[key] = value
        for key, value in catalog['aerosol'][aerosol].items():
            gocart[key] = value
        for key, value in catalog['emissions'][emission_key].items():
            gocart[key] = value

        return gocart

    def post_rc_copy(self, exp_dir: Path, expConfig: dict, component_config: dict) -> None:
        """Switch GOCART2G tuning coefficients to L072 values if AGCM_LM ==
        72. Runs right after RC_setup() copies install/etc into
        <exp_dir>/RC (replaces the old adjust_tuning_coefficients()).
        """
        if int(expConfig['AGCM_LM']) != 72:
            return

        for name in ('DU2G_instance_DU.rc', 'SS2G_instance_SS.rc'):
            path = exp_dir / 'RC' / name
            content = path.read_text()
            # Comment out L181 lines
            content = re.sub(r"^(.*# L181)", r"#\1", content, flags=re.MULTILINE)
            # Uncomment L072 lines
            content = re.sub(r"^#(.*# L072)", r"\1", content, flags=re.MULTILINE)
            path.write_text(content)

    def configure(self, exp_dir: Path, expConfig: dict, component_config: dict) -> None:
        """Enable DATA_DRIVEN GOCART2G instances in GOCART2G_GridComp.rc
        (replaces the old SetupGCM.config_gocartGridComp()).
        """
        if not component_config['data_driven']:
            return

        path = exp_dir / 'RC' / 'GOCART2G_GridComp.rc'
        content = path.read_text()
        for instance in ('DU', 'SS', 'SU', 'NI'):
            content = search_replace_in_string(
                rf'(ACTIVE_INSTANCES_{instance}:\s*){instance}', rf'\1{instance}.data', content,
            )
        # Known bug, carried over unchanged from the pre-move code: each of
        # these anchors on the text right after "ACTIVE_INSTANCES_CA:", so
        # once CA.oc is replaced with CA.oc.data, the CA.bc/CA.br patterns
        # no longer match on the same line (see
        # gcmpy_tests/.../test_gocart_component.py). Not fixed here; this
        # move is meant to be behavior-preserving.
        for suffix in ('oc', 'bc', 'br'):
            content = search_replace_in_string(
                rf'(ACTIVE_INSTANCES_CA:\s*)CA\.{suffix}', rf'\1CA.{suffix}.data', content,
            )
        path.write_text(content)

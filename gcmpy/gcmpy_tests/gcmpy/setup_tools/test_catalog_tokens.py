"""
CMake-token hygiene for the setup catalogs (ocean_catalog.yaml et al.).

The catalogs are configured at build time (catalogs/CMakeLists.txt:
``configure_file(... @ONLY)``) so that the PRELOAD_COMMAND token becomes
``LD_PRELOAD`` (``DYLD_INSERT_LIBRARIES`` on macOS) and the
CMAKE_SHARED_LIBRARY_SUFFIX token becomes ``.so``.  A malformed token --
one missing its closing at-sign -- survives configure_file untouched and
lands verbatim in run_<expid>.yaml as ``ocean.preload``; the run side
then refuses it (mpi_runner._parse_ocean_preload warns and ignores the
value), so a coupled launch silently loses the LD_PRELOAD of
libmom6/libcice6 that gcm_run.j embeds for every coupled run.

Guards:
  1. every at-sign in every catalog participates in a complete token;
  2. a configured coupled MOM6 + CICE6 preload composes and parses into
     the expected LD_PRELOAD environment mapping.
"""

import re
from pathlib import Path

import yaml

from gcmpy.run_tools.mpi_runner import _parse_ocean_preload
from gcmpy.setup_tools.ocean import CATALOG_PATH

TOKEN = re.compile(r"@([A-Za-z0-9_]+)@")

# Stand-in for the build's configure_file() substitution.  Only the
# tokens the catalogs actually use; an unknown token maps to itself so a
# new/misspelled token surfaces as a leftover at-sign rather than a
# KeyError.
CONFIGURE = {
    'PRELOAD_COMMAND': 'LD_PRELOAD',
    'CMAKE_SHARED_LIBRARY_SUFFIX': '.so',
}


def _configured(text: str) -> str:
    return TOKEN.sub(lambda m: CONFIGURE.get(m.group(1), m.group(0)), text)


def test_every_at_sign_belongs_to_a_complete_cmake_token():
    catalog_dir = CATALOG_PATH.parent
    assert catalog_dir.is_dir(), f"missing catalog dir {catalog_dir}"
    for path in sorted(catalog_dir.glob('*.yaml')):
        residue = TOKEN.sub('', path.read_text())
        if '@' in residue:
            i = residue.index('@')
            raise AssertionError(
                f"{path.name}: at-sign outside a complete token: "
                f"...{residue[max(0, i - 40):i + 40]!r}"
            )


def test_configured_coupled_mom6_cice6_preload_parses():
    catalog = yaml.safe_load(_configured(CATALOG_PATH.read_text()))
    ocean_preload = catalog['coupled']['MOM6']['preload']
    seaice_preload = catalog['seaice']['CICE6']['seaice_preload']

    # ocean.py::_build_seaice concatenates the two fragments exactly so.
    env = _parse_ocean_preload(
        f"{ocean_preload}:{seaice_preload}", Path('/geos')
    )
    assert env == {
        'LD_PRELOAD': '/geos/lib/libmom6.so:/geos/lib/libcice6.so',
    }

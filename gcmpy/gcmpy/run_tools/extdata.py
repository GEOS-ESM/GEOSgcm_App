"""
data_staging/extdata.py

Generates the ExtData.yaml file and prunes unused collections (StratChem, GMI, etc.).
NOTE: extdata1g support has been removed. 

Replaces lines 839-885 of gcm_run.j.
"""

import re
import subprocess
from pathlib import Path


# Maps GEOS_ChemGridComp.rc enable-keys to the ExtData file they gate.
_CHEM_COLLECTIONS: list[tuple[str, str]] = [
    ("ENABLE_STRATCHEM", "StratChem_ExtData.rc"),
    ("ENABLE_GMICHEM",   "GMI_ExtData.rc"),
    ("ENABLE_GEOSCHEM",  "GEOSCHEMchem_ExtData.rc"),
    ("ENABLE_CARMA",     "CARMAchem_GridComp_ExtData.rc"),
    ("ENABLE_DNA",       "DNA_ExtData.rc"),
    ("ENABLE_ACHEM",     "ACHEM_ExtData.rc"),
]


def _is_enabled(chem_rc_text: str, key: str) -> bool:
    """Return True if `key: .TRUE.` (case-insensitive) appears in chem_rc_text."""
    pattern = rf"^\s*{re.escape(key)}:\s*\.TRUE\."
    return bool(re.search(pattern, chem_rc_text, re.IGNORECASE | re.MULTILINE))


def prune_chem_collections(rc_dir: Path) -> None:
    """
    Rename unused chem ExtData.yaml files to <name>.NOT_USED.

    Reads GEOS_ChemGridComp.rc and, for each chemistry component that is not
    enabled (.TRUE.), renames the corresponding ExtData file so it is excluded
    from the final ExtData.rc assembly.

    Args:
        rc_dir: Directory containing GEOS_ChemGridComp.rc and the *_ExtData.rc files.
    """
    chem_rc_path = rc_dir / "GEOS_ChemGridComp.rc"
    # gcm_run.j runs `grep ... GEOS_ChemGridComp.rc | wc -l`: when the file
    # is missing, grep fails and wc reports 0, i.e. every component is
    # treated as disabled. Reproduce that instead of raising.
    chem_rc_text = chem_rc_path.read_text() if chem_rc_path.exists() else ""

    for key, filename in _CHEM_COLLECTIONS:
        extdata_file = rc_dir / filename
        if not _is_enabled(chem_rc_text, key) and extdata_file.exists():
            extdata_file.rename(extdata_file.with_suffix(".rc.NOT_USED"))


def patch_wsub_extdata(rc_dir: Path) -> None:
    """
    Redirect WSUB_CLIM to /dev/null for microphysics schemes that don't use it
    (1MOM and GFDL).

    Modifies WSUB_ExtData.yaml in place.

    Args:
        rc_dir: Directory containing WSUB_ExtData.yaml.
    """
    wsub_yaml = rc_dir / "WSUB_ExtData.yaml"
    if not wsub_yaml.exists():
        return

    text = wsub_yaml.read_text()
    patched = re.sub(
        r"(collection:\s*)WSUB_SWclim.*",
        r"\1/dev/null",
        text,
    )
    wsub_yaml.write_text(patched)


def generate_extdata_file(rc_dir: Path, geosbin: Path) -> None:
    """
    Build the YAML-based ExtData collection list and create an empty ExtData.rc.

    Delegates to construct_extdata_yaml_list.py to generate the collection
    list from GEOS_ChemGridComp.rc, then touches ExtData.rc as a placeholder.

    Args:
        rc_dir:  Directory where ExtData.rc will be written.
        geosbin: Path to the GEOS bin directory containing helper scripts.
    """
    extdata_rc = rc_dir / "ExtData.rc"

    if extdata_rc.exists():
        extdata_rc.unlink()

    construct_script = geosbin / "construct_extdata_yaml_list.py"
    try:
        subprocess.run(
            ["python3", str(construct_script), str(rc_dir / "GEOS_ChemGridComp.rc")],
            check=True,
            cwd=rc_dir,
        )
    except (subprocess.CalledProcessError, OSError) as error:
        # gcm_run.j:879-883 runs the constructor bare (no `set -e`), so a
        # failure there doesn't abort the job either -- warn and fall
        # through to the ExtData.rc placeholder below.
        print(f"WARNING: construct_extdata_yaml_list.py failed: {error}")

    # Keep this due to MAPL expectation of an ExtData.rc file, but it is not actually used for anything
    extdata_rc.touch()


def stage_extdata(
    rc_dir: Path,
    geosbin: Path,
    turn_off_wsub: bool = False,
) -> None:
    """
    Top-level entry point: prune unused collections, optionally patch WSUB,
    then assemble ExtData.rc.

    Args:
        rc_dir:        Directory containing GEOS_ChemGridComp.rc and all *_ExtData.rc files.
        geosbin:       Path to the GEOS bin directory.
        turn_off_wsub: If True, patch WSUB_ExtData.yaml to redirect WSUB_CLIM to
                       /dev/null (set for 1MOM/GFDL microphysics schemes).
    """
    prune_chem_collections(rc_dir)

    if turn_off_wsub:
        patch_wsub_extdata(rc_dir)

    generate_extdata_file(rc_dir, geosbin)

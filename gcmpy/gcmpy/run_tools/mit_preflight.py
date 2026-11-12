"""
mit_preflight.py

Run-time check of the MIT/CICE4 boundary inputs, after linkbcs has linked
the segment (csh PR #931: check_mit_inputs). A failure raises before
GEOSgcm.x starts, so a missing input cannot produce a silent dangling link.
"""

from pathlib import Path


MIT_LINKS = (
    "tile.data",
    "runoff.bin",
    "mit.ascii",
    "SEAWIFS_KPAR_mon_clim.data",
    "grid_cice.bin",
    "kmt_cice.bin",
    "INPUT",
)

# Pickup/namelist inputs staged by the user in <exp_dir>/mit_input.
MIT_STAGED_FILES = ("data", "data.exch2")


class MITInputError(RuntimeError):
    """Raised when an MIT/CICE4 input is missing or the tile does not match the grids."""


def read_tile_header(tile) -> list:
    """First field of lines 3-8 of a tile file (AGCM gridname/IM/JM, OGCM gridname/IM/JM)."""
    with open(tile) as f:
        lines = f.readlines()[2:8]
    return [line.split()[0] for line in lines if line.strip()]


def check_mit_inputs(scratch_dir, exp_dir, geosbin, remap_name, data_atmos, resources):
    scratch = Path(scratch_dir)
    exp = Path(exp_dir)
    geos_lib = Path(geosbin).parent / "lib"

    required = list(MIT_LINKS)
    if data_atmos:
        required.append(remap_name)

    missing = [str(scratch / name) for name in required if not (scratch / name).exists()]
    if not any(geos_lib.glob("libCICE4.*")):
        missing.append(str(geos_lib / "libCICE4<suffix>"))
    missing += [str(exp / "mit_input" / name) for name in MIT_STAGED_FILES
                if not (exp / "mit_input" / name).exists()]
    if missing:
        raise MITInputError("Missing MIT/CICE4 input(s): " + ", ".join(missing))

    tile = scratch / "tile.data"
    header = read_tile_header(tile)
    if len(header) != 6:
        raise MITInputError(f"Invalid MIT tile header in {tile}")

    agcm_im = resources["AGCM_IM"]
    agcm_jm = resources["AGCM_JM"]
    expected = [f"PE{agcm_im}x{agcm_jm}-CF", str(agcm_im), str(agcm_jm),
                None, str(resources["OGCM_IM"]), str(resources["OGCM_JM"])]
    if any(want is not None and got != want for got, want in zip(header, expected)):
        raise MITInputError("MIT tile grids do not match the AGCM.rc grids")

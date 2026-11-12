"""
MITgcm LLC ocean setup helpers.

Ports the MIT block of the csh gcm_setup (GEOSgcm_App PR #931): the LLC
profiles and their layout rules, the boundary-condition overrides (GEOS_MIT_*
environment variables and the G12_t4_MITC4 profile), and the setup summary.
The numbers here must agree with ocean_catalog.yaml and atmosphere_catalog.yaml.
"""

MIT_REMAP_NAME = "DC0360xPC0181_LL5400x15-LL.bin"
MIT_BCS_PROFILES = ("standard", "G12_t4_MITC4")

MIT_LLC_FOR_AGCM = {"c90": "llc90", "c180": "llc270"}

MIT_LLC_CHOICES = {
    "llc90": "llc90 (1-deg, 5400x15 compact, 360 tiles)",
    "llc270": "llc270 (1/3-deg, 23040x30 compact, 768 tiles)",
}

G12_T4_MITC4 = {
    "bcs_dir": "/discover/nobackup/projects/gmao/geos_itv/skhani/Ocean_Bathymetry/NEW_LAND_BC_TEST/v13",
    "coupled_dir": "/discover/nobackup/projects/gmao/geos_itv/skhani/Ocean_Bathymetry/NEW_OCI_BC_TEST",
    "land_bcrslv": "CF0180x6C_M6TP1440x1080",
    "tile": "/gpfsm/dnb10/projects/p118/afahad/g12exp/G12_t4_MITC4/restarts/tile_target.data",
    "gridspec": "/discover/nobackup/projects/GEOS_MITgcm/afahad/GEOSMITgcmfiles/mit.ascii",
    "kpar": "/discover/nobackup/projects/GEOS_MITgcm/afahad/GEOSMITgcmfiles/SEAWIFS_KPAR_mon_clim.data",
    "cice_grid": "/gpfsm/dnb06/projects/p185/make_bcs_inputs/ocean/MOM6/1440x1080/cice/grid_cice.bin",
    "cice_kmt": "/gpfsm/dnb06/projects/p185/make_bcs_inputs/ocean/MOM6/1440x1080/cice/kmt_cice.bin",
}

# GEOS_MIT_* overrides, applied after the profile defaults. Same names as the csh script.
MIT_ENV_OVERRIDES = {
    "GEOS_MIT_BCSDIR": "bcs_dir",
    "GEOS_MIT_LAND_BCRSLV": "land_bcrslv",
    "GEOS_MIT_TILE": "tile",
    "GEOS_MIT_RUNOFF": "runoff",
    "GEOS_MIT_GRIDSPEC": "gridspec",
    "GEOS_MIT_REMAP": "remap",
    "GEOS_MIT_KPAR": "kpar",
    "GEOS_MIT_INPUTDIR": "input_dir",
    "GEOS_MIT_CICE_GRID": "cice_grid",
    "GEOS_MIT_CICE_KMT": "cice_kmt",
}


class MITSetupError(ValueError):
    """A MIT configuration that setup must refuse."""


def mit_remap_name(environ) -> str:
    return environ.get("GEOS_MIT_REMAP_NAME", MIT_REMAP_NAME)


def default_llc(agcm_grid):
    return MIT_LLC_FOR_AGCM.get(agcm_grid)


def bcs_profile(site, agcm_grid, environ) -> str:
    profile = "G12_t4_MITC4" if site == "NCCS" and agcm_grid == "c180" else "standard"
    profile = environ.get("GEOS_MIT_BCS_PROFILE", profile)
    if profile not in MIT_BCS_PROFILES:
        raise MITSetupError("GEOS_MIT_BCS_PROFILE must be standard or G12_t4_MITC4.")
    if profile == "G12_t4_MITC4" and (site != "NCCS" or agcm_grid != "c180"):
        raise MITSetupError("G12_t4_MITC4 boundary conditions require C180 on NCCS/Discover.")
    return profile


def validate_setup(agcm_grid, llc, seaice_model, atmos_nx, atmos_ny, ocean_nprocs, site, environ):
    required = default_llc(agcm_grid)
    if required is None:
        raise MITSetupError("You MUST select c90 or c180 atmospheric resolution for MIT ocean!")
    if llc != required:
        raise MITSetupError(
            f"MIT {agcm_grid} requires {required} with its matching compiled configuration.")
    if seaice_model != "CICE4":
        raise MITSetupError("MIT supports only CICE4.")
    if int(atmos_nx) * int(atmos_ny) != int(ocean_nprocs):
        raise MITSetupError("Atmosphere and compiled MIT ocean rank counts must agree.")
    bcs_profile(site, agcm_grid, environ)


def resolve_bcs(site, agcm_grid, ocean, coupled_dir, environ) -> dict:
    """
    Resolve every MIT boundary-condition path. None means "use the stagebcs
    default for this file". Mirrors the csh MIT_CPLDIR / MIT_*FILE logic.
    """
    profile = bcs_profile(site, agcm_grid, environ)
    g12 = profile == "G12_t4_MITC4"
    cpl = G12_T4_MITC4["coupled_dir"] if g12 else f"{coupled_dir}/MIT"
    cpl = environ.get("GEOS_MIT_CPLDIR", cpl)

    sub = f"{ocean['IM']}x{ocean['JM']}"
    remap_name = mit_remap_name(environ)
    bcs = {
        "profile": profile,
        "coupled_dir": cpl,
        "bcs_dir": G12_T4_MITC4["bcs_dir"] if g12 else None,
        "land_bcrslv": G12_T4_MITC4["land_bcrslv"] if g12 else None,
        "tile": G12_T4_MITC4["tile"] if g12 else None,
        "runoff": None,
        "gridspec": G12_T4_MITC4["gridspec"] if g12 else None,
        "kpar": G12_T4_MITC4["kpar"] if g12 else f"{cpl}/{sub}/{ocean['kpar_file']}",
        "input_dir": f"{cpl}/1440x1080/INPUT" if g12 else f"{cpl}/{sub}/INPUT",
        "cice_grid": G12_T4_MITC4["cice_grid"] if g12 else f"{cpl}/{sub}/cice/grid_cice.bin",
        "cice_kmt": G12_T4_MITC4["cice_kmt"] if g12 else f"{cpl}/{sub}/cice/kmt_cice.bin",
        "remap_name": remap_name,
        "remap": f"{cpl}/{remap_name}",
    }

    for env_name, key in MIT_ENV_OVERRIDES.items():
        if env_name in environ:
            bcs[key] = environ[env_name]
    return bcs


def required_build_lines(ocean) -> list:
    return [
        "Required build: BUILD_MIT_OCEAN=ON, MIT_CONFIG_ID=" + ocean["mit_config_id"],
        f"MIT SIZE.h must use sNx=sNy={ocean['JM']}, nPx={ocean['NX']}, nPy=1, nSx=nSy=1, Nr={ocean['LM']}.",
    ]


def setup_summary(agcm_grid, llc, ocean, atmos, bcs) -> str:
    """Text for <exp_dir>/MIT_SETUP.txt (csh parity)."""
    def show(value, default):
        return value if value is not None else f"{default} (stagebcs default)"

    lines = [
        f"Selected profile: {agcm_grid.upper()} / {llc} / CICE4",
        f"Boundary-condition profile: {bcs['profile']}",
        f"Required build: BUILD_MIT_OCEAN=ON; MIT_CONFIG_ID={ocean['mit_config_id']}",
        f"SIZE.h: sNx=sNy={ocean['JM']}; nSx=nSy=1; nPx={ocean['NX']}; nPy=1; Nr={ocean['LM']}",
        f"Atmosphere NX x NY: {atmos['nx']} x {atmos['ny']}; ocean NX x NY: {ocean['NX']} x {ocean['NY']}",
        f"Ocean grid: {ocean['gridname']}; compact dimensions: {ocean['IM']} x {ocean['JM']}",
        f"Tile: {show(bcs['tile'], 'bcs geometry tile')}",
        f"Runoff (must match this tile/land geometry): {show(bcs['runoff'], 'bcs geometry runoff')}",
        f"Grid specification: {show(bcs['gridspec'], 'bcs geometry mit.ascii')}",
        f"Land BC root/tag: {show(bcs['bcs_dir'], 'boundary tiles root')} / {show(bcs['land_bcrslv'], atmos['res'] + '_' + ocean['res'])}",
        f"KPAR: {bcs['kpar']}",
        f"INPUT directory: {bcs['input_dir']}",
        f"CICE4 grid: {bcs['cice_grid']}",
        f"CICE4 mask: {bcs['cice_kmt']}",
        f"Data-atmosphere transform: {bcs['remap']}",
        "Stage the matching MIT data, data.exch2, package namelists, grid/bathymetry,",
        "forcing, and pickups in <exp_dir>/mit_input and <exp_dir>/restarts.",
        "MIT manages its ocean state through pickups; OCEAN_INTERNAL_RESTART_FILE",
        "is disabled. Keep the GEOS openwater/saltwater/CICE4 restarts appropriate",
        "to this grid (5 ice categories and 4 ice layers).",
        "Changing the menu selection does not rebuild the executable. Verify the",
        "actual CMakeCache.txt, SIZE.h, and data.exch2 before launching.",
    ]
    return "\n".join(lines) + "\n"

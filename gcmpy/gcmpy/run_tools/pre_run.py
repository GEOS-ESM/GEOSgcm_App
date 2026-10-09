"""
pre_run.py

Per-segment pre-launch steps ported from gcm_run.j (lines 886-983 and
996-1022): GOCART RRTMGP instance-file renaming, ridge-scheme GWD restart
handling, tile.bin generation, CICE6 pond-state restart flagging, openwater
restart tile-count validation, MERRA2OX pchem date guard, and REPLAY-mode
GAAS/forcing-file linking.

Every function operates on the scratch directory (gcm_run.j cds to
$SCRDIR at line 644 and stays there for the whole segment loop), except
where a GEOSBIN helper executable is needed.

Validation failures keep the original script's exit codes: 7 for the
openwater tile-count mismatch, 8 for the MERRA2OX date guard.
"""

import os
import re
import shlex
import shutil
import subprocess
import sys
from pathlib import Path


def rename_rrtmgp_instance_files(scratch_dir: Path) -> None:
    """
    gcm_run.j:886-894 -- move GOCART over to RRTMGP bands.

    For every `*_instance*.rc` file, rewrite standalone `RRTMG` tokens to
    `RRTMGP` (the `\\b` guard leaves existing `RRTMGP` occurrences alone,
    matching the original sed `'/\\bRRTMG\\b/ s#RRTMG#RRTMGP#'`). Caller
    gates on `atmos.RRTMGP_RADIATION` being enabled.
    """
    for instance_file in sorted(scratch_dir.glob("*_instance*.rc")):
        text = instance_file.read_text()
        new_text = re.sub(r"\bRRTMG\b", "RRTMGP", text)
        if new_text != text:
            instance_file.write_text(new_text)


def configure_gwd_restarts(scratch_dir: Path, rst_by_face: bool) -> None:
    """
    gcm_run.j:901-921 -- get the proper ridge-scheme GWD internal restart.

    With face-decomposed restarts the generated gwd_internal_face_x_rst
    files are used (warning only; the original's face-copy loop is
    commented out in the script itself). Otherwise, a missing
    gwd_internal_rst disables the ridge scheme by forcing NCAR_NRDG to 0
    in AGCM.rc (appending the key if the user never set it).
    """
    if rst_by_face:
        print("WARNING: The generated gwd_internal_face_x_rst are used")
        return

    if (scratch_dir / "gwd_internal_rst").exists():
        return

    print("WARNING: gwd_internal_rst not found. Setting NCAR_NRDG to 0")
    agcm_rc = scratch_dir / "AGCM.rc"
    text = agcm_rc.read_text()
    if "NCAR_NRDG:" not in text:
        if text and not text.endswith("\n"):
            text += "\n"
        text += "NCAR_NRDG: 0\n"
    else:
        text = re.sub(r"^.*NCAR_NRDG:.*$", "NCAR_NRDG: 0", text, flags=re.M)
    agcm_rc.write_text(text)


def ensure_tile_bin(scratch_dir: Path, geosbin: Path) -> None:
    """
    gcm_run.j:923-925 -- build tile.bin from tile.data if absent.

    tile.data itself is a symlink created earlier in the segment by
    apply_linkbcs(); this only handles the (frequent) case where the
    binary .TIL counterpart was not staged in the BCs.
    """
    if (scratch_dir / "tile.bin").exists():
        return

    binary = geosbin / "binarytile.x"
    try:
        subprocess.run(
            [str(binary), "tile.data", "tile.bin"],
            cwd=scratch_dir,
            check=True,
        )
    except FileNotFoundError:
        # csh continues after "Command not found"; so do we (linkbcs.py
        # already warns loudly when tile.bin is the only missing file).
        print(f"WARNING: {binary} not found; cannot create tile.bin")


def configure_cice6_pond_restart(scratch_dir: Path) -> None:
    """
    gcm_run.j:933-942 -- detect the CICE6 pond state in the restart and
    align the `restart_pond_lvl` flag in ice_in accordingly (only reached
    for CICE6-coupled runs).
    """
    iced = scratch_dir / "INPUT" / "iced.nc"
    if iced.exists() and _restart_has_apnd(iced):
        print("pond state in restart, turn on restart flag if not already")
        pattern = r"^\s*restart_pond_lvl\s*=\s*\.false\."
        replacement = "    restart_pond_lvl  = .true."
    else:
        print("pond state NOT in restart, turn off restart flag if already on")
        pattern = r"^\s*restart_pond_lvl\s*=\s*\.true\."
        replacement = "    restart_pond_lvl  = .false."

    ice_in = scratch_dir / "ice_in"
    text = ice_in.read_text()
    ice_in.write_text(re.sub(pattern, replacement, text, flags=re.M))


def _restart_has_apnd(iced: Path) -> bool:
    """True if `apnd` appears anywhere in the netCDF header (ncdump -h)."""
    ncdump = shutil.which("ncdump")
    if ncdump:
        result = subprocess.run(
            [ncdump, "-h", str(iced)], capture_output=True, text=True
        )
        return "apnd" in result.stdout

    # Fallback when the netcdf CLI tools are not on PATH: search the same
    # metadata ncdump -h prints (dimensions, variables, attributes).
    import netCDF4

    with netCDF4.Dataset(iced) as dataset:
        haystack = list(dataset.ncattrs()) + list(dataset.dimensions)
        for name, var in dataset.variables.items():
            haystack.append(name)
            haystack.extend(str(var.getncattr(a)) for a in var.ncattrs())
    return any("apnd" in str(item) for item in haystack)


def validate_openwater_tiles(
    scratch_dir: Path, geosbin: Path, run_cmd: str
) -> None:
    """
    gcm_run.j:943-958 -- verify that openwater_internal_rst holds the same
    number of tiles as tile.data expects (i.e. restarts match the ocean).

    Runs rs_numtiles.x on a single PE when the helper is installed, and
    exits 7 on a mismatch, as the original script does.
    """
    rs_numtiles = geosbin / "rs_numtiles.x"
    if not os.access(rs_numtiles, os.X_OK):
        return

    tile_data = scratch_dir / "tile.data"
    if tile_data.exists():
        expected = sum(
            1
            for line in tile_data.read_text().splitlines()
            if re.match(r"^\s*0", line)
        )
    else:
        # grep on a missing file yields no output -> `wc -l` counts 0.
        expected = 0

    # `$RUN_CMD 1 rs_numtiles.x openwater_internal_rst` -- one PE.
    cmd = shlex.split(run_cmd) + [
        "1",
        str(rs_numtiles),
        "openwater_internal_rst",
    ]
    result = subprocess.run(
        cmd, cwd=scratch_dir, capture_output=True, text=True
    )
    # `grep Total | awk '{print $NF}'` over the tool's stdout.
    found = " ".join(
        line.split()[-1]
        for line in result.stdout.splitlines()
        if "Total" in line
    )

    if str(expected) != found:
        print(
            f"Error! Found {found} tiles in openwater. "
            f"Expect to find {expected} tiles."
        )
        print("Your restarts are probably for a different ocean.")
        sys.exit(7)


def check_merra2ox_dates(scratch_dir: Path) -> None:
    """
    gcm_run.j:960-983 -- guard against running past the MERRA2OX pchem
    climatology's validity end (201706).

    pchem_clim_years == 39 identifies MERRA2OX; exits 8 if the segment
    start date in cap_restart is later than the magic date.
    """
    agcm_rc = scratch_dir / "AGCM.rc"
    pchem_clim_years = None
    for line in agcm_rc.read_text().splitlines():
        if "pchem_clim_years" in line:
            fields = line.split()
            pchem_clim_years = fields[1] if len(fields) > 1 else None
            break

    if pchem_clim_years != "39":
        return

    cap_restart = scratch_dir / "cap_restart"
    lines = cap_restart.read_text().splitlines()
    yearmon = lines[0][:6] if lines else ""
    if yearmon > "201706":
        print(
            "You seem to be using MERRA2OX pchem species file, but your "
            f"simulation date [{yearmon}] is after 201706. This file is "
            "only valid until this time."
        )
        sys.exit(8)


def configure_replay(scratch_dir: Path) -> None:
    """
    gcm_run.j:996-1022 -- in REPLAY mode, redirect the GAAS AOD ExtData
    entries to the analysis experiment's output and symlink its forcing
    files into the run directory.

    No-op unless AGCM.rc sets REPLAY_MODE to Exact or Regular.
    """
    agcm_rc_text = (scratch_dir / "AGCM.rc").read_text()

    def rc_field(name: str) -> str:
        # `grep '^\s*NAME:' AGCM.rc | cut -d: -f2`, csh-word-stripped.
        for line in agcm_rc_text.splitlines():
            if re.match(rf"^\s*{re.escape(name)}:", line):
                parts = line.split(":")
                return parts[1].strip() if len(parts) > 1 else ""
        return ""

    if rc_field("REPLAY_MODE") not in ("Exact", "Regular"):
        return

    ana_expid = rc_field("REPLAY_ANA_EXPID")
    ana_location = rc_field("REPLAY_ANA_LOCATION")

    def file_type(name: str) -> str:
        # `cut -d"/" -f1 | grep -v %` -- first path segment, dropped when
        # it carries a format placeholder.
        first_segment = rc_field(name).split("/")[0]
        return "" if "%" in first_segment else first_segment

    replay_file_type = file_type("REPLAY_FILE")
    replay_file09_type = file_type("REPLAY_FILE09")

    aod_path = f"chem/Y%y4/M%m2/{ana_expid}.aod_"
    for suffix in ("yaml", "rc"):
        path = scratch_dir / f"GAAS_GridComp_ExtData.{suffix}"
        template = scratch_dir / f"GAAS_GridComp_ExtData.{suffix}.tmpl"
        if not path.exists():
            raise FileNotFoundError(
                f"{path.name} not found in {scratch_dir} although "
                "REPLAY_MODE is set"
            )
        text = path.read_text()
        template.write_text(text)
        path.write_text(text.replace("das.aod_", aod_path))

    for directory in ("chem", replay_file_type, replay_file09_type):
        if directory:
            source = f"{ana_location}/{directory}"
        else:
            # `ln -sf ${ANA_LOCATION}/ .` links ANA_LOCATION itself under
            # its own basename when the type came back empty.
            source = f"{ana_location}/"
        _link_force(source, scratch_dir)


def _link_force(source: str, dest_dir: Path) -> None:
    """Mirror `ln -sf <source> .` executed with cwd=dest_dir."""
    name = os.path.basename(os.path.normpath(source)) or source
    dest = dest_dir / name

    if dest.is_symlink() or dest.is_file():
        dest.unlink()
    elif dest.is_dir():
        # ln -sf links *into* an existing directory (dest/name).
        dest = dest / name
        if dest.is_symlink():
            dest.unlink()

    dest.symlink_to(source)

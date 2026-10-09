"""
stage_restarts.py

Identifies and copies the required initial restarts to the scratch directory.
Handles face vs tile restart layout detection and AGCM.rc configuration.

Replaces lines 494-645 of gcm_run.j.
"""

import re
import shutil
import sys
import tarfile
from pathlib import Path

# Tile-based restart components — these are never expanded into face files
# even when rst_by_face is True. (A frozenset is an immutable list)
_TILE_RSTS: frozenset[str] = frozenset({
    "catch",
    "catchcn",
    "route",
    "lake",
    "landice",
    "openwater",
    "saltwater",
    "seaicethermo",
})


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

def _parse_agcm_restarts(agcm_rc_path: Path) -> list[str]:
    """
    Parse restart file names from AGCM.rc.

    Reads all RESTART_FILE entries (excluding VEGDYN and commented lines)
    and returns the list of file names.

    Args:
        agcm_rc_path: Path to AGCM.rc.

    Returns:
        List of restart file name strings.
    """
    text = agcm_rc_path.read_text()
    names = []

    for line in text.splitlines():
        # Skip comments and VEGDYN entries
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if "RESTART_FILE" not in line or "VEGDYN" in line:
            continue

        # Value is everything after the first colon
        parts = line.split(":", 1)
        if len(parts) < 2:
            continue

        name = parts[1].strip().split()[0] if parts[1].strip() else ""
        if not name:
            continue

        names.append(name)

    return names


def _parse_monthly_checkpoints(history_rc_path: Path) -> list[str]:
    """
    Parse monthly checkpoint names from HISTORY.rc.

    Finds all collections with `.monthly: 1` set (ignoring commented lines)
    and returns their names suffixed with `_rst`.

    Args:
        history_rc_path: Path to HISTORY.rc.

    Returns:
        List of monthly checkpoint file name strings.
    """
    text = history_rc_path.read_text()
    names = []

    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        match = re.match(r"^(\S+)\.monthly:\s*1", line)
        if match:
            names.append(match.group(1) + "_rst")

    return names


# ---------------------------------------------------------------------------
# Face detection
# ---------------------------------------------------------------------------

def _detect_rst_by_face(exp_dir: Path, gcmemip: bool, rstdate: str) -> bool:
    """
    Determine whether restarts are stored in face-decomposed format.

    Checks for the presence of `fvcore_internal_face_1_rst`. If both the
    grid-based `fvcore_internal_rst` and a face file coexist, the run is
    aborted with an error message matching the original script's guidance.

    Args:
        exp_dir:  Experiment directory (or restarts subdirectory base).
        gcmemip:  True if this is a GCMEMIP run (restarts live under
                  exp_dir/restarts/{rstdate}/).
        rstdate:  Restart date string used for the GCMEMIP path (e.g. '20000301').

    Returns:
        True if face-based restarts are detected, False otherwise.

    Raises:
        SystemExit: If both grid-based and face-based fvcore restarts coexist.
    """
    if gcmemip:
        rst_dir = exp_dir / "restarts" / rstdate
    else:
        rst_dir = exp_dir

    grid_rst = rst_dir / "fvcore_internal_rst"
    face_rst = rst_dir / "fvcore_internal_face_1_rst"

    if grid_rst.exists() and face_rst.exists():
        print("grid-based internal_rst and internal_face_x_rst should not co-exist")
        print("please remove all *internal_rst except these tile-based restarts:")
        for rst in _TILE_RSTS:
            print(f"  {rst}_internal_rst")
        sys.exit(5)

    return face_rst.exists()


# ---------------------------------------------------------------------------
# Face expansion
# ---------------------------------------------------------------------------

def _expand_face_restarts(rst_names: list[str], rst_by_face: bool) -> list[str]:
    """
    Expand non-tile restart names into per-face variants when using face layout.

    When rst_by_face is True, any restart that is not a tile-based component
    (see _TILE_RSTS) is replaced by six face-numbered variants:
      <base>_face_1_rst ... <base>_face_6_rst

    Tile-based restarts are passed through unchanged.

    If rst_by_face is False, the original list is returned as-is.

    Args:
        rst_names:   List of restart file names (already stripped of +/- markers).
        rst_by_face: True if face-based restart layout is in use.

    Returns:
        Expanded list of restart file names.
    """
    if not rst_by_face:
        return rst_names

    expanded = []
    for rst in rst_names:
        is_tile = any(tile in rst for tile in _TILE_RSTS)
        if is_tile:
            expanded.append(rst)
        else:
            # Strip trailing _rst, append _face_N_rst for each face
            base = re.sub(r"_rst$", "", rst)
            for n in range(1, 7):
                expanded.append(f"{base}_face_{n}_rst")

    return expanded


# ---------------------------------------------------------------------------
# AGCM.rc configuration
# ---------------------------------------------------------------------------

def _set_agcm_flag(agcm_rc_path: Path, key: str, value: str) -> None:
    """Helper function -- Replaces a key: value line in AGCM.rc, ignoring commented lines."""
    text = agcm_rc_path.read_text()
    updated = re.sub(
        rf"^(\s*{re.escape(key)}:\s*).*$",
        rf"\g<1>{value}",
        text,
        flags=re.MULTILINE,
    )
    agcm_rc_path.write_text(updated)


def _get_agcm_int(agcm_rc_path: Path, key: str) -> int:
    """Helper function -- Reads an integer value for a key from AGCM.rc (ignoring comments)."""
    text = agcm_rc_path.read_text()
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("#"):
            continue
        match = re.match(rf"^\s*{re.escape(key)}:\s*(\d+)", line)
        if match:
            return int(match.group(1))
    raise ValueError(f"{key} not found in {agcm_rc_path}")


def configure_face_settings(agcm_rc_path: Path, rst_by_face: bool) -> None:
    """
    Update READ_RESTART_BY_FACE, WRITE_RESTART_BY_FACE, NUM_READERS, and
    NUM_WRITERS in AGCM.rc based on the detected restart layout.

    - If rst_by_face is False, ensures READ_RESTART_BY_FACE is set to NO.
    - If rst_by_face is True, ensures READ_RESTART_BY_FACE is YES and that
      NUM_READERS is a multiple of 6 (defaulting to 6 if not).
    - If WRITE_RESTART_BY_FACE is YES, ensures NUM_WRITERS is a multiple of 6.

    Args:
        agcm_rc_path: Path to AGCM.rc.
        rst_by_face:  True if face-based restarts were detected.
    """
    if not rst_by_face:
        _set_agcm_flag(agcm_rc_path, "READ_RESTART_BY_FACE", "NO")
        return

    # Face-based reads: ensure NUM_READERS is a multiple of 6
    num_readers = _get_agcm_int(agcm_rc_path, "NUM_READERS")
    if num_readers % 6 != 0:
        _set_agcm_flag(agcm_rc_path, "NUM_READERS", "6")

    _set_agcm_flag(agcm_rc_path, "READ_RESTART_BY_FACE", "YES")

    # If face-based writes are also requested, enforce NUM_WRITERS multiple of 6
    try:
        write_by_face_line = ""
        for line in agcm_rc_path.read_text().splitlines():
            if line.strip().startswith("#"):
                continue
            if re.match(r"^\s*WRITE_RESTART_BY_FACE:", line):
                write_by_face_line = line
                break

        if "YES" in write_by_face_line.upper():
            num_writers = _get_agcm_int(agcm_rc_path, "NUM_WRITERS")
            if num_writers % 6 != 0:
                _set_agcm_flag(agcm_rc_path, "NUM_WRITERS", "6")
    except ValueError:
        pass  # NUM_WRITERS or WRITE_RESTART_BY_FACE not present — skip


# ---------------------------------------------------------------------------
# Copying restarts to scratch
# ---------------------------------------------------------------------------

def copy_restarts_to_scratch(
    exp_dir: Path,
    scratch_dir: Path,
    rst_file_names: list[str],
    monthly_chk_names: list[str],
    gcmemip: bool,
    rstdate: str,
    wavewatch: bool = False,
) -> None:
    """
    Copy restart and checkpoint files into the scratch directory.

    Files are copied concurrently (mirroring the original `cp ... &` / `wait`
    pattern). Missing files are silently skipped, matching the original
    `if(-e ...)` guards.

    Args:
        exp_dir:           Experiment directory.
        scratch_dir:       Scratch directory (destination).
        rst_file_names:    Expanded list of restart file names to copy.
        monthly_chk_names: Monthly checkpoint file names to copy.
        gcmemip:           True if this is a GCMEMIP run.
        rstdate:           Restart date string for the GCMEMIP source path.
        wavewatch:         True if WaveWatch3 is enabled (copies restart.ww3).
    """
    if gcmemip:
        src_dir = exp_dir / "restarts" / rstdate
    else:
        src_dir = exp_dir

    all_names = list(rst_file_names) + list(monthly_chk_names)

    for name in all_names:
        src = src_dir / name
        if src.exists():
            shutil.copy2(src, scratch_dir / name)

    if not gcmemip and wavewatch:
        src = src_dir / "restart.ww3"
        if src.exists():
            shutil.copy2(src, scratch_dir / "restart.ww3")


# ---------------------------------------------------------------------------
# Coupled-input staging
# ---------------------------------------------------------------------------

def stage_coupled_input(exp_dir: Path, scratch_dir: Path) -> None:
    """
    Seed scratch INPUT/ from the experiment's RESTART/ directory.

    Replaces gcm_run.j:617-620 (runs once, before the segment loop):

        {{ ocean.coupled }} /bin/mkdir INPUT
        {{ ocean.coupled }} cp $EXPDIR/RESTART/* INPUT

    The model then refreshes INPUT/ from its own scratch RESTART/ output
    after each segment (gcm_run.j:1237, mirrored in archiver.py), but the
    initial contents must come from the experiment. Only files are copied:
    plain csh `cp` without -r fails on subdirectories, and a missing
    $EXPDIR/RESTART makes the glob fail without aborting the script.

    Args:
        exp_dir:    Experiment directory (source RESTART/ lives here).
        scratch_dir: Scratch/run directory (INPUT/ is created here).
    """
    # csh `mkdir INPUT` errors harmlessly when it already exists; the
    # Python equivalent is an idempotent create.
    input_dir = scratch_dir / "INPUT"
    input_dir.mkdir(exist_ok=True)

    restart_dir = exp_dir / "RESTART"
    if not restart_dir.is_dir():
        return

    for item in restart_dir.iterdir():
        if item.is_file():
            shutil.copy2(item, input_dir / item.name)


# ---------------------------------------------------------------------------
# Archiving initial restarts
# ---------------------------------------------------------------------------

def read_edate(cap_restart_path: Path) -> str:
    """
    Derive the edate label from cap_restart.

    cap_restart contains a line like '20000301 210000'. The edate format
    used in restart archive names is 'e{YYYYMMDD}_{HH}z'.

    Args:
        cap_restart_path: Path to the cap_restart file.

    Returns:
        edate string, e.g. 'e20000301_21z'.
    """
    content = cap_restart_path.read_text().strip().split()
    date_str = content[0]          # e.g. '20000301'
    time_str = content[1][:2]      # first two chars of HHMMSS -> HH
    return f"e{date_str}_{time_str}z"


def archive_initial_restarts(
    exp_dir: Path,
    scratch_dir: Path,
    rst_file_names: list[str],
    cap_restart_path: Path,
    expid: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
    coupled: bool = False,
    wavewatch: bool = False,
) -> None:
    """
    Archive the initial restarts into a tar file in the experiment restarts directory.

    If a tar archive for this edate already exists, this step is skipped
    entirely (matching the original `if($numrs == 0)` guard).

    Each restart file is added to the tar as:
      {EXPID}.{rst}.{edate}.{GCMVER}.{BCTAG}_{BCRSLV}

    For coupled runs, the RESTART/ directory is also included.

    Args:
        exp_dir:          Experiment directory.
        scratch_dir:      Scratch directory (source of the current restarts).
        rst_file_names:   Expanded list of restart file names.
        cap_restart_path: Path to cap_restart to read the current date.
        expid:            Experiment ID string.
        gcmver:           GEOS model version string.
        bctag:            Boundary condition tag string.
        bcrslv:           Boundary condition resolution string.
        coupled:          True for coupled (MOM6/CICE) runs — includes RESTART/.
        wavewatch:        True if WaveWatch3 is enabled.
    """
    restarts_dir = exp_dir / "restarts"
    restarts_dir.mkdir(exist_ok=True)

    edate = read_edate(cap_restart_path)

    # Skip if an archive for this date already exists
    existing = list(restarts_dir.glob(f"*{edate}*"))
    if existing:
        return

    suffix = f"{edate}.{gcmver}.{bctag}_{bcrslv}"

    all_rsts = list(rst_file_names) + (["restart.ww3"] if wavewatch else [])

    tar_path = restarts_dir / f"restarts.{edate}.tar"
    with tarfile.open(tar_path, "w") as tar:
        for rst in all_rsts:
            src = scratch_dir / rst
            if not src.exists():
                continue
            tar.add(src, arcname=f"{expid}.{rst}.{suffix}")

        if coupled:
            tar.add(exp_dir / "RESTART", arcname=f"RESTART.{edate}")


# ---------------------------------------------------------------------------
# Top-level entry point
# ---------------------------------------------------------------------------

def fetch_restarts(
    exp_dir: Path,
    scratch_dir: Path,
    agcm_rc_path: Path,
    history_rc_path: Path,
    cap_restart_path: Path,
    expid: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
    gcmemip: bool = False,
    rstdate: str = "",
    coupled: bool = False,
    wavewatch: bool = False,
) -> bool:
    """
    Top-level entry point: parse restart names, detect and configure the face
    layout, copy restarts to scratch (seed coupled INPUT/ from the
    experiment's RESTART/, gcm_run.j:617-620), and archive the initial set.

    Args:
        exp_dir:          Experiment directory.
        scratch_dir:      Scratch directory.
        agcm_rc_path:     Path to AGCM.rc.
        history_rc_path:  Path to HISTORY.rc.
        cap_restart_path: Path to cap_restart.
        expid:            Experiment ID string.
        gcmver:           GEOS model version string.
        bctag:            Boundary condition tag string.
        bcrslv:           Boundary condition resolution string.
        gcmemip:          True if this is a GCMEMIP run.
        rstdate:          Restart date string (required when gcmemip=True).
        coupled:          True for coupled (MOM6/CICE) runs.
        wavewatch:        True if WaveWatch3 is enabled.

    Returns:
        True if restarts are stored in face-decomposed layout. Callers need
        this later (gcm_run.j:901-921 -- ridge-scheme GWD restart handling)
        to decide whether to patch NCAR_NRDG in AGCM.rc.
    """
    # Parse restart and checkpoint names
    rst_names = _parse_agcm_restarts(agcm_rc_path)
    monthly_chk_names = _parse_monthly_checkpoints(history_rc_path)

    # Detect face layout and update AGCM.rc accordingly
    rst_by_face = _detect_rst_by_face(exp_dir, gcmemip, rstdate)
    configure_face_settings(agcm_rc_path, rst_by_face)

    # Expand any non-tile restarts into face-numbered variants if needed
    rst_file_names = _expand_face_restarts(rst_names, rst_by_face)

    # Copy everything to the scratch directory
    copy_restarts_to_scratch(
        exp_dir, scratch_dir, rst_file_names, monthly_chk_names,
        gcmemip, rstdate, wavewatch,
    )

    # gcm_run.j:617-620 -- coupled runs seed scratch INPUT/ from the
    # experiment's RESTART/ directory (before the initial archive below).
    if coupled:
        stage_coupled_input(exp_dir, scratch_dir)

    # Archive the initial restarts (no-op if already archived for this date)
    #if archive == True
    archive_initial_restarts(
        exp_dir, scratch_dir, rst_file_names, cap_restart_path,
        expid, gcmver, bctag, bcrslv, coupled, wavewatch,
    )

    return rst_by_face

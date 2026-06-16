"""
restarts/archiver.py

Post-run logic that takes _checkpoint files, renames them, strips EXPIDs,
and builds the restart tarballs for the next segment.

Replaces lines 1283-1364 of gcm_run.j.
"""

import tarfile
from pathlib import Path

import shutil
from stage_restarts import read_edate


def _stripname(path: Path, old: str, new: str) -> Path:
    """
    Rename a file by replacing the first occurrence of old with new in its name.

    Python equivalent of the $GEOSBIN/stripname utility.

    Args:
        path: Path to the file to rename.
        old:  Substring to replace.
        new:  Replacement string (use '' to simply remove old).

    Returns:
        The new Path after renaming.
    """
    new_path = path.parent / path.name.replace(old, new, 1)
    path.rename(new_path)
    return new_path


def move_intermediate_checkpoints(scr_dir: Path, restarts_dir: Path) -> None:
    """
    Move intermediate checkpoint files (those with an extension) to the
    experiment restarts directory.

    Intermediate checkpoints match the pattern `*_checkpoint.*` — i.e. they
    already have a file extension, distinguishing them from the final
    `*_checkpoint` files which have none.

    Replaces:
        /bin/mv -f *_checkpoint.* ${EXPDIR}/restarts

    Args:
        scr_dir:      Scratch directory containing the checkpoint files.
        restarts_dir: Experiment restarts directory (destination).
    """
    for checkpoint in scr_dir.glob("*_checkpoint.*"):
        shutil.move(str(checkpoint), restarts_dir / checkpoint.name)


def rename_final_checkpoints(
    scr_dir: Path,
    expid: str,
    edate: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
) -> list[Path]:
    """
    Rename final checkpoint files in the scratch directory with the full
    experiment metadata stamp, and replace `_checkpoint` with `_rst` in
    each filename.

    For each `*_checkpoint` file:
      1. Rename to: {EXPID}.{name}.{edate}.{GCMVER}.{BCTAG}_{BCRSLV}.nc4
      2. Replace `_checkpoint` with `_rst` in that stamped name.

    Replaces:
        /bin/mv $checkpoint $EXPID.${checkpoint}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.nc4
        $GEOSBIN/stripname _checkpoint _rst $EXPID.${checkpoint}...

    Args:
        scr_dir: Scratch directory containing `*_checkpoint` files.
        expid:   Experiment ID string.
        edate:   Experiment date label (e.g. 'e20000301_21z').
        gcmver:  GEOS model version string.
        bctag:   Boundary condition tag string.
        bcrslv:  Boundary condition resolution string.

    Returns:
        List of Paths to the renamed *_rst.* files in scr_dir.
    """
    suffix = f"{edate}.{gcmver}.{bctag}_{bcrslv}"
    renamed = []

    for checkpoint in sorted(scr_dir.glob("*_checkpoint")):
        stamped = checkpoint.parent / f"{expid}.{checkpoint.name}.{suffix}.nc4"
        checkpoint.rename(stamped)
        final = _stripname(stamped, "_checkpoint", "_rst")
        renamed.append(final)

    return renamed


def remove_initial_restarts(scr_dir: Path) -> None:
    """
    Remove the initial `*_rst` restart files from the scratch directory.

    These are the restarts that were copied in at the start of the segment
    by stage_restarts.py. They are removed here because they will be
    replaced by the freshly renamed checkpoint files for the next segment.

    Replaces:
        set restarts = `/bin/ls -1 *_rst`
        /bin/rm -f $restarts

    Args:
        scr_dir: Scratch directory.
    """
    for rst in scr_dir.glob("*_rst"):
        rst.unlink()


def archive_wavewatch(
    scr_dir: Path,
    expid: str,
    edate: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
) -> Path | None:
    """
    Handle WaveWatch3 restart archiving.

    Finds the latest numbered WaveWatch3 checkpoint file
    (`restart[0-9][0-9][0-9].ww3`), renames it to the stamped archive name
    in scratch, then removes any remaining intermediate numbered ww3 files.

    The stamped ww3 file remains in scr_dir and is picked up by tar_restarts.

    Replaces:
        set ww3checkpoint = `/bin/ls -1 restart[0-9][0-9][0-9].ww3 | sort -n | tail -n 1`
        /bin/mv -f $ww3checkpoint restart.ww3
        cp restart.ww3 ${EXPDIR}/restarts/$EXPID.restart.ww3.${edate}...bin
        /bin/rm ./restart[0-9][0-9][0-9].ww3

    Args:
        scr_dir: Scratch directory.
        expid:   Experiment ID string.
        edate:   Experiment date label.
        gcmver:  GEOS model version string.
        bctag:   Boundary condition tag string.
        bcrslv:  Boundary condition resolution string.

    Returns:
        Path to the stamped ww3 restart in scr_dir, or None if no ww3 checkpoints found.
    """
    ww3_checkpoints = sorted(scr_dir.glob("restart[0-9][0-9][0-9].ww3"))
    if not ww3_checkpoints:
        return None

    latest = ww3_checkpoints[-1]
    suffix = f"{edate}.{gcmver}.{bctag}_{bcrslv}"
    stamped_ww3 = scr_dir / f"{expid}.restart.ww3.{suffix}.bin"
    latest.rename(stamped_ww3)

    # Remove any remaining intermediate numbered ww3 checkpoints
    for checkpoint in scr_dir.glob("restart[0-9][0-9][0-9].ww3"):
        checkpoint.unlink()

    return stamped_ww3


def strip_restart_names(
    scr_dir: Path,
    expid: str,
    edate: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
) -> None:
    """
    Strip the EXPID prefix and the date/version suffix from the stamped restart
    files in the scratch directory, leaving clean `*_rst` files ready for the
    next segment.

    Two renames per file:
      1. Remove EXPID prefix:
           {EXPID}.moist_rst.{edate}...nc4  →  moist_rst.{edate}...nc4
      2. Remove date/version suffix:
           moist_rst.{edate}...nc4          →  moist_rst

    Replaces:
        $GEOSBIN/stripname $EXPID. '' $restart
        $GEOSBIN/stripname .${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.* '' $restart

    Args:
        scr_dir: Scratch directory containing the stamped restart files.
        expid:   Experiment ID string.
        edate:   Experiment date label.
        gcmver:  GEOS model version string.
        bctag:   Boundary condition tag string.
        bcrslv:  Boundary condition resolution string.
    """
    suffix = f"{edate}.{gcmver}.{bctag}_{bcrslv}"

    for rst in scr_dir.glob(f"{expid}.*_rst.{suffix}.*"):
        # Step 1: strip EXPID prefix
        stripped = _stripname(rst, f"{expid}.", "")
        # Step 2: strip date/version suffix (everything from .{edate} onward)
        marker = f".{suffix}."
        idx = stripped.name.find(marker)
        if idx != -1:
            clean_name = stripped.name[:idx]
            stripped.rename(stripped.parent / clean_name)


def tar_restarts(
    scr_dir: Path,
    restarts_dir: Path,
    expid: str,
    edate: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
    fsegment: str,
    coupled: bool = False,
) -> None:
    """
    Bundle the stamped restart files directly from the scratch directory into a
    tar archive written to the restarts directory, then remove the originals.

    Tars from scr_dir before strip_restart_names runs, so stamped filenames
    are still intact. No intermediate copy to restarts_dir is needed.

    Only runs when fsegment == '00000000', indicating a full-segment run.

    Replaces:
        if( $FSEGMENT == 00000000 ) then
            tar cf restarts.${edate}.tar $EXPID.*.${edate}...
            /bin/rm -rf `ls -d -1 $EXPID.*.${edate}...`
            @COUPLED /bin/rm -rf RESTART.${edate}
        endif

    Args:
        scr_dir:      Scratch directory containing the stamped restart files.
        restarts_dir: Experiment restarts directory (tar is written here).
        expid:        Experiment ID string.
        edate:        Experiment date label.
        gcmver:       GEOS model version string.
        bctag:        Boundary condition tag string.
        bcrslv:       Boundary condition resolution string.
        fsegment:     Segment frequency string (tar only runs if '00000000').
        coupled:      True for coupled (MOM6/CICE) runs — includes RESTART/ dir.
    """
    if fsegment != "00000000":
        return

    suffix = f"{edate}.{gcmver}.{bctag}_{bcrslv}"
    stamped_files = sorted(scr_dir.glob(f"{expid}.*.{suffix}.*"))

    tar_path = restarts_dir / f"restarts.{edate}.tar"
    with tarfile.open(tar_path, "w") as tar:
        for f in stamped_files:
            tar.add(f, arcname=f.name)
        if coupled:
            restart_dir = restarts_dir / f"RESTART.{edate}"
            if restart_dir.exists():
                tar.add(restart_dir, arcname=f"RESTART.{edate}")

    for f in stamped_files:
        f.unlink()

    if coupled:
        restart_dest = restarts_dir / f"RESTART.{edate}"
        if restart_dest.exists():
            shutil.rmtree(restart_dest)


def archive_checkpoints(
    scr_dir: Path,
    exp_dir: Path,
    cap_restart_path: Path,
    expid: str,
    gcmver: str,
    bctag: str,
    bcrslv: str,
    fsegment: str,
    coupled: bool = False,
    wavewatch: bool = False,
) -> None:
    """
    Top-level entry point: rename checkpoints, build the tar archive, then
    strip stamped names in scratch so clean `*_rst` files are ready for the
    next segment.

    Flow:
      1. Move intermediate checkpoints to the restarts directory.
      2. Rename final checkpoints with full experiment stamp in scratch.
      3. Remove stale bare `*_rst` files from scratch.
      4. (Optional) Handle WaveWatch3 restart — stamps ww3 file in scratch.
      5. Tar all stamped files from scratch into restarts_dir.
      6. Strip stamps from files in scratch for the next segment.

    Args:
        scr_dir:          Scratch directory containing post-run checkpoint files.
        exp_dir:          Experiment directory.
        cap_restart_path: Path to cap_restart (used to derive edate).
        expid:            Experiment ID string.
        gcmver:           GEOS model version string.
        bctag:            Boundary condition tag string.
        bcrslv:           Boundary condition resolution string.
        fsegment:         Segment frequency string ('00000000' triggers tarring).
        coupled:          True for coupled (MOM6/CICE) runs.
        wavewatch:        True if WaveWatch3 is enabled.
    """
    restarts_dir = exp_dir / "restarts"
    restarts_dir.mkdir(exist_ok=True)

    edate = read_edate(cap_restart_path)

    if coupled:
        shutil.copytree(scr_dir / "RESTART", restarts_dir / f"RESTART.{edate}")

    move_intermediate_checkpoints(scr_dir, restarts_dir)
    rename_final_checkpoints(scr_dir, expid, edate, gcmver, bctag, bcrslv)
    remove_initial_restarts(scr_dir)

    if wavewatch:
        archive_wavewatch(scr_dir, expid, edate, gcmver, bctag, bcrslv)

    tar_restarts(
        scr_dir, restarts_dir, expid, edate, gcmver, bctag, bcrslv, fsegment, coupled
    )

    strip_restart_names(scr_dir, expid, edate, gcmver, bctag, bcrslv)

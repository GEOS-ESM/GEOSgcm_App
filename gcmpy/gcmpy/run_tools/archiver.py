"""
restarts/archiver.py

Post-run logic that takes _checkpoint files, renames them, strips EXPIDs,
and builds the restart tarballs for the next segment.

Replaces lines 1196-1290 of gcm_run.j.
"""

import subprocess
import tarfile
from pathlib import Path

import shutil
from gcmpy.run_tools.stage_restarts import read_edate


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


def _checkpoint_extension(path: Path) -> str:
    """
    Determine the archive extension for a final checkpoint file.

    Replaces:
        set chk_type = `/usr/bin/file -Lb --mime-type $checkpoint`
        if ( $chk_type =~ "application/octet-stream" ) then ext = bin
        else ext = nc4

    Args:
        path: The checkpoint file to sniff.

    Returns:
        'bin' for application/octet-stream content, 'nc4' otherwise.
        Falls back to 'nc4' (with a warning) if the file(1) utility is
        not available.
    """
    file_tool = shutil.which("file")
    if file_tool is None:
        print(f"WARNING: file(1) not found; assuming nc4 for {path.name}")
        return "nc4"

    result = subprocess.run(
        [file_tool, "-Lb", "--mime-type", str(path)],
        capture_output=True,
        text=True,
    )
    mime_type = result.stdout.strip()
    return "bin" if mime_type == "application/octet-stream" else "nc4"


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
        /bin/mv $checkpoint $EXPID.${checkpoint}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.{bin,nc4}
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
    renamed = []

    for checkpoint in sorted(scr_dir.glob("*_checkpoint")):
        ext = _checkpoint_extension(checkpoint)
        stamped = checkpoint.parent / (
            f"{expid}.{checkpoint.name}.{edate}.{gcmver}."
            f"{bctag}_{bcrslv}.{ext}"
        )
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


def archive_wavewatch(scr_dir: Path) -> Path | None:
    """
    Promote the latest numbered WaveWatch3 checkpoint to the canonical
    clean name, discarding the older numbered ones.

    The stamped archive copy (`$EXPID.restart.ww3.${edate}...bin`) is made
    by archive_checkpoints() into the restarts directory, matching the
    original, which renames in scratch first (clean `restart.ww3`) and then
    copies a stamped version to $EXPDIR/restarts.

    Replaces:
        set ww3checkpoint = `/bin/ls -1 restart[0-9][0-9][0-9].ww3 | sort -n | tail -n 1`
        if( $#ww3checkpoint != 0 ) /bin/mv -f $ww3checkpoint restart.ww3
        cp restart.ww3 ${EXPDIR}/restarts/$EXPID.restart.ww3.${edate}...bin
        /bin/rm ./restart[0-9][0-9][0-9].ww3

    Args:
        scr_dir: Scratch directory.

    Returns:
        Path to scratch's `restart.ww3`, or None if no new numbered
        checkpoint was produced this segment (existing restart.ww3 is
        then left untouched, as in the original).
    """
    ww3_checkpoints = sorted(scr_dir.glob("restart[0-9][0-9][0-9].ww3"))
    if not ww3_checkpoints:
        return None

    ww3_checkpoints[-1].rename(scr_dir / "restart.ww3")

    # Remove any remaining intermediate numbered ww3 checkpoints
    for checkpoint in scr_dir.glob("restart[0-9][0-9][0-9].ww3"):
        checkpoint.unlink()

    return scr_dir / "restart.ww3"


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
    Bundle the stamped restart files from the experiment restarts directory
    into a tar archive, then remove the stamped originals (and the copied
    RESTART.${edate} tree for coupled runs).

    Callers must have copied the stamped files into restarts_dir first --
    matching gcm_run.j, which always copies there (lines 1236-1242) but
    only tars for full-segment runs.

    Replaces:
        cd $EXPDIR/restarts
        if( $FSEGMENT == 00000000 ) then
            tar cf restarts.${edate}.tar $EXPID.*.${edate}...
            /bin/rm -rf `ls -d -1 $EXPID.*.${edate}...`
            @COUPLED /bin/rm -rf RESTART.${edate}
        endif

    Args:
        restarts_dir: Experiment restarts directory (source and tar target).
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
    stamped_files = sorted(restarts_dir.glob(f"{expid}.*.{suffix}.*"))

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
    Top-level entry point: rename checkpoints, copy the stamped results to
    the experiment restarts directory, build the tar archive, then strip
    stamped names in scratch so clean `*_rst` files are ready for the
    next segment.

    Flow (gcm_run.j line references):
      1. (1237-1238, coupled) Copy RESTART/ into restarts/RESTART.${edate}
         and its contents into scratch INPUT/.
      2. (1239) Move intermediate checkpoints to the restarts directory.
      3. (1245) Rename final checkpoints with full experiment stamp in
         scratch (bin/nc4 chosen by sniffing).
      4. (1260) Remove stale bare `*_rst` files from scratch.
      5. (1288) Promote the latest ww3 checkpoint to clean `restart.ww3`
         in scratch (the stamped copy lands in step 6).
      6. (1236-1242) Copy all stamped files into the restarts directory
         (always, even for forecast-segment runs).
      7. (1302-1311) Tar the stamped files (full segments only).
      8. (1274-1286) Strip stamps from files in scratch for the next
         segment.

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
    suffix = f"{edate}.{gcmver}.{bctag}_{bcrslv}"

    if coupled:
        restart_src = scr_dir / "RESTART"
        if restart_src.exists():
            shutil.copytree(restart_src, restarts_dir / f"RESTART.{edate}")
            # cp RESTART/* INPUT
            input_dir = scr_dir / "INPUT"
            if input_dir.is_dir():
                for item in restart_src.iterdir():
                    if item.is_file():
                        # Initial INPUT files may be links to the user's restarts.
                        if (input_dir / item.name).is_symlink():
                            (input_dir / item.name).unlink()
                        shutil.copy2(item, input_dir / item.name)
            else:
                print(f"WARNING: {input_dir} not found; skipping RESTART/* INPUT")

    move_intermediate_checkpoints(scr_dir, restarts_dir)
    rename_final_checkpoints(scr_dir, expid, edate, gcmver, bctag, bcrslv)
    remove_initial_restarts(scr_dir)

    ww3_restart = None
    if wavewatch:
        ww3_restart = archive_wavewatch(scr_dir)

    # gcm_run.j:1236-1242 -- stamped restarts are copied into the
    # experiment restarts directory for every segment; only the tar below
    # is conditional on a full-segment run.
    for stamped in sorted(scr_dir.glob(f"{expid}.*_rst.{suffix}.*")):
        shutil.copy2(stamped, restarts_dir / stamped.name)
    if ww3_restart is not None:
        shutil.copy2(
            ww3_restart, restarts_dir / f"{expid}.restart.ww3.{suffix}.bin"
        )

    tar_restarts(
        restarts_dir, expid, edate, gcmver, bctag, bcrslv, fsegment, coupled
    )

    strip_restart_names(scr_dir, expid, edate, gcmver, bctag, bcrslv)

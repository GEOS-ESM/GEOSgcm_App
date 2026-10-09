"""
post_run.py

Post-segment steps ported from gcm_run.j (lines 1314-1431): moving HISTORY
files to the holding directory, moving MOM6/CICE6 output files out of
scratch, triggering post-processing / cycled forecasts, and copying the
segment's finished restarts back into the experiment directory so the next
job (or a manual restart) can pick them up.

Note on `rc`: gcm_run.j hard-exits on GEOSgcm.x failure (lines 1127-1138)
and only ever assigns `rc = 0` on the surviving path, so its
`if ($rc == 0)` guards around post-processing are always true -- everything
below belongs to the success path only (failures surface as
GEOSRunError/EgressMissingError from mpi_runner.run_gcm() and abort the
job, exactly as the original's `exit $run_status` does).
"""

import os
import shutil
import subprocess
from datetime import timedelta
from pathlib import Path

from gcmpy.utils.clock_ops import create_clock


def move_history_to_holding(
    scratch_dir: Path, exp_dir: Path, collections: list[str]
) -> None:
    """
    Move this segment's HISTORY files from scratch into the experiment's
    per-collection holding directories.

    Replaces (gcm_run.j:1314-1333):
        cd $SCRDIR
        foreach collection ( $collections )
           /bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection
        end

    Args:
        scratch_dir: Scratch directory holding the finished output files.
        exp_dir:     Experiment directory (holding/ is created at setup).
        collections: HISTORY collection names (job_ctx['collections']).
    """
    for collection in collections:
        holding_dir = exp_dir / "holding" / collection
        for history_file in sorted(scratch_dir.glob(f"*.{collection}.*")):
            shutil.move(str(history_file), holding_dir / history_file.name)


def move_ocean_outputs(
    scratch_dir: Path,
    exp_dir: Path,
    edate: str,
    mom6: bool,
    cice6: bool,
) -> None:
    """
    Move coupled-ocean model output out of scratch (gcm_run.j:1337-1352).

    MOM6:  ocean_state/prog_z/sfc_ave/forcing .nc -> MOM_Output/<dset>.<edate>.nc
    CICE6: iceh.*.nc                              -> CICE_Output/

    (The MOM5 and MITgcm variants in the original are unsupported/deferred
    elsewhere in this port, so they are not reproduced here.)

    Args:
        scratch_dir: Scratch directory containing the model output.
        exp_dir:     Experiment directory.
        edate:       Experiment date label for this segment.
        mom6:        True for MOM6-coupled runs.
        cice6:       True for CICE6-coupled runs.
    """
    if mom6:
        for dset in ("ocean_state", "prog_z", "sfc_ave", "forcing"):
            src = scratch_dir / f"{dset}.nc"
            if src.exists():
                mom_output = exp_dir / "MOM_Output"
                mom_output.mkdir(exist_ok=True)
                shutil.move(str(src), mom_output / f"{dset}.{edate}.nc")

    if cice6:
        ice_files = sorted(scratch_dir.glob("iceh.*.nc"))
        if ice_files:
            cice_output = exp_dir / "CICE_Output"
            cice_output.mkdir(exist_ok=True)
            for ice_file in ice_files:
                shutil.move(str(ice_file), cice_output / ice_file.name)


def run_post_processing(exp_dir: Path, install_path: str) -> None:
    """
    Run the GEOS post-processing script (gcm_run.j:1356-1361).

    Replaces:
        $GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles

    A missing script (e.g. in stripped-down installs) is reported and
    skipped, mirroring csh's non-fatal "Command not found".

    Args:
        exp_dir:      Experiment directory.
        install_path: GEOS install path ($GEOSUTIL), run_config['install_path'].
    """
    if not install_path:
        print("WARNING: install_path not set; skipping gcmpost.script")
        return

    script = Path(install_path) / "post" / "gcmpost.script"
    if not script.exists():
        print(f"WARNING: {script} not found; skipping post-processing")
        return

    try:
        result = subprocess.run(
            [str(script), "-source", str(exp_dir), "-movefiles"]
        )
    except OSError as exc:
        print(f"WARNING: could not run {script}: {exc}")
        return

    if result.returncode != 0:
        print(f"WARNING: {script} exited with status {result.returncode}")


def run_forecast_setup(
    scratch_dir: Path,
    exp_dir: Path,
    fsegment: str,
    nymdc: str,
    nhmsc: str,
) -> None:
    """
    Trigger a cycled-REPLAY forecast for this segment
    (gcm_run.j:1363-1378; gated on FCST_SEGMENT != 00000000).

    Replaces:
        set nday  = `echo $FSEGMENT | bc`
        @ dt      = 10800 - 86400 * $nday
        set date  = `$GEOSBIN/tick $nymdc $nhmsc $dt`      (-> create_clock)
        if( $nymdz >= REPLAY_BEG_DATE & $nymdz <= REPLAY_END_DATE ) then
            setenv CYCLED .TRUE.
            $EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $nday TRUE
        endif

    The forecast script is read from $EXPDIR/forecasts and is run with
    cwd=$SCRDIR (the original's working directory at that point).

    Args:
        scratch_dir: Scratch directory (cwd for the forecast script).
        exp_dir:     Experiment directory; CAP.rc is read from here ($HOMDIR).
        fsegment:    CAP.rc FCST_SEGMENT value; '00000000' means no forecast.
        nymdc:       Segment start date (YYYYMMDD) read from cap_restart
                     *before* GEOSgcm.x advanced it (gcm_run.j:691-692).
        nhmsc:       Segment start time (HHMMSS), same provenance.
    """
    if fsegment == "00000000":
        return

    beg_date = _rc_field(exp_dir / "CAP.rc", "BEG_REPDATE")
    end_date = _rc_field(exp_dir / "CAP.rc", "END_REPDATE")

    nday = int(fsegment)  # `echo $FSEGMENT | bc`
    dt = 10800 - 86400 * nday
    clock = create_clock(nymdc, nhmsc) + timedelta(seconds=dt)
    nymdz = clock.strftime("%Y%m%d")

    # csh compares these numerically; equal-width YYYYMMDD strings sort
    # identically. Placeholder values (e.g. the literal 'YYYYMMDD' shipped
    # in template CAP.rc files) cannot be compared -- warn and skip.
    try:
        in_window = int(beg_date) <= int(nymdz) <= int(end_date)
    except ValueError:
        print(
            f"WARNING: non-numeric REPLAY window [{beg_date}, {end_date}]; "
            "skipping forecast setup"
        )
        return

    if not in_window:
        return

    script = exp_dir / "forecasts" / "gcm_forecast.setup"
    if not script.exists():
        print(f"WARNING: {script} not found; skipping forecast setup")
        return

    env = {**os.environ, "CYCLED": ".TRUE."}
    subprocess.run(
        [str(script), nymdz, nymdz, str(nday), "TRUE"],
        env=env,
        cwd=scratch_dir,
    )


def _rc_field(rc_path: Path, name: str) -> str:
    r"""Value after the first colon of the `^\s*NAME:` line (cut -d: -f2)."""
    if not rc_path.exists():
        return ""
    for line in rc_path.read_text().splitlines():
        if line.lstrip().startswith(f"{name}:"):
            parts = line.split(":")
            return parts[1].strip() if len(parts) > 1 else ""
    return ""


def copy_restarts_to_exp_dir(
    scratch_dir: Path,
    exp_dir: Path,
    gcmemip: bool,
    rstdate: str,
    wavewatch: bool,
    coupled: bool,
) -> None:
    """
    Copy the segment's finished restarts back to the experiment directory
    after the segment loop (gcm_run.j:1403-1431).

    GCMEMIP runs restore into restarts/$RSTDATE/ (the directory the job
    started from); normal runs restore into the experiment directory
    itself. Existing destination files are removed first (`rm -f` then
    `cp`), and the WaveWatch3 restart follows only for non-GCMEMIP runs,
    matching the original's branch structure. Coupled runs additionally
    push the RESTART/ directory back with `cp -rf`.

    Args:
        scratch_dir: Scratch directory containing clean `*_rst` files
                     and cap_restart.
        exp_dir:     Experiment directory.
        gcmemip:     True for GCMEMIP runs.
        rstdate:     Restart date label (GCMEMIP destination subdirectory).
        wavewatch:   True if WaveWatch3 is enabled (non-GCMEMIP only).
        coupled:     True for coupled runs (RESTART/ push-back).
    """
    if gcmemip:
        dest_dir = exp_dir / "restarts" / rstdate
    else:
        dest_dir = exp_dir
    dest_dir.mkdir(parents=True, exist_ok=True)

    for restart in sorted(scratch_dir.glob("*_rst")):
        (dest_dir / restart.name).unlink(missing_ok=True)
        shutil.copy2(restart, dest_dir / restart.name)

    (dest_dir / "cap_restart").unlink(missing_ok=True)
    shutil.copy2(scratch_dir / "cap_restart", dest_dir / "cap_restart")

    if wavewatch and not gcmemip:
        ww3_restart = scratch_dir / "restart.ww3"
        if ww3_restart.exists():
            (exp_dir / "restart.ww3").unlink(missing_ok=True)
            shutil.copy2(ww3_restart, exp_dir / "restart.ww3")

    if coupled:
        restart_dir = scratch_dir / "RESTART"
        if restart_dir.exists():
            shutil.copytree(restart_dir, exp_dir / "RESTART", dirs_exist_ok=True)
        else:
            print(f"WARNING: {restart_dir} not found; skipping RESTART push-back")

#!/usr/bin/env python3
"""
  * If neither SLURM_JOB_ID nor PBS_JOBID is set, we're on a login node (by a
    human, or by gcm_setup.py right after it writes run_<expid>.yaml) to
    kick off the very first segment. We build the equivalent `sbatch`
    command-line flags from the site/experiment batch parameters that
    used to be Jinja-rendered into the #SBATCH header (batch_time/run_t,
    run_p, run_q, batch_jobname/run_n, batch_group_full,
    batch_outputname, ...) and submit the (static, non-templated)
    gcm_run.sh job script via `sbatch` or `qsub`. Node counts are refreshed
    from AGCM.rc for every submission.

  * If SLURM_JOB_ID or PBS_JOBID is set, we're inside an allocated batch job
    (invoked by gcm_run.sh). We run the actual per-segment GCM run loop
    (staging restarts, emissions, ExtData, launching GEOSgcm.x, archiving
    restarts/checkpoints, ...), and -- if more segments remain --
    resubmit for the next segment using the same submit_job() code path
    (via subprocess, not by looping back through shell) -- see execute().

Both paths are implemented: submit_job() handles the login-node sbatch
submission, and execute() runs the full in-job segment loop (staging
restarts, emissions, ExtData, launching GEOSgcm.x, archiving
restarts/checkpoints, post-processing, copy-back, resubmission) by
calling into the gcmpy.run_tools modules ported from gcm_run.j, with
original gcm_run.j line-number citations throughout. A few areas remain
deliberately deferred (singularity execution, MIT/MOM5 ocean variants)
and are documented at their call sites.
"""

import argparse
import math
import os
import re
import subprocess
import sys
from pathlib import Path

import yaml

from gcmpy.batch_tools.batch_environ import load_env_vars, get_batch_resources
from gcmpy.run_tools.agcm_file_ops import (
    get_nx_ny, get_ogcm_coupled_overrides, get_ioserver_params,
)
from gcmpy.utils.flag_ops import is_enabled


# ---------------------------------------------------------------------------
# Config I/O
# ---------------------------------------------------------------------------

def validate_yaml_file(file_path: str) -> str:
    path = Path(file_path)

    if not path.exists():
        raise argparse.ArgumentTypeError(f"File does not exist: {file_path}")

    if path.suffix.lower() not in [".yaml", ".yml"]:
        raise argparse.ArgumentTypeError(
            f"File must have .yaml or .yml extension: {file_path}"
        )

    return file_path


def capture_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--config",
        required=True,
        type=validate_yaml_file,
        help="Experiment run_<expid>.yaml configuration file",
    )

    return parser.parse_args()


def import_yaml(yaml_file: Path) -> dict:
    with open(yaml_file, "r") as f:
        return yaml.safe_load(f)


# ---------------------------------------------------------------------------
# Login-node path: translate run_<expid>.yaml batch params into sbatch flags
# ---------------------------------------------------------------------------

def _sbatch_args_from_field(value) -> list:
    """
    Turn one of the site-specific batch-header strings already computed by
    setup_main.py::set_batch_params() (e.g. "SBATCH --nodes=4
    --ntasks-per-node=40") into a list of plain `sbatch` CLI arguments
    (e.g. ["--nodes=4", "--ntasks-per-node=40"]).

    These values used to be written verbatim (with a leading "#") into the
    #SBATCH header of gcm_run.j via Jinja2. Here we strip the "SBATCH "
    prefix and hand the rest straight to the `sbatch` command line instead,
    so no file templating is needed.

    Values of "DELETE"/"NULL" (used as placeholders for sites/fields that
    don't apply, e.g. PBS-only fields on SLURM sites) are skipped. Values
    that still use PBS syntax (site == "NAS") are also skipped here --
    PBS headers are handled separately by build_qsub_args().
    """
    if not value:
        return []

    value = str(value).strip()

    if value in ("DELETE", "NULL", ""):
        return []

    if value.startswith("SBATCH "):
        value = value[len("SBATCH "):]
    elif value.startswith("PBS "):
        # PBS-style header values (NAS) aren't translatable to sbatch flags.
        return []

    return value.split()


def submission_nodes(run_config: dict, per_node: int) -> tuple[int, int]:
    """Read the current layout without using the submitting job's allocation."""
    if per_node <= 0:
        raise ValueError('Tasks per node must be positive')
    agcm_rc = Path(run_config['exp_dir']) / 'AGCM.rc'
    if not agcm_rc.is_file():
        raise FileNotFoundError(f'Cannot size batch allocation: {agcm_rc} is missing')
    nx, ny = get_nx_ny(agcm_rc)
    if is_enabled(run_config['ocean']['coupled']):
        nx, ny, _ = get_ogcm_coupled_overrides(agcm_rc)
    if nx <= 0 or ny <= 0:
        raise ValueError('AGCM.rc NX and NY must be positive')
    model_pes = nx * ny
    nodes = math.ceil(model_pes / per_node)
    total_pes = model_pes
    if int(run_config['expConfig']['io_server']) == 1:
        io_nodes, _ = get_ioserver_params(agcm_rc)
        if io_nodes < 0:
            raise ValueError('AGCM.rc IOSERVER_NODES must not be negative')
        nodes += io_nodes
        total_pes = nodes * per_node
    return nodes, total_pes


def build_sbatch_args(run_config: dict) -> list:
    """
    Build the full list of `sbatch` resource flags for the run job from an
    experiment's run_<expid>.yaml dictionary.
    """
    args = []

    args += _sbatch_args_from_field(
        f"{run_config.get('batch_time', '')}{run_config.get('run_t', '')}"
    )
    parser = argparse.ArgumentParser(add_help=False, allow_abbrev=False)
    parser.add_argument('--nodes', '-N')
    parser.add_argument('--ntasks', '-n')
    parser.add_argument('--ntasks-per-node', type=int)
    options, other_args = parser.parse_known_args(
        _sbatch_args_from_field(run_config.get('run_p'))
    )
    per_node = options.ntasks_per_node
    if per_node is None:
        per_node = int(run_config['num_CPUs'])
    nodes, total_pes = submission_nodes(run_config, per_node)
    args += [f'--nodes={nodes}', f'--ntasks-per-node={per_node}']
    if options.ntasks is not None:
        args.append(f'--ntasks={total_pes}')
    args += other_args
    args += _sbatch_args_from_field(
        f"{run_config.get('batch_jobname', '')}{run_config.get('run_n', '')}"
    )
    args += _sbatch_args_from_field(run_config.get("run_q"))
    args += _sbatch_args_from_field(run_config.get("batch_group_full"))

    output_name = "gcm_run.o%j"
    args += _sbatch_args_from_field(
        f"{run_config.get('batch_outputname', '')}{output_name}"
    )

    return args


def build_qsub_args(run_config: dict) -> list:
    """Translate generated NAS headers, resizing the homogeneous select request."""
    def field(value):
        value = str(value or '').strip()
        if value in ('DELETE', '#DELETE', 'NULL', ''):
            return []
        if not value.startswith('PBS '):
            raise ValueError(f'Expected a PBS header, got {value!r}')
        return value[4:].split()

    resource_args = field(run_config['run_p'])
    selects = [arg for arg in resource_args if arg.startswith('select=')]
    if len(selects) != 1 or not re.fullmatch(r'select=\d+(?::[^+\s]+)*', selects[0]):
        raise ValueError('Expected one homogeneous PBS select request in run_p')
    select = selects[0]
    qualifiers = dict(part.split('=', 1) for part in select.split(':')[1:])
    per_node = int(qualifiers.get('mpiprocs', qualifiers.get('ncpus', run_config['num_CPUs'])))
    nodes, _ = submission_nodes(run_config, per_node)
    resized = re.sub(r'^select=\d+', f'select={nodes}', select)
    resource_args[resource_args.index(select)] = resized
    return (
        field(f"{run_config['batch_time']}{run_config['run_t']}")
        + resource_args
        + field(f"{run_config['batch_jobname']} {run_config['run_n']}")
        + field(run_config.get('run_q'))
        + field(run_config.get('batch_group_full'))
        + field(run_config.get('batch_joinouterr'))
    )


def submit_job(config_path: Path, run_config: dict) -> subprocess.CompletedProcess:
    """
    Submit gcm_run.sh via the configured scheduler, resizing from AGCM.rc.
    Slurm receives the config as a script argument; PBS receives it via -v.
    """
    bin_path = run_config.get("bin_path")
    if not bin_path:
        raise KeyError(
            "run_config is missing 'bin_path' (GEOSBIN) -- cannot locate gcm_run.sh"
        )

    gcm_run_sh = Path(bin_path) / "gcm_run.sh"
    batch_cmd = run_config.get('batch_cmd', 'sbatch')
    if batch_cmd == 'qsub':
        # qsub does not accept script arguments; pass the config via the environment.
        config_path = Path(config_path).resolve()
        if ',' in str(config_path) or ',' in str(bin_path):
            raise ValueError('PBS environment paths must not contain commas')
        cmd = [
            'qsub', *build_qsub_args(run_config), '-V', '-S', '/bin/bash',
            '-v', f'GEOSBIN={bin_path},GEOS_RUN_CONFIG={config_path}',
            str(gcm_run_sh),
        ]
        print(f"Submitting: {' '.join(cmd)}")
        return subprocess.run(cmd, check=True)
    if batch_cmd != 'sbatch':
        raise ValueError(f'Unsupported batch command: {batch_cmd!r}')
    sbatch_args = build_sbatch_args(run_config)

    # The job script sources ${GEOSBIN}/g5_modules.sh, but inside a SLURM
    # job BASH_SOURCE points at the spool copy -- so GEOSBIN must arrive via
    # the submission environment rather than be rediscovered in-job.
    cmd = [
        "sbatch",
        *sbatch_args,
        f"--export=ALL,GEOSBIN={bin_path}",
        str(gcm_run_sh),
        str(config_path),
    ]
    print(f"Submitting: {' '.join(cmd)}")

    return subprocess.run(cmd, check=True)


# ---------------------------------------------------------------------------
# In-job path: the actual per-segment GCM run loop
# ---------------------------------------------------------------------------

def run_job_setup(config_path: Path, run_config: dict) -> dict:
    """
    Perform the once-per-job setup (gcm_run.j lines ~65-645): resource
    resolution, GCMEMIP staging, experiment/HISTORY directories, scratch
    directory creation+population, executable setup, and initial restart
    staging.

    This corresponds to everything in gcm_run.j that happens *before* the
    `while ( $counter <= ${NUM_SGMT} )` segment loop starts (gcm_run.j:709).
    The segment loop itself lives in run_segments() below, which calls this
    once and then iterates up to NUM_SGMT times within the same job --
    matching the original script's job/resubmit cadence (one job runs up
    to NUM_SGMT segments before resubmitting), rather than resubmitting a
    fresh job for every single segment.

    Returns:
        dict ("job context") bundling everything the per-segment loop needs:
        exp_dir, geosbin, scratch_dir, geosexe, singularity_run, resources,
        segment_control, expid, gcmver, bctag, bcrslv, coupled, wavewatch,
        collections, gcmemip, rstdate, rst_by_face.
    """
    # gcm_run.j:178-273 -- experiment run parameters + CPU/node over-spec check.
    from gcmpy.run_tools.run_resources import resolve_run_resources
    from gcmpy.utils.flag_ops import is_enabled

    exp_dir = Path(run_config["exp_dir"])
    geosbin = Path(run_config["bin_path"])
    agcm_rc_file = exp_dir / "AGCM.rc"
    resources = resolve_run_resources(agcm_rc_file, run_config)

    # gcm_run.j:273-348 -- GCMEMIP setup (regrid v11.5.2 REMIP restarts,
    # create CAP.rc/cap_restart under $EXPDIR/restarts/$RSTDATE).
    gcmemip = run_config.get("GCMEMIP", False)
    rstdate = run_config.get("RSTDATE", "")

    if gcmemip:
        from gcmpy.run_tools.gcmemip_setup import stage_gcmemip_restarts

        stage_gcmemip_restarts(
            exp_dir,
            rstdate,
            resources["AGCM_IM"],
            resources["AGCM_LM"],
            geosbin,
            run_config["land"]["bcs"],
            run_config["ocean"]["out"],
            run_config["bc_base"],
        )

    # gcm_run.j:96-107, 399-416 -- experiment base + HISTORY collection dirs.
    from gcmpy.run_tools.history_dirs import ensure_exp_base_dirs, create_history_collection_dirs

    ensure_exp_base_dirs(exp_dir)
    collections = create_history_collection_dirs(exp_dir, exp_dir / "HISTORY.rc")

    # gcm_run.j:113-169 -- scratch directory creation (TSE_TMPDIR handling).
    from gcmpy.run_tools.scratchdir_ops import create_scratchdir

    scratch_dir_name = f"scratch.{rstdate}" if gcmemip else "scratch"
    scratch_dir = Path(create_scratchdir(str(exp_dir), scratch_dir_name))

    # gcm_run.j:313-360 -- move to scratch + copy RC files from $HOMDIR.
    from gcmpy.run_tools.scratch_setup import populate_scratch_from_home
    from gcmpy.run_tools.agcm_file_ops import get_GIGATRAJ

    gigatraj_file = get_GIGATRAJ(agcm_rc_file) or ""
    mom6 = is_enabled(run_config["ocean"].get("MOM6", "#DELETE"))
    cice6 = is_enabled(run_config["ocean"].get("CICE6", "#DELETE"))
    dataatm = is_enabled(run_config["ocean"].get("DATAATM", "#DELETE"))

    segment_control = populate_scratch_from_home(
        exp_dir,
        scratch_dir,
        geosbin,
        gcmemip=gcmemip,
        rstdate=rstdate,
        gigatraj_file=gigatraj_file,
        mom6=mom6,
        cice6=cice6,
        dataatm=dataatm,
    )

    # gcm_run.j:432-436 -- GCMRUN_catchment: strip CNFROOTC history
    # fields when the CatchmentCN land model (LSM_CHOICE == 2) is
    # active. Flag gate lives here (Jinja line-prefix semantics), the
    # LSM_CHOICE check inside reads scratch AGCM.rc dynamically.
    if is_enabled(run_config.get("land", {}).get("GCMRUN_catchment", "#DELETE")):
        from gcmpy.run_tools.history_file_ops import strip_gcmrun_cnfrootc_lines

        strip_gcmrun_cnfrootc_lines(
            scratch_dir / "HISTORY.rc", scratch_dir / "AGCM.rc"
        )

    # gcm_run.j:436-540 -- select the GEOSgcm.x executable (gcmpy launches
    # $GEOSBIN/GEOSgcm.x directly; EXPDIR fallback for custom builds.
    # Singularity raises NotImplementedError).
    from gcmpy.run_tools.executable_setup import setup_executable

    geosexe, singularity_run = setup_executable(geosbin, exp_dir, run_config)

    # gcm_run.j:541-708 -- staging initial restarts.
    from gcmpy.run_tools.stage_restarts import fetch_restarts
    from gcmpy.run_tools.restart_metadata import determine_wavewatch

    expid = run_config["expConfig"]["experiment_id"]
    gcmver = (Path(run_config["etc_path"]) / ".AGCM_VERSION").read_text().strip()
    bcrslv = f"{run_config['atmos']['res']}_{run_config['ocean']['res']}"
    coupled = is_enabled(run_config["ocean"]["coupled"])

    if is_enabled(run_config["ocean"].get("data", "#DELETE")):
        bctag = Path(run_config["bcs_dir"]).name
    else:
        bctag = Path(
            f"{run_config['coupled_dir']}/{run_config['ocean']['model']}/"
            f"{resources['OGCM_IM']}x{resources['OGCM_JM']}"
        ).name

    wavewatch = determine_wavewatch(scratch_dir, scratch_dir / "AGCM.rc")

    rst_by_face = fetch_restarts(
        exp_dir,
        scratch_dir,
        scratch_dir / "AGCM.rc",
        scratch_dir / "HISTORY.rc",
        scratch_dir / "cap_restart",
        expid,
        gcmver,
        bctag,
        bcrslv,
        gcmemip=gcmemip,
        rstdate=rstdate,
        coupled=coupled,
        wavewatch=wavewatch,
    )

    return {
        "exp_dir": exp_dir,
        "geosbin": geosbin,
        "scratch_dir": scratch_dir,
        "geosexe": geosexe,
        "singularity_run": singularity_run,
        "resources": resources,
        "segment_control": segment_control,
        "expid": expid,
        "gcmver": gcmver,
        "bctag": bctag,
        "bcrslv": bcrslv,
        "coupled": coupled,
        "mom6": mom6,
        "cice6": cice6,
        "wavewatch": wavewatch,
        "collections": collections,
        "gcmemip": gcmemip,
        "rstdate": rstdate,
        "rst_by_face": rst_by_face,
    }


def run_one_segment(job_context: dict, run_config: dict, counter: int) -> None:
    """
    Run one segment iteration of the GCM job (one pass through gcm_run.j's
    `while ( $counter <= ${NUM_SGMT} )` loop body, lines 709-1349).

    Wires up every ported gcmpy/run_tools module in the original's order:
    ExtData assembly, linkbcs, pre-run checks, MPI environment,
    bundleParser, REPLAY linking, OpenMP setup, the GEOSgcm.x launch,
    checkpoint archiving, HISTORY/holding moves, and post-processing.
    Launch failures raise GEOSRunError/EgressMissingError, matching
    gcm_run.j's hard `exit $run_status` on a failed run.

    Returns None (the original communicates failure only by exiting).
    """
    exp_dir = job_context["exp_dir"]
    scratch_dir = job_context["scratch_dir"]
    gcmemip = job_context["gcmemip"]
    rstdate = job_context["rstdate"]

    # gcm_run.j:709-718 -- copy CAP.rc from $HOMDIR (or GCMEMIP restarts
    # dir) and normalize whitespace, ready for this segment's run.
    import shutil

        # gcm_run.j:711 -- drop any EGRESS left by a previous segment so a
    # failed launch can't be masked by the stale file.
    (scratch_dir / "EGRESS").unlink(missing_ok=True)

    cap_rc_dest = scratch_dir / "CAP.rc"
    if gcmemip:
        cap_rc_src = exp_dir / "restarts" / rstdate / "CAP.rc"
    else:
        cap_rc_src = exp_dir / "CAP.rc"
    shutil.copy2(cap_rc_src, cap_rc_dest)

    from gcmpy.utils.regex_ops import normalize_whitespace

    cap_rc_dest.write_text(normalize_whitespace(cap_rc_dest.read_text()))

    # gcm_run.j:718-786 -- Current/Ending/Segment-Finish date-time
    # variables, END_DATE clamp, non-Reynolds-SST year-boundary clamp.
    from gcmpy.run_tools.segment_time import compute_segment_finish_time

    time_vars = compute_segment_finish_time(
        scratch_dir / "cap_restart",
        cap_rc_dest,
        run_config["ocean"]["res"],
    )

    # gcm_run.j:781-790 -- stage AMIP emissions.
    from gcmpy.run_tools.emissions import stage_amip_emissions

    if run_config["gocart"]["emissions"] == "AMIP_EMISSIONS":
        stage_amip_emissions(
            exp_dir / "RC",
            scratch_dir,
            job_context["resources"]["AGCM_LM"],
        )

    # gcm_run.j:799-820 -- WW3 start/end date injection (no-op if
    # ww3_multi.nml isn't present -- see ww3_ops.py's module docstring).
    from gcmpy.run_tools.ww3_ops import set_ww3_dates

    if job_context["wavewatch"]:
        set_ww3_dates(
            scratch_dir,
            time_vars["current_date"],
            time_vars["current_time"],
            time_vars["experiment_end_date"],
            time_vars["experiment_end_time"],
        )

    # gcm_run.j:821-841 -- ocean heartbeat DT sync + final *.yaml L72 patch.
    from gcmpy.run_tools.heartbeat_sync import sync_heartbeat_dt, patch_yaml_levels

    sync_heartbeat_dt(
        scratch_dir,
        coupled=job_context["coupled"],
        mom6=job_context["mom6"],
        cice6=job_context["cice6"],
    )
    patch_yaml_levels(scratch_dir, job_context["resources"]["AGCM_LM"])

    # gcm_run.j:842-854 -- ExtData assembly: patch WSUB_CLIM for
    # microphysics that don't use it, and run
    # construct_extdata_yaml_list.py (ExtData2G; extdata1g support was
    # removed from gcmpy).
    from gcmpy.run_tools.extdata import stage_extdata

    stage_extdata(
        scratch_dir,
        job_context["geosbin"],
        # The setup-time Jinja prefix '' keeps the WSUB block active
        # (1MOM/GFDL); '#DELETE#' comments it out (MGB2_2M uses WSUB_CLIM).
        turn_off_wsub=(
            run_config.get("atmos", {}).get("mp_turn_off_wsub_extdata", "")
            == ""
        ),
    )

    # gcm_run.j:856-993 -- per-segment pre-launch steps (see
    # gcmpy/run_tools/pre_run.py for the individual gcm_run.j citations).
    from gcmpy.run_tools.pre_run import (
        check_merra2ox_dates,
        configure_cice6_pond_restart,
        configure_gwd_restarts,
        configure_replay,
        ensure_tile_bin,
        rename_rrtmgp_instance_files,
        validate_openwater_tiles,
    )
    from gcmpy.utils.flag_ops import is_enabled

    # gcm_run.j:856-863 -- move GOCART over to RRTMGP bands.
    if is_enabled(run_config["atmos"].get("RRTMGP_RADIATION", "")):
        rename_rrtmgp_instance_files(scratch_dir)

    # gcm_run.j:350, 430, 868 -- boundary dataset linking (linkbcs).
    # The manifest (linkbcs.yaml) itself is generated once at experiment-
    # setup time by SetupGCM.stage_bcs(); this re-applies it every segment.
    from gcmpy.run_tools.linkbcs_ops import apply_linkbcs

    apply_linkbcs(scratch_dir, job_context["geosbin"], scratch_dir / "cap_restart")

    # gcm_run.j:870-950 -- GWD ridge restart, tile.bin, CICE6 pond state,
    # openwater tile-count validation, MERRA2OX date guard.
    configure_gwd_restarts(scratch_dir, job_context["rst_by_face"])
    ensure_tile_bin(scratch_dir, job_context["geosbin"])
    if job_context["cice6"]:
        configure_cice6_pond_restart(scratch_dir)
    validate_openwater_tiles(
        scratch_dir, job_context["geosbin"], run_config["run_command"]
    )
    check_merra2ox_dates(scratch_dir)

    # gcm_run.j:955-963 -- MPI environment variables.
    from gcmpy.run_tools.mpi_env import apply_setenv_block

    apply_setenv_block(run_config.get("mpi_config"))
    apply_setenv_block(run_config["ocean"].get("mpt_shepherd"))

    # gcm_run.j:961-963 -- run bundleParser.py.
    subprocess.run(["python3", "bundleParser.py"], cwd=scratch_dir, check=True)

    # gcm_run.j:967-993 -- REPLAY-mode GAAS/forcing-file linking.
    configure_replay(scratch_dir)

    # gcm_run.j:1061-1072 -- OpenMP environment setup.
    from gcmpy.run_tools.env_builder import setup_omp_env, teardown_omp_env

    setup_omp_env(scratch_dir)

    # gcm_run.j:1077-1109 -- launch GEOSgcm.x.
    from gcmpy.run_tools.mpi_runner import run_gcm

    resources = job_context["resources"]
    run_gcm(
        run_cmd=run_config["run_command"],
        total_pes=resources["TOTAL_PES"],
        geosexe=job_context["geosexe"],
        geosbin=job_context["geosbin"],
        scr_dir=scratch_dir,
        use_shmem=(job_context["segment_control"]["USE_SHMEM"] == "1"),
        use_ioserver=(resources["USE_IOSERVER"] == 1),
        model_npes=resources["NX"] * resources["NY"],
        num_oserver_nodes=resources["NUM_OSERVER_NODES"],
        num_backend_pes=resources["NUM_BACKEND_PES"],
        singularity_run=job_context["singularity_run"],
        # gcm_run.j:1086-1087 {{ ocean.preload }} -- raw catalog prefix
        # (empty for uncoupled runs); run_gcm() resolves $GEOSDIR etc.
        ocean_preload=run_config.get("ocean", {}).get("preload") or None,
        max_attempts=3 if run_config.get("several_tries") else 1,
    )

    # gcm_run.j:1107-1109 -- reset OpenMP threads.
    teardown_omp_env(scratch_dir)

    if run_config.get("stop_after_run", False):
        print(f"stop_after_run enabled: leaving model outputs in {scratch_dir}")
        sys.exit(0)

    # gcm_run.j:1196-1290 -- rename final checkpoints -> restarts, move
    # intermediate checkpoints, build the restart tarball (archiver.py).
    from gcmpy.run_tools.archiver import archive_checkpoints
    from gcmpy.run_tools.stage_restarts import read_edate

    # edate reflects the date GEOSgcm.x advanced cap_restart to.
    edate = read_edate(scratch_dir / "cap_restart")
    archive_checkpoints(
        scratch_dir,
        exp_dir,
        scratch_dir / "cap_restart",
        job_context["expid"],
        job_context["gcmver"],
        job_context["bctag"],
        job_context["bcrslv"],
        fsegment=job_context["segment_control"]["FCST_SEGMENT"],
        coupled=job_context["coupled"],
        wavewatch=job_context["wavewatch"],
    )

    # gcm_run.j:1283-1320 -- HISTORY files to holding, MOM6/CICE6 output.
    from gcmpy.run_tools.post_run import (
        move_history_to_holding,
        move_ocean_outputs,
        run_forecast_setup,
        run_post_processing,
    )

    move_history_to_holding(scratch_dir, exp_dir, job_context["collections"])
    move_ocean_outputs(
        scratch_dir,
        exp_dir,
        edate,
        mom6=job_context["mom6"],
        cice6=job_context["cice6"],
    )

    # gcm_run.j:1321-1345 -- post-processing and cycled forecasts. The
    # original wraps these in `if ($rc == 0)`, but rc is only ever set to
    # 0 -- GEOSgcm.x failure exits the script at gcm_run.j:1100-1107.
    run_post_processing(exp_dir, run_config.get("install_path", ""))
    run_forecast_setup(
        scratch_dir,
        exp_dir,
        job_context["segment_control"]["FCST_SEGMENT"],
        nymdc=time_vars["current_date"],
        nhmsc=time_vars["current_time"],
    )


def run_segments(config_path: Path, run_config: dict) -> bool:
    """
    Run up to NUM_SGMT segments within this job (gcm_run.j:709-1349's
    `while ( $counter <= ${NUM_SGMT} )` loop), then copy the finished
    restarts back to the experiment directory (gcm_run.j:1365-1405).

    A segment loop iteration ends early once cap_restart has reached the
    experiment's END_DATE (gcm_run.j:1347-1356). The original's
    `if ($rc != 0)` escape at lines 1357-1359 is dead code -- rc is only
    ever set to 0, since GEOSgcm.x failure exits the script earlier
    (1100-1107) -- so failures surface here as raised exceptions from
    run_one_segment() instead.

    Returns:
        True if more segments remain (cap_restart is still before
        END_DATE), i.e. execute() should resubmit the job;
        False when the experiment is finished.
    """
    job_context = run_job_setup(config_path, run_config)
    num_sgmt = int(job_context["segment_control"]["NUM_SGMT"])
    enddate = job_context["segment_control"]["END_DATE"][:8]

    counter = 1
    while counter <= num_sgmt:
        run_one_segment(job_context, run_config, counter)
        counter += 1

        # gcm_run.j:1347-1356 -- stop early once cap_restart has reached
        # the experiment's END_DATE.
        capdate = (job_context["scratch_dir"] / "cap_restart").read_text()[:8]
        if capdate >= enddate:
            break

    # gcm_run.j:1347-1348 -- re-read for the resubmit decision below
    # (also covers NUM_SGMT == 0, where the loop body never ran).
    capdate = (job_context["scratch_dir"] / "cap_restart").read_text()[:8]

    # gcm_run.j:1365-1405 -- copy restarts (+ cap_restart, ww3, RESTART/)
    # back to the experiment directory (or restarts/$RSTDATE for GCMEMIP).
    from gcmpy.run_tools.post_run import copy_restarts_to_exp_dir

    copy_restarts_to_exp_dir(
        job_context["scratch_dir"],
        job_context["exp_dir"],
        gcmemip=job_context["gcmemip"],
        rstdate=job_context["rstdate"],
        wavewatch=job_context["wavewatch"],
        coupled=job_context["coupled"],
    )

    # gcm_run.j:1395-1405 -- resubmit only while the end date is ahead.
    return capdate < enddate


def execute(config_path: Path, run_config: dict) -> None:
    """
    In-job entrypoint (invoked by gcm_run.sh, detected via the scheduler job ID):
    run up to NUM_SGMT segments, then resubmit for the next job's worth of
    segments (if any) using the same sbatch-flag logic as the login-node
    submit path -- no shell-side resubmission loop required.
    """
    if run_segments(config_path, run_config):
        submit_job(config_path, run_config)


# ---------------------------------------------------------------------------
# Entrypoint
# ---------------------------------------------------------------------------

def main() -> None:
    args = capture_arguments()
    config_path = Path(args.config)
    run_config = import_yaml(config_path)

    if "SLURM_JOB_ID" in os.environ or "PBS_JOBID" in os.environ:
        # We're running inside an allocated SLURM job (invoked by gcm_run.sh).
        # Batch resources (NCPUS, NUM_MODEL_NODES, ...) are only needed once
        # we're actually inside the running job, launching GEOSgcm.x.
        nx = run_config.get("atmos", {}).get("nx")
        ny = run_config.get("atmos", {}).get("ny")
        env_dict = load_env_vars()
        if env_dict and nx and ny:
            run_config.update(get_batch_resources(nx, ny, env_dict))

        os.environ["GEOSBIN"] = str(run_config.get("bin_path", ""))
        execute(config_path, run_config)
    else:
        # We're on a login node (invoked by a human or gcm_setup.py) --
        # just kick off the first job.
        submit_job(config_path, run_config)


if __name__ == "__main__":
    main()

"""
execution/mpi_runner.py

Builds and executes the srun / mpiexec command to run GEOSgcm.x.

Replaces lines 1162-1194 of gcm_run.j.
"""

import os
import subprocess
from pathlib import Path


class GEOSRunError(RuntimeError):
    """Raised when GEOSgcm.x exits with a non-zero return code."""


class EgressMissingError(RuntimeError):
    """Raised when GEOSgcm.x exits cleanly but EGRESS file is absent."""


def _build_ioserver_args(
    model_npes: int,
    num_oserver_nodes: int,
    num_backend_pes: int,
) -> list[str]:
    """
    Build the IO server argument list for GEOSgcm.x.

    Replaces:
        set IOSERVER_OPTIONS = "--npes_model $MODEL_NPES --nodes_output_server $NUM_OSERVER_NODES"
        set IOSERVER_EXTRA   = "--oserver_type multigroup --npes_backend_pernode $NUM_BACKEND_PES"

    Args:
        model_npes:        Number of model PEs.
        num_oserver_nodes: Number of output server nodes.
        num_backend_pes:   Number of backend PEs per node.

    Returns:
        List of argument strings to append to the GEOSgcm.x command.
    """
    return [
        "--npes_model", str(model_npes),
        "--nodes_output_server", str(num_oserver_nodes),
        "--oserver_type", "multigroup",
        "--npes_backend_pernode", str(num_backend_pes),
    ]


def _build_command(
    run_cmd: str,
    total_pes: int,
    geosexe: Path,
    ioserver_args: list[str],
    singularity_run: str | None,
    ocean_preload: str | None,
) -> tuple[list[str], dict[str, str]]:
    """
    Assemble the full subprocess command and environment for a single launch attempt.

    Args:
        run_cmd:         MPI launcher and its base flags (e.g. 'srun' or 'mpiexec').
        total_pes:       Total number of PEs to pass to the launcher.
        geosexe:         Path to the GEOSgcm.x executable.
        ioserver_args:   Pre-built IO server argument list (empty list if not used).
        singularity_run: Singularity invocation prefix, or None for a native build.
        ocean_preload:   Value for LD_PRELOAD (coupled ocean runs), or None.

    Returns:
        A (cmd, env) tuple ready for subprocess.run.
    """
    if singularity_run is not None:
        exe_args = [singularity_run, str(geosexe)]
    else:
        exe_args = [str(geosexe)]

    cmd = [run_cmd, str(total_pes)] + exe_args + ioserver_args + [
        "--logging_config", "logging.yaml"
    ]

    env = os.environ.copy()
    if ocean_preload:
        env["LD_PRELOAD"] = ocean_preload

    return cmd, env


def _remove_shm_keys(geosbin: Path) -> None:
    """
    Run RmShmKeys_sshmpi.csh to clean up shared memory keys, suppressing output.

    Replaces:
        if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

    Args:
        geosbin: Path to the GEOS bin directory containing RmShmKeys_sshmpi.csh.
    """
    subprocess.run(
        [str(geosbin / "RmShmKeys_sshmpi.csh")],
        capture_output=True,
    )


def run_gcm(
    run_cmd: str,
    total_pes: int,
    geosexe: Path,
    geosbin: Path,
    scr_dir: Path,
    use_shmem: bool = False,
    use_ioserver: bool = False,
    model_npes: int = 0,
    num_oserver_nodes: int = 0,
    num_backend_pes: int = 0,
    singularity_run: str | None = None,
    ocean_preload: str | None = None,
    max_attempts: int = 1,
) -> None:
    """
    Build and execute the MPI command to run GEOSgcm.x, then verify success.

    Handles shared memory cleanup, IO server configuration, singularity vs.
    native builds, optional LD_PRELOAD for ocean coupling, and retries.

    Replaces:
        @OCEAN_PRELOAD @SEVERAL_TRIES $RUN_CMD $TOTAL_PES $GEOSEXE $IOSERVER_OPTIONS ...

    Args:
        run_cmd:           MPI launcher string (e.g. 'srun' or 'mpiexec -n').
        total_pes:         Total number of PEs to launch.
        geosexe:           Path to the GEOSgcm.x executable.
        geosbin:           Path to the GEOS bin directory.
        scr_dir:           Scratch/run directory (checked for EGRESS after run).
        use_shmem:         If True, run RmShmKeys before and after execution.
        use_ioserver:      If True, append IO server flags to the command.
        model_npes:        Number of model PEs (used when use_ioserver=True).
        num_oserver_nodes: Number of output server nodes (used when use_ioserver=True).
        num_backend_pes:   Number of backend PEs per node (used when use_ioserver=True).
        singularity_run:   Singularity invocation prefix, or None for a native build.
        ocean_preload:     Value for LD_PRELOAD for coupled ocean runs, or None.
        max_attempts:      Number of launch attempts before giving up (native builds only).

    Raises:
        GEOSRunError:      If GEOSgcm.x exits with a non-zero return code on all attempts.
        EgressMissingError: If GEOSgcm.x exits cleanly but EGRESS is not present.
    """
    ioserver_args = (
        _build_ioserver_args(model_npes, num_oserver_nodes, num_backend_pes)
        if use_ioserver
        else []
    )

    cmd, env = _build_command(
        run_cmd, total_pes, geosexe, ioserver_args,
        singularity_run, ocean_preload,
    )

    if use_shmem:
        _remove_shm_keys(geosbin)

    # Singularity builds do not retry
    attempts = 1 if singularity_run is not None else max_attempts

    run_status = None
    for attempt in range(1, attempts + 1):
        result = subprocess.run(cmd, env=env, cwd=scr_dir)
        run_status = result.returncode
        if run_status == 0:
            break

    if run_status != 0:
        raise GEOSRunError(f"GEOSgcm.x failed with return code {run_status}")

    if use_shmem:
        _remove_shm_keys(geosbin)

    if not (scr_dir / "EGRESS").exists():
        raise EgressMissingError("EGRESS file not found, GEOSgcm.x likely failed")

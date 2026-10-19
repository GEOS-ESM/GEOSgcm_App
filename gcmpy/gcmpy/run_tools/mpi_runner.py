"""
execution/mpi_runner.py

Builds and executes the srun / mpiexec command to run GEOSgcm.x.

Replaces lines 1077-1109 of gcm_run.j.
"""

import os
import shlex
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


def _parse_ocean_preload(raw: str | None, geosdir: Path) -> dict[str, str] | None:
    """
    Turn the ocean catalog's preload command prefix into an environment
    mapping for the launch.

    The catalog stores a csh command prefix such as
    `env LD_PRELOAD=$GEOSDIR/lib/libmom6.so:$GEOSDIR/lib/libcice6.so`
    (or `env DYLD_INSERT_LIBRARIES=...` on macOS), which gcm_run.j embeds
    verbatim in the launch line (gcm_run.j:1086-1087) for the shell to
    expand. Here we strip the `env` wrapper and expand `$GEOSDIR` against
    the experiment's GEOS directory (GEOSBIN lives at $GEOSDIR/bin).

    Args:
        raw:     Raw `ocean.preload` string from run_<expid>.yaml, or None.
        geosdir: GEOS installation directory ($GEOSDIR).

    Returns:
        Mapping of environment variable name to expanded value, or None
        when there is nothing to preload (empty string) or the value is
        unusable (unresolved `@...@` CMake tokens, malformed input) --
        never raises, so a broken catalog entry cannot abort the run.
    """
    if not raw:
        return None

    text = str(raw).strip()
    if text.startswith("env "):
        text = text[4:].strip()

    # An unresolved CMake template token (e.g. @PRELOAD_COMMAND@) means
    # the install was configured without it -- skip rather than export
    # garbage.
    if "@" in text:
        print(f"WARNING: unresolved ocean preload {raw!r}; ignoring")
        return None

    name, sep, value = text.partition("=")
    name = name.strip()
    if not sep or not name or not value:
        print(f"WARNING: cannot parse ocean preload {raw!r}; ignoring")
        return None

    value = value.replace("${GEOSDIR}", str(geosdir))
    value = value.replace("$GEOSDIR", str(geosdir))
    return {name: value}


def _tokenize_command(cmd_string: str, what: str) -> list[str]:
    """
    Split a csh-style command string into argv tokens with variable
    expansion.

    gcm_run.j relies on the shell to expand and word-split `$RUN_CMD`
    (and `$SINGULARITY_RUN`) before launching GEOSgcm.x, and setup
    writes multi-word values such as '$GEOSBIN/esma_mpirun -np '
    (setup_envs.py:34). subprocess requires the tokens pre-split and
    expanded.

    Args:
        cmd_string: Command string, possibly multi-word with $VAR refs.
        what:       Label used in error messages (e.g. 'run_command').

    Returns:
        Expanded argv token list.

    Raises:
        ValueError: If a token still contains '$' after expansion
                    (the referenced variable is not in the environment).
    """
    tokens = [os.path.expandvars(token) for token in shlex.split(cmd_string)]
    unresolved = [token for token in tokens if "$" in token]
    if unresolved:
        raise ValueError(
            f"{what} contains unexpanded variable reference(s): {unresolved} "
            "-- the referenced variable (e.g. GEOSBIN, set by gcm_run.sh) "
            "must be exported in the environment"
        )
    return tokens


def _build_command(
    run_cmd: str,
    total_pes: int,
    geosexe: Path,
    ioserver_args: list[str],
    singularity_run: str | None,
    preload_env: dict[str, str] | None,
) -> tuple[list[str], dict[str, str]]:
    """
    Assemble the full subprocess command and environment for a single launch attempt.

    Args:
        run_cmd:      MPI launcher string; multi-word values like
                      '$GEOSBIN/esma_mpirun -np ' are tokenized and
                      expanded (replaces csh word-splitting of $RUN_CMD).
        total_pes:    Total number of PEs to pass to the launcher.
        geosexe:      Path to the GEOSgcm.x executable.
        ioserver_args: Pre-built IO server argument list (empty list if not used).
        singularity_run: Singularity invocation prefix (tokenized like
                      run_cmd), or None for a native build.
        preload_env:  Extra environment (coupled-ocean LD_PRELOAD /
                      DYLD_INSERT_LIBRARIES) from _parse_ocean_preload(), or None.

    Returns:
        A (cmd, env) tuple ready for subprocess.run.
    """
    env = os.environ.copy()
    if preload_env:
        env.update(preload_env)

    run_tokens = _tokenize_command(run_cmd, "run_command")

    if singularity_run is not None:
        exe_args = _tokenize_command(singularity_run, "singularity_run") + [
            str(geosexe)
        ]
    else:
        exe_args = [str(geosexe)]

    cmd = run_tokens + [str(total_pes)] + exe_args + ioserver_args + [
        "--logging_config", "logging.yaml"
    ]

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
        ocean_preload:     Raw `ocean.preload` catalog string (csh command
                           prefix such as `env LD_PRELOAD=$GEOSDIR/lib/
                           libmom6.so`), resolved against geosbin's parent
                           into launch-time environment variables; None or
                           '' skips preloading.
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

    # gcm_run.j:1086-1087 -- {{ ocean.preload }} sits in front of the
    # launch command; resolve it into launch-time environment variables.
    preload_env = _parse_ocean_preload(ocean_preload, Path(geosbin).parent)

    cmd, env = _build_command(
        run_cmd, total_pes, geosexe, ioserver_args,
        singularity_run, preload_env,
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

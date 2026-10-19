"""
executable_setup.py

Resolves the GEOSgcm.x executable to use for the run: copies it from the
experiment directory into scratch for native builds. Singularity builds
are not yet supported by gcmpy (singularity_build/singularity_sandbox are
currently unreachable in the setup_tools config model -- see
setup_main.py:59-61 -- so this is intentionally not implemented here).

Replaces lines 436-540 of gcm_run.j.
"""

import shutil
from pathlib import Path

from gcmpy.utils.flag_ops import is_enabled


class ExecutableNotFoundError(RuntimeError):
    """Raised when GEOSgcm.x is not found in the experiment directory."""


def setup_executable(
    exp_dir: Path | str,
    scratch_dir: Path | str,
    run_config: dict,
) -> tuple:
    """
    Top-level entry point: resolve the GEOSgcm.x executable to use for the
    run, returning values ready to pass directly into
    gcmpy.run_tools.mpi_runner.run_gcm(geosexe=..., singularity_run=...).

    Replaces:
        {{ native_build }} if (-e $EXPDIR/GEOSgcm.x) then
        {{ native_build }}    /bin/cp $EXPDIR/GEOSgcm.x $SCRDIR/GEOSgcm.x
        {{ native_build }} else
        {{ native_build }}    exit 4
        {{ native_build }} endif
        {{ native_build }} setenv GEOSEXE $SCRDIR/GEOSgcm.x

    Args:
        exp_dir:     Experiment directory (EXPDIR/HOMDIR).
        scratch_dir: Scratch/run directory.
        run_config:  The experiment's run_<expid>.yaml dictionary.

    Returns:
        (geosexe, singularity_run) tuple:
            geosexe:         Path to the GEOSgcm.x executable to run.
            singularity_run: Always None (native-only path).

    Raises:
        NotImplementedError:    If singularity_build is enabled (not yet
                                 supported -- see module docstring).
        ExecutableNotFoundError: If GEOSgcm.x is not found in exp_dir
                                 (mirrors `exit 4`).
    """
    if is_enabled(run_config.get("singularity_build", "#DELETE")):
        raise NotImplementedError(
            "Singularity execution is not supported by gcmpy yet -- "
            "singularity_build/singularity_sandbox are unreachable in the "
            "current setup_tools config model (always defaulted to "
            "disabled in setup_main.py)."
        )

    src = Path(exp_dir) / "GEOSgcm.x"
    if not src.exists():
        raise ExecutableNotFoundError(
            f"{src} not found. Please link or copy the executable to the "
            "experiment directory."
        )

    dest = Path(scratch_dir) / "GEOSgcm.x"
    shutil.copy2(src, dest)

    return dest, None

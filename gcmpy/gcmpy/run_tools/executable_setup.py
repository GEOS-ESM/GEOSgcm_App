"""
executable_setup.py

Resolves the GEOSgcm.x executable to use for the run: launches the
install/bin copy directly ($GEOSBIN/GEOSgcm.x) instead of copying it
into scratch (gcmpy divergence from gcm_run.j -- the scratch copy was
a redundant 149 MB per-segment copy; the model reads its input files
(CAP.rc, input.nml, RESTART/, ...) from the working directory, which
stays the scratch directory). A fallback to $EXPDIR/GEOSgcm.x is kept
for native custom-build workflows that place a build in the experiment
directory. Singularity builds are not yet supported by gcmpy
(singularity_build/singularity_sandbox are currently unreachable in the
setup_tools config model -- see setup_main.py:59-61 -- so this is
intentionally not implemented here).

Replaces lines 436-540 of gcm_run.j (with the noted divergence).
"""

from pathlib import Path

from gcmpy.utils.flag_ops import is_enabled


class ExecutableNotFoundError(RuntimeError):
    """Raised when GEOSgcm.x is not found in install/bin or exp_dir."""


def setup_executable(
    geosbin: Path | str,
    exp_dir: Path | str,
    run_config: dict,
) -> tuple:
    """
    Top-level entry point: resolve the GEOSgcm.x executable to use for the
    run, returning values ready to pass directly into
    gcmpy.run_tools.mpi_runner.run_gcm(geosexe=..., singularity_run=...).

    Diverges from the original:
        {{ native_build }} if (-e $EXPDIR/GEOSgcm.x) then
        {{ native_build }}    /bin/cp $EXPDIR/GEOSgcm.x $SCRDIR/GEOSgcm.x
        {{ native_build }} else
        {{ native_build }}    exit 4
        {{ native_build }} endif
        {{ native_build }} setenv GEOSEXE $SCRDIR/GEOSgcm.x

    gcmpy resolves $GEOSBIN/GEOSgcm.x (install/bin) directly -- no copy
    into scratch. $EXPDIR/GEOSgcm.x is kept only as a fallback so native
    custom builds placed in the experiment directory still work.

    Args:
        geosbin:    GEOS bin directory (install/bin, i.e. $GEOSBIN).
        exp_dir:    Experiment directory (EXPDIR/HOMDIR); fallback location.
        run_config: The experiment's run_<expid>.yaml dictionary.

    Returns:
        (geosexe, singularity_run) tuple:
            geosexe:         Path to the GEOSgcm.x executable to run.
            singularity_run: Always None (native-only path).

    Raises:
        NotImplementedError:    If singularity_build is enabled (not yet
                                 supported -- see module docstring).
        ExecutableNotFoundError: If GEOSgcm.x is not found in either
                                 install/bin or exp_dir (mirrors `exit 4`).
    """
    if is_enabled(run_config.get("singularity_build", "#DELETE")):
        raise NotImplementedError(
            "Singularity execution is not supported by gcmpy yet -- "
            "singularity_build/singularity_sandbox are unreachable in the "
            "current setup_tools config model (always defaulted to "
            "disabled in setup_main.py)."
        )

    geosexe = Path(geosbin) / "GEOSgcm.x"
    if geosexe.exists():
        return geosexe, None

    fallback = Path(exp_dir) / "GEOSgcm.x"
    if fallback.exists():
        return fallback, None

    raise ExecutableNotFoundError(
        f"GEOSgcm.x not found in {geosexe} or {fallback}."
    )

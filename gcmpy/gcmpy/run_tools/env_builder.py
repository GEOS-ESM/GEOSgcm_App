"""
execution/env_builder.py

Sets up OMP_NUM_THREADS, KMP_AFFINITY, stack sizes, and patches
GWD_GridComp.rc for threaded runs. Provides a matching teardown to
restore the environment after GEOSgcm.x completes.

Replaces lines 1061-1072 and 1107-1109 of gcm_run.j.
"""

import os
from pathlib import Path


def setup_omp_env(scr_dir: Path, omp_num_threads: int = 1) -> None:
    """
    Configure OpenMP environment variables and patch GWD_GridComp.rc for the run.

    When omp_num_threads > 1, also sets OMP_STACKSIZE and KMP_AFFINITY, and
    flips the GWD_GridComp.rc threading flag from FALSE to TRUE.

    Replaces:
        setenv OMP_NUM_THREADS 1
        if ($OMP_NUM_THREADS > 1) then
          setenv OMP_STACKSIZE 16M
          setenv KMP_AFFINITY compact
          ./strip GWD_GridComp.rc
          sed -i -e "s|FALSE|TRUE|g" GWD_GridComp.rc
        endif

    Args:
        scr_dir:         Scratch/run directory containing GWD_GridComp.rc.
        omp_num_threads: Number of OpenMP threads. Values > 1 enable threaded
                         stack/affinity settings and patch GWD_GridComp.rc.
    """
    os.environ["OMP_NUM_THREADS"] = str(omp_num_threads)

    if omp_num_threads > 1:
        os.environ["OMP_STACKSIZE"] = "16M"
        os.environ["KMP_AFFINITY"] = "compact"
        _patch_gwd_rc(scr_dir, old="FALSE", new="TRUE")


def teardown_omp_env(scr_dir: Path) -> None:
    """
    Reset OpenMP environment and restore GWD_GridComp.rc after the run.

    Always resets OMP_NUM_THREADS to 1 and flips the GWD_GridComp.rc
    threading flag back from TRUE to FALSE, regardless of what it was
    set to during setup.

    Replaces:
        setenv OMP_NUM_THREADS 1
        ./strip GWD_GridComp.rc
        sed -i -e "s|TRUE|FALSE|g" GWD_GridComp.rc

    Args:
        scr_dir: Scratch/run directory containing GWD_GridComp.rc.
    """
    os.environ["OMP_NUM_THREADS"] = "1"
    _patch_gwd_rc(scr_dir, old="TRUE", new="FALSE")


def _patch_gwd_rc(scr_dir: Path, old: str, new: str) -> None:
    """
    Replace all occurrences of old with new in GWD_GridComp.rc in-place.

    Replaces the combination of ./strip + sed -i in the original, which
    first stripped comments then performed the substitution. Here we do the
    substitution directly on the existing file content.

    Args:
        scr_dir: Scratch/run directory containing GWD_GridComp.rc.
        old:     String to replace (e.g. 'FALSE').
        new:     Replacement string (e.g. 'TRUE').
    """
    rc_path = scr_dir / "GWD_GridComp.rc"
    text = rc_path.read_text()
    rc_path.write_text(text.replace(old, new))

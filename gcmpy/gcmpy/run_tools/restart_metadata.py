"""
restart_metadata.py

Resolves small bits of run metadata needed to call
gcmpy.run_tools.stage_restarts.fetch_restarts() that aren't simple
dict lookups -- currently just WaveWatch3 detection.

Replaces lines 653-656 of gcm_run.j.
"""

from pathlib import Path

from gcmpy.run_tools.agcm_file_ops import get_use_waves
from gcmpy.utils.file_ops import get_val_from_rc_file


def determine_wavewatch(scratch_dir: Path | str, agcm_rc_path: Path | str) -> bool:
    """
    Determine whether WaveWatch3 is enabled for this run.

    Replaces:
        set USE_WAVES = `grep '^\\s*USE_WAVES:' AGCM.rc| cut -d: -f2`
        set wavemodel = `cat WGCM.rc | grep "wave_model:" | cut -d "#" -f1 | cut -d ":" -f 2 | sed 's/\\s//g'`
        set wavewatch = 0
        if (($USE_WAVES != 0) && ($wavemodel == "WW3") ) set wavewatch = 1

    WGCM.rc is not part of gcmpy's config model (no catalog/question
    represents it) -- this simply reads whatever WGCM.rc happens to have
    been copied into scratch_dir (via populate_scratch_from_home()'s
    generic `*.rc` copy), and returns False gracefully if it isn't present
    (matching the rarity of wave-model-enabled experiments).

    Args:
        scratch_dir:  Scratch/run directory (may contain WGCM.rc).
        agcm_rc_path: Path to the (scratch-copy) AGCM.rc.

    Returns:
        True if USE_WAVES is non-zero and WGCM.rc's wave_model is 'WW3'.
    """
    use_waves = get_use_waves(agcm_rc_path)
    if use_waves == 0:
        return False

    wgcm_rc_path = Path(scratch_dir) / "WGCM.rc"
    if not wgcm_rc_path.exists():
        return False

    wave_model = get_val_from_rc_file(wgcm_rc_path, "wave_model:")
    if wave_model is None:
        return False

    wave_model = wave_model.replace(" ", "")

    return wave_model == "WW3"

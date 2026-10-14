"""
heartbeat_sync.py

Syncs the ocean-model DT (timestep) in AGCM.rc/MOM_override/ice_in to
CAP.rc's HEARTBEAT_DT (since a user might change HEARTBEAT_DT in CAP.rc
at run time), and patches the vertical-level tag ('L72'/'z72') in all
remaining *.yaml files in scratch to match AGCM_LM.

MOM5 support has been removed from gcmpy entirely (OCEAN_type only offers
'MOM6'/'MIT'), so the MOM5 branch from gcm_run.j is intentionally omitted
here.

Replaces lines 792 (comment)-839 of gcm_run.j (the ocean.coupled/MOM6/
CICE6 HEARTBEAT_DT sync block and the final *.yaml L72 patch).
"""

import re
from pathlib import Path

from gcmpy.utils.file_ops import get_val_from_rc_file


def _replace_numeric_assignment(text: str, pattern: str, new_value: str) -> str:
    """
    Replace a `<pattern><number>(.<number>)?` occurrence with
    `<pattern><new_value>`, preserving the matched prefix text exactly
    (so this works regardless of the surrounding whitespace/punctuation
    conventions of each target file).
    """
    return re.sub(
        rf"({pattern})[0-9]+(\.[0-9]+)?",
        rf"\g<1>{new_value}",
        text,
    )


def sync_heartbeat_dt(
    scratch_dir: Path | str,
    coupled: bool = False,
    mom6: bool = False,
    cice6: bool = False,
) -> None:
    """
    Top-level entry point: read HEARTBEAT_DT from the segment's CAP.rc and
    sync it into AGCM.rc (coupled), MOM_override (MOM6), and ice_in (CICE6).

    Replaces:
        {{ ocean.coupled }}set HEARTBEAT_DT = `grep '^\\s*HEARTBEAT_DT:' CAP.rc | cut -d: -f2 | awk '{print $1}'`
        {{ ocean.coupled }} sed -i -e "s/OGCM_RUN_DT: [0-9]\\+\\(\\.[0-9]\\+\\)\\?/OGCM_RUN_DT: $HEARTBEAT_DT/g" AGCM.rc
        {{ ocean.MOM6 }} sed -i -e "s/DT = [0-9]\\+\\(\\.[0-9]\\+\\)\\?/DT = $HEARTBEAT_DT/g" \\
        {{ ocean.MOM6 }}        -e "s/DT_THERM = [0-9]\\+\\(\\.[0-9]\\+\\)\\?/DT_THERM = $HEARTBEAT_DT/g" MOM_override
        {{ ocean.CICE6 }} sed -i -E "s/^([[:space:]]*dt[[:space:]]*=[[:space:]]*)[0-9]+(\\.[0-9]+)?/\\1${HEARTBEAT_DT}/" ice_in

    Note: MOM5 support has been removed from gcmpy (see module docstring),
    so that branch is omitted here.

    Args:
        scratch_dir: Scratch/run directory.
        coupled:     True for coupled-ocean experiments (patches AGCM.rc).
        mom6:        True if running the MOM6 ocean model (patches MOM_override).
        cice6:       True if running the CICE6 seaice model (patches ice_in).
    """
    if not (coupled or mom6 or cice6):
        return

    scratch_dir = Path(scratch_dir)
    cap_rc_path = scratch_dir / "CAP.rc"
    heartbeat_dt = get_val_from_rc_file(cap_rc_path, "HEARTBEAT_DT:")
    if heartbeat_dt is None:
        return
    heartbeat_dt = heartbeat_dt.split()[0]

    if coupled:
        agcm_rc_path = scratch_dir / "AGCM.rc"
        if agcm_rc_path.exists():
            text = agcm_rc_path.read_text()
            text = _replace_numeric_assignment(text, r"OGCM_RUN_DT:\s*", heartbeat_dt)
            agcm_rc_path.write_text(text)

    if mom6:
        mom_override_path = scratch_dir / "MOM_override"
        if mom_override_path.exists():
            text = mom_override_path.read_text()
            text = _replace_numeric_assignment(text, r"DT\s*=\s*", heartbeat_dt)
            text = _replace_numeric_assignment(text, r"DT_THERM\s*=\s*", heartbeat_dt)
            mom_override_path.write_text(text)

    if cice6:
        ice_in_path = scratch_dir / "ice_in"
        if ice_in_path.exists():
            text = ice_in_path.read_text()
            text = re.sub(
                r"^([ \t]*dt[ \t]*=[ \t]*)[0-9]+(\.[0-9]+)?",
                rf"\g<1>{heartbeat_dt}",
                text,
                flags=re.MULTILINE,
            )
            ice_in_path.write_text(text)


def patch_yaml_levels(scratch_dir: Path | str, agcm_lm: int) -> None:
    """
    Top-level entry point: patch '/L72/'->'/L{agcm_lm}/' and 'z72'->'z{agcm_lm}'
    in every remaining *.yaml file in scratch_dir, if agcm_lm != 72.

    This is the final catch-all pass over scratch_dir's *.yaml files (e.g.
    ExtData yaml files copied in by populate_scratch_from_home()'s generic
    copy), distinct from emissions.py's L72 patch which only applies to
    the AMIP-emissions-directory-sourced files.

    Replaces:
        if( $AGCM_LM  != 72 ) then
            set files = `/bin/ls  *.yaml`
            foreach file ($files)
              cp $file dummy
              cat dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > $file
            end
        endif

    Args:
        scratch_dir: Scratch/run directory.
        agcm_lm:     Number of atmospheric model levels.
    """
    if agcm_lm == 72:
        return

    for yaml_path in Path(scratch_dir).glob("*.yaml"):
        text = yaml_path.read_text()
        patched = text.replace("/L72/", f"/L{agcm_lm}/").replace("z72", f"z{agcm_lm}")
        yaml_path.write_text(patched)

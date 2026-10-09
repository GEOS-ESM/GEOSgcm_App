"""
linkbcs_ops.py

Per-segment application of the boundary-dataset ("BCS") linking manifest.

The manifest itself (linkbcs.yaml) is generated once, at experiment-setup
time, by gcmpy.setup_tools.setup_main.SetupGCM.stage_bcs() (which invokes
stagebcs.py). Every segment, this module re-applies that manifest via
linkbcs.py, substituting the current segment's year (derived from
cap_restart via packtoiso.py) into any remaining '{{ year }}' placeholders
(e.g. SST/sea-ice data file paths).

Replaces lines 384-403 and 886-900 of gcm_run.j (the linkbcs.tmpl
invocation and the "Link Boundary Conditions for Appropriate Date" step).
"""

import subprocess
from pathlib import Path


class LinkBCSError(RuntimeError):
    """Raised when packtoiso.py or linkbcs.py exit with a non-zero return code."""


def get_link_timestamp(geosbin: Path, cap_restart_path: Path) -> str:
    """
    Convert cap_restart's "NYMD NHMS" into the ISO-8601 timestamp string
    linkbcs.py expects for its --timestamp argument, by invoking
    packtoiso.py exactly as gcm_run.j does.

    Args:
        geosbin:          Path to the GEOS bin directory containing packtoiso.py.
        cap_restart_path: Path to the segment's cap_restart file.

    Returns:
        ISO-8601 timestamp string (e.g. '2000-03-01T21:00:00').

    Raises:
        LinkBCSError: If packtoiso.py exits with a non-zero return code.
    """
    result = subprocess.run(
        [str(Path(geosbin) / "packtoiso.py"), str(cap_restart_path)],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise LinkBCSError(
            f"packtoiso.py failed with return code {result.returncode}: {result.stderr}"
        )

    return result.stdout.strip()


def apply_linkbcs(
    scratch_dir: Path,
    geosbin: Path,
    cap_restart_path: Path,
    manifest_name: str = "linkbcs.yaml",
) -> None:
    """
    Top-level entry point: apply the boundary-dataset linking manifest for
    the current segment.

    Replaces:
        set link_timestamp = `$GEOSBIN/packtoiso.py cap_restart`
        $GEOSBIN/linkbcs.py --manifest linkbcs.yaml --timestamp $link_timestamp
        set linkbcs_status = $status
        if ($linkbcs_status != 0) then
           echo "linkbcs.py failed with return code $linkbcs_status"
           exit $linkbcs_status
        endif

    Args:
        scratch_dir:      Scratch/run directory (contains cap_restart and
                           the linkbcs.yaml manifest copied in from $EXPDIR).
        geosbin:           Path to the GEOS bin directory containing
                           packtoiso.py and linkbcs.py.
        cap_restart_path: Path to the segment's cap_restart file.
        manifest_name:    Name of the manifest file within scratch_dir
                           (default: 'linkbcs.yaml').

    Raises:
        LinkBCSError: If linkbcs.py exits with a non-zero return code.
    """
    link_timestamp = get_link_timestamp(geosbin, cap_restart_path)

    manifest_path = (Path(scratch_dir) / manifest_name).resolve()

    result = subprocess.run(
        [
            str(Path(geosbin) / "linkbcs.py"),
            "--manifest", str(manifest_path),
            "--timestamp", link_timestamp,
        ],
        cwd=scratch_dir,
    )

    if result.returncode != 0:
        raise LinkBCSError(f"linkbcs.py failed with return code {result.returncode}")

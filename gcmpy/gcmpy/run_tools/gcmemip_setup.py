"""
gcmemip_setup.py

GCMEMIP restart staging: for a GCMEMIP ensemble member (identified by
RSTDATE), regrids the sdrabenh REMIP v11.5.2 restarts to the current
experiment's resolution/BCs and creates the initial CAP.rc/cap_restart
for that ensemble member, if not already done.

This is a verbatim port of a single, specific (not generalized) restart
source used historically for GCMEMIP initialization -- the hardcoded
source directory is parameterized (remip_source_dir) but defaults to the
exact path gcm_run.j used.

Replaces lines 241-311 of gcm_run.j.
"""

import re
import subprocess
import tarfile
from pathlib import Path

from gcmpy.utils.clock_ops import create_clock, advance_clock
from gcmpy.utils.regex_ops import search_replace_in_file, extract_line_starting_with, normalize_whitespace

num_seconds_per_day = 86400

_DEFAULT_REMIP_SOURCE_DIR = (
    "/discover/nobackup/projects/gmao/geos_itv/sdrabenh/REMIP_Experiments/"
    "v11.5.2_L072_C180_M2_REMIP/restarts"
)


def _parse_year_month(rstdate: str) -> tuple:
    """
    Parse year/month from RSTDATE.

    Replaces:
        set year  = `echo $RSTDATE | cut -d_ -f1 | cut -b1-4`
        set month = `echo $RSTDATE | cut -d_ -f1 | cut -b5-6`
    """
    date_part = rstdate.split("_")[0]
    return date_part[0:4], date_part[4:6]


def _zero_pad_im(agcm_im: int) -> str:
    """
    Replaces:
        set IMC = $AGCM_IM
        if(     $IMC < 10 ) then
             set IMC = 000$IMC
        else if($IMC < 100) then
             set IMC = 00$IMC
        else if($IMC < 1000) then
             set IMC = 0$IMC
        endif
    """
    if agcm_im < 10:
        return f"000{agcm_im}"
    if agcm_im < 100:
        return f"00{agcm_im}"
    if agcm_im < 1000:
        return f"0{agcm_im}"
    return str(agcm_im)


def _bulk_stripname(directory: Path, old: str, new: str = "") -> None:
    """
    Python equivalent of the $GEOSBIN/stripname utility (bulk form, no
    explicit file argument): renames every file in `directory` whose name
    contains `old`, replacing the first occurrence of `old` with `new`.

    Replaces:
        $GEOSBIN/stripname <old> [<new>]
    """
    for path in sorted(directory.iterdir()):
        if old in path.name:
            new_name = path.name.replace(old, new, 1)
            path.rename(path.parent / new_name)


def _extract_rstid_and_day(directory: Path) -> tuple:
    """
    Replaces:
        set RSTID = `/bin/ls *catch* | /bin/grep -Po '^.*(?=\\.\\w+_rst\\.)'`
        set day   = `/bin/ls *catch* | /bin/grep -Po '(?<=\\d{6})\\d{2}(?=_21z)'`
    """
    catch_files = sorted(p.name for p in directory.glob("*catch*"))
    if not catch_files:
        raise FileNotFoundError(f"No '*catch*' restart files found in {directory}")

    first = catch_files[0]

    rstid_match = re.match(r"^(.*)(?=\.\w+_rst\.)", first)
    if not rstid_match:
        raise ValueError(f"Could not parse RSTID from filename: {first}")
    rstid = rstid_match.group(1)

    day_match = re.search(r"(?<=\d{6})\d{2}(?=_21z)", first)
    if not day_match:
        raise ValueError(f"Could not parse day from filename: {first}")
    day = day_match.group(0)

    return rstid, day


def _detect_file_extension(file_path: Path) -> str:
    """
    Replaces:
        set  chk_type = `ls -1 <file> | xargs /usr/bin/file -Lb --mime-type`
        if( "$chk_type" =~ "application/octet-stream" ) set ext = bin
        if( "$chk_type" =~ "application/x-hdf"        ) set ext = nc4
    """
    result = subprocess.run(
        ["file", "-Lb", "--mime-type", str(file_path)],
        capture_output=True,
        text=True,
        check=True,
    )
    mime_type = result.stdout.strip()

    if "application/octet-stream" in mime_type:
        return "bin"
    if "application/x-hdf" in mime_type:
        return "nc4"

    raise ValueError(f"Unrecognized restart file mime-type for {file_path}: {mime_type}")


def stage_gcmemip_restarts(
    exp_dir: Path | str,
    rstdate: str,
    agcm_im: int,
    agcm_lm: int,
    geosbin: Path | str,
    land_bcs: str,
    ocean_out: str,
    bc_base: str,
    remip_source_dir: str = _DEFAULT_REMIP_SOURCE_DIR,
) -> None:
    """
    Top-level entry point: stage the initial GCMEMIP restarts for the
    given RSTDATE ensemble member, if not already done.

    Args:
        exp_dir:          Experiment directory (EXPDIR/HOMDIR).
        rstdate:          Restart date string, e.g. '199511' (YYYYMM[_...]).
        agcm_im:          AGCM_IM (used for the regrid target resolution).
        agcm_lm:          AGCM_LM (used for the regrid target levels).
        geosbin:          Path to the GEOS bin directory (remap_restarts.py).
        land_bcs:         Target land BCS tag (run_config['land']['bcs']).
        ocean_out:        Target ocean grid tag (run_config['ocean']['out']).
        bc_base:          Output BC base directory (run_config['bc_base']).
        remip_source_dir: Directory containing the source REMIP restart
                           tarballs (defaults to the original hardcoded
                           sdrabenh path).
    """
    exp_dir = Path(exp_dir)
    geosbin = Path(geosbin)
    rst_dir = exp_dir / "restarts" / rstdate

    cap_restart_path = rst_dir / "cap_restart"
    if cap_restart_path.exists():
        return

    rst_dir.mkdir(parents=True, exist_ok=True)

    # -- Normalize CAP.rc --
    cap_rc_orig = rst_dir / "CAP.rc.orig"
    cap_rc = rst_dir / "CAP.rc"
    cap_rc_orig.write_text((exp_dir / "CAP.rc").read_text())
    cap_rc.write_text(normalize_whitespace(cap_rc_orig.read_text()))

    year, month = _parse_year_month(rstdate)

    # -- Copy restarts from v11.5.2 REPLAY to MERRA-2 --
    tar_name = f"restarts.e{year}{month}10_21z.tar"
    tar_link = rst_dir / tar_name
    tar_link.symlink_to(Path(remip_source_dir) / tar_name)

    with tarfile.open(tar_link) as tar:
        members = [m for m in tar.getmembers() if "_internal_rst" in m.name]
        tar.extractall(path=rst_dir, members=members)

    # -- Regrid v11.5.2 restarts --
    rstid, day = _extract_rstid_and_day(rst_dir)

    subprocess.run(
        [
            str(geosbin / "remap_restarts.py"), "command_line",
            "-np",
            "-ymdh", f"{year}{month}{day}21",
            "-grout", f"C{agcm_im}",
            "-levsout", str(agcm_lm),
            "-out_dir", ".",
            "-rst_dir", ".",
            "-expid", rstid,
            "-bcvin", "NL3",
            "-oceanin", "1440x720",
            "-in_bc_base", "/discover/nobackup/projects/gmao/bcs_shared/fvInput/ExtData/esm/tiles",
            "-newid", "regrid",
            "-nobkg",
            "-nolcv",
            "-bcvout", land_bcs,
            "-rs", "3",
            "-oceanout", ocean_out,
            "-out_bc_base", bc_base,
        ],
        cwd=rst_dir,
        check=True,
    )

    imc = _zero_pad_im(agcm_im)  # noqa: F841 (mirrors gcm_run.j; not otherwise used downstream)

    regridded_file = rst_dir / f"regrid.catch*_internal_rst.{year}{month}{day}_21z.nc4"
    matches = list(rst_dir.glob(f"regrid.catch*_internal_rst.{year}{month}{day}_21z.nc4"))
    if not matches:
        raise FileNotFoundError(f"No regridded catch restart found matching {regridded_file}")
    ext = _detect_file_extension(matches[0])

    _bulk_stripname(rst_dir, "regrid.")
    _bulk_stripname(rst_dir, f".{year}{month}{day}_21z.{ext}")

    # -- Create CAP.rc and cap_restart --
    nymd = f"{year}{month}{day}"
    nhms = "210000"
    cap_restart_path.write_text(f"{nymd} {nhms}\n")

    clock = create_clock(nymd, nhms)
    curmonth = clock.month
    count = 0
    while count < 4:
        clock = advance_clock(clock, secs_to_add=num_seconds_per_day)
        if curmonth != clock.month:
            curmonth = clock.month
            count += 1

    new_end_date = f"END_DATE: {clock.strftime('%Y')}{clock.strftime('%m')}01 210000"
    old_end_date_line = extract_line_starting_with(str(cap_rc), "END_DATE:")
    if old_end_date_line:
        search_replace_in_file(str(cap_rc), re.escape(old_end_date_line), new_end_date)

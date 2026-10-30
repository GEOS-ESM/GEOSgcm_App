#!/usr/bin/env python3
"""
GEOS Standalone Forecast Setup and 6-Way Verification Pipeline.
Enforces strict 6-hourly verification cadence across all analysis and forecast datasets.
Includes partition discovery and unique cycle-tagged stats log files to prevent clobbering.

python3 gcm_forecast.py 20251203
python3 gcm_forecast.py 20251203 --stats-only
python3 gcm_forecast.py 20251206 20260126 3

"""

import os
import sys
import re
import math
import glob
import subprocess
from datetime import datetime, timedelta
from pathlib import Path


def parse_uncommented(filepath: Path, pattern: str, default: str = "") -> str:
    """Extracts first regex match from uncommented lines in a file."""
    if not filepath.is_file():
        return default
    regex = re.compile(pattern)
    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            line_str = line.strip()
            if not line_str.startswith("#"):
                match = regex.search(line_str)
                if match:
                    return match.group(1).strip()
    return default


def parse_env_or_set(filepath: Path, var: str, default: str = "") -> str:
    """Extracts variables defined as either 'setenv VAR val' or 'set VAR = val'."""
    pattern = rf"^\s*(?:setenv\s+{var}\s+|set\s+{var}\s*=\s*)([^\s\n#]+)"
    return parse_uncommented(filepath, pattern, default)


def parse_sbatch_opt(filepath: Path, key: str, default: str = "") -> str:
    """Parses active #SBATCH options while skipping commented ##SBATCH directives."""
    if not filepath.is_file():
        return default

    if key == "partition":
        pattern = re.compile(rf"^#SBATCH\s+.*(?:--partition[=\s]|-p\s+|-p=)([^\s]+)")
    else:
        pattern = re.compile(rf"^#SBATCH\s+.*--{key}[=\s]([^\s]+)")

    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            line_str = line.strip()
            if line_str.startswith("#SBATCH") and not line_str.startswith("##"):
                match = pattern.search(line_str)
                if match:
                    return match.group(1).strip()
    return default


def estimate_walltime(homedir: Path, ndays: int, extra_hours: float = 3.0) -> tuple:
    """
    Parses recent Throughput(days/day) lines from gcm_run.o* logs,
    averages the middle column, and calculates walltime with a 20% cushion.
    """
    log_candidates = sorted(glob.glob(str(homedir / "gcm_run.o*")), key=os.path.getmtime)
    if not log_candidates:
        log_candidates = sorted(glob.glob(str(homedir.parent / "gcm_run.o*")), key=os.path.getmtime)

    throughput_values = []
    tp_regex = re.compile(r"Throughput\(days/day\)\[Avg\s+Tot\s+Run\]:\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)")

    if log_candidates:
        latest_log = log_candidates[-1]
        with open(latest_log, "r", encoding="utf-8", errors="ignore") as f:
            for line in f:
                match = tp_regex.search(line)
                if match:
                    middle_val = float(match.group(2))
                    throughput_values.append(middle_val)

    samples = throughput_values[-30:] if len(throughput_values) >= 30 else throughput_values

    if samples:
        avg_throughput = sum(samples) / len(samples)
        sim_days = float(ndays) + (extra_hours / 24.0)

        # Baseline hours = (Simulated Days / Throughput Rate) * 24 hours/day
        est_hours = (sim_days / avg_throughput) * 24.0

        # Apply 20% cushion + 10 min startup/I/O teardown allowance
        buffered_hours = (est_hours * 1.20) + (10.0 / 60.0)

        total_secs = int(buffered_hours * 3600)
        if total_secs < 1800:
            total_secs = 1800

        hrs = total_secs // 3600
        mins = (total_secs % 3600) // 60
        secs = total_secs % 60
        walltime_str = f"{hrs:02d}:{mins:02d}:{secs:02d}"
        return walltime_str, avg_throughput
    else:
        return "04:00:00", None


def get_config(homedir: Path) -> dict:
    """Reads runtime setup and topology from gcm_run.j, AGCM.rc, and CAP.rc."""
    gcm_run = homedir / "gcm_run.j"
    agcm_rc = homedir / "AGCM.rc"
    cap_rc = homedir / "CAP.rc"

    cfg = {}
    cfg["EXPID"] = parse_env_or_set(gcm_run, "EXPID", "GEOS_EXP")
    cfg["EXPDIR"] = parse_env_or_set(gcm_run, "EXPDIR", str(homedir.resolve()))
    cfg["HOMDIR"] = parse_env_or_set(gcm_run, "HOMDIR", str(homedir.resolve()))
    cfg["SSDDIR"] = parse_env_or_set(gcm_run, "SSDDIR", cfg["EXPDIR"])
    cfg["GEOSDIR"] = parse_env_or_set(gcm_run, "GEOSDIR", "")
    cfg["GEOSBIN"] = f"{cfg['GEOSDIR']}/bin"
    cfg["GEOSETC"] = f"{cfg['GEOSDIR']}/etc"
    cfg["GEOSUTIL"] = cfg["GEOSDIR"]

    # Resolution & Layout
    cfg["NX"] = int(parse_uncommented(agcm_rc, r"^\s*NX:\s*(\d+)", "1"))
    cfg["NY"] = int(parse_uncommented(agcm_rc, r"^\s*NY:\s*(\d+)", "1"))
    cfg["AGCM_IM"] = parse_uncommented(agcm_rc, r"^\s*AGCM_IM:\s*(\d+)", "576")
    cfg["AGCM_JM"] = parse_uncommented(agcm_rc, r"^\s*AGCM_JM:\s*(\d+)", "361")
    cfg["AGCM_LM"] = parse_uncommented(agcm_rc, r"^\s*AGCM_LM:\s*(\d+)", "72")
    cfg["IONODES"] = int(parse_uncommented(agcm_rc, r"^\s*IOSERVER_NODES:\s*(\d+)", "0"))
    cfg["NUM_BACKEND_PES"] = parse_uncommented(agcm_rc, r"^\s*NUM_BACKEND_PES:\s*(\d+)", "1")

    # SLURM Options
    cfg["TPNODE"] = int(parse_sbatch_opt(gcm_run, "ntasks-per-node", "120"))
    cfg["CONSTRAINT"] = parse_sbatch_opt(gcm_run, "constraint", "mil")
    cfg["ACCOUNT"] = parse_sbatch_opt(gcm_run, "account", "s1062")
    cfg["PARTITION"] = parse_sbatch_opt(gcm_run, "partition", "compute")
    cfg["THRDS"] = int(parse_env_or_set(gcm_run, "OMP_NUM_THREADS", "1"))

    # Segment Length & Forecast Hour
    fcst_seg = parse_uncommented(cap_rc, r"^\s*FCST_SEGMENT:\s*([^\n#]+)", "")
    if not fcst_seg:
        fcst_seg = parse_uncommented(cap_rc, r"^\s*JOB_SGMT:\s*([^\n#]+)", "00000005 000000")

    tokens = fcst_seg.split()
    days = int(tokens[0]) if tokens else 5
    time_str = tokens[1] if len(tokens) > 1 else "000000"

    cfg["NDAYS"] = days
    cfg["FSEGMENT"] = f"{days:08d}"
    cfg["FHOUR"] = f"{time_str[:2]:0>2}"
    cfg["FLOOP"] = days + 1
    cfg["FHRS"] = days * 24

    # Total 6-hourly verification output steps (e.g. 5 days = 120h / 6h + 1 = 21 steps)
    cfg["NSTEPS_6HR"] = (cfg["FHRS"] // 6) + 1

    # Parallel Geometry Calculations
    model_npes = cfg["NX"] * cfg["NY"]
    model_nodes = math.ceil(model_npes / cfg["TPNODE"])
    total_nodes = model_nodes + cfg["IONODES"]
    total_pes = total_nodes * cfg["TPNODE"] if cfg["IONODES"] > 0 else model_npes

    cfg["MODEL_NPES"] = model_npes
    cfg["TOTAL_NODES"] = total_nodes
    cfg["TOTAL_PES"] = total_pes

    if cfg["IONODES"] > 0:
        cfg["IOSERVER_OPTS"] = (
            f"--npes_model {model_npes} --nodes_output_server {cfg['IONODES']} "
            f"--oserver_type multigroup --npes_backend_pernode {cfg['NUM_BACKEND_PES']}"
        )
    else:
        cfg["IOSERVER_OPTS"] = ""

    # Calculate dynamic walltime
    walltime, avg_tp = estimate_walltime(homedir, days, extra_hours=3.0)
    cfg["WALLTIME"] = walltime
    cfg["AVG_THROUGHPUT"] = avg_tp

    return cfg


# ===========================================================================
# Forecast CSH Template
# ===========================================================================
CSH_FCST_BODY = r"""#!/bin/csh -f
#SBATCH --time=__WALLTIME__
#SBATCH --nodes=__TOTAL_NODES__ --ntasks-per-node=__TPNODE__
#SBATCH --job-name=__EXPID___FCST___DATE__
#SBATCH --partition=__PARTITION__
#SBATCH --constraint=__CONSTRAINT__
#SBATCH --account=__ACCOUNT__
#SBATCH --output=__LOG_FILE__

umask 022
limit stacksize unlimited

setenv ARCH `uname`
setenv SITE NCCS
setenv GEOSDIR  __GEOSDIR__
setenv GEOSBIN  __GEOSBIN__
setenv GEOSETC  __GEOSETC__
setenv GEOSUTIL __GEOSUTIL__
setenv SSDDIR   __SSDDIR__

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${GEOSDIR}/lib
if ( $?BASEDIR ) setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "
setenv GCMVER `cat $GEOSETC/.AGCM_VERSION`

setenv EXPID  __EXPID__
setenv EXPDIR __EXPDIR__
setenv HOMDIR __HOMDIR__

set date0 = __DATE__
set time0 = __HOUR__
set hour  = `echo $time0 | cut -c1-2`
set fdate = ${date0}_${hour}z

set FCST_LABL = __FCST_LABL__
setenv FCSTDIR $EXPDIR/forecasts/${FCST_LABL}_$fdate
setenv SCRDIR  $FCSTDIR/scratch

/bin/rm -rf $SCRDIR
mkdir -p $SCRDIR
cd $SCRDIR

# Initialize cap_restart stepped back 3 hours (-10800s) for replay
set date_init = `$GEOSUTIL/post/tick $date0 $time0 -10800`
set nymd0     = $date_init[1]
set nhms0     = $date_init[2]
set hour0     = `echo $nhms0 | cut -c1-2`
echo $nymd0 $nhms0 > cap_restart

# Copy resource files
cp -f $EXPDIR/RC/* .
cp    $EXPDIR/linkbcs .
cp -f $HOMDIR/*.rc $HOMDIR/*.nml $HOMDIR/*.yaml .
cp    $GEOSBIN/bundleParser.py .
cat fvcore_layout.rc >> input.nml

# Configure AGCM.rc and CAP.rc
sed -i '/^\s*NX:/c\NX: '__NX__ AGCM.rc
sed -i '/^\s*NY:/c\NY: '__NY__ AGCM.rc
sed -i '/^\s*RECORD_FINAL:/c\RECORD_FINAL: NO' AGCM.rc
sed -i 's|^.*RECORD_FREQUENCY:|#&|g; s|^.*RECORD_REF_DATE:|#&|g; s|^.*RECORD_REF_TIME:|#&|g' AGCM.rc
sed -i '/REPLAY_SHUTOFF:/c\REPLAY_SHUTOFF: 21600' AGCM.rc
sed -i "/JOB_SGMT:/c\JOB_SGMT: __FSEGMENT__ 030000" CAP.rc

# Create HISTORY.rc
set HIST_IM = `expr __AGCM_IM__ \* 4`
set HIST_JM = `expr __AGCM_IM__ \* 2 + 1`
if ( $HIST_IM < 576 ) then
  set HIST_IM = 576
  set HIST_JM = 361
endif

cat << _EOF_ > HISTORY.rc
VERSION: 1
EXPID:  ${EXPID}_FCST
EXPDSC: Forecasts_spawned_from_${EXPID}

COLLECTIONS: 'geosgcm_fcst'
             'geosgcm_budi'
             'inst6_2d_asm_Nx'
             'tavg3_2d_aer_x'
             'aer_comp_trigger'
              ::

GRID_LABELS: PC576x361-DC
             PC${HIST_IM}x${HIST_JM}-DC
             ::

  PC576x361-DC.GRID_TYPE: LatLon
  PC576x361-DC.IM_WORLD: 576
  PC576x361-DC.JM_WORLD: 361
  PC576x361-DC.POLE: PC
  PC576x361-DC.DATELINE: DC
  PC576x361-DC.LM: __AGCM_LM__

  PC${HIST_IM}x${HIST_JM}-DC.GRID_TYPE: LatLon
  PC${HIST_IM}x${HIST_JM}-DC.IM_WORLD: ${HIST_IM}
  PC${HIST_IM}x${HIST_JM}-DC.JM_WORLD: ${HIST_JM}
  PC${HIST_IM}x${HIST_JM}-DC.POLE: PC
  PC${HIST_IM}x${HIST_JM}-DC.DATELINE: DC
  PC${HIST_IM}x${HIST_JM}-DC.LM: __AGCM_LM__

  geosgcm_fcst.format:        'CFIO' ,
  geosgcm_fcst.grid_label:     PC576x361-DC ,
  geosgcm_fcst.template:      '%y4%m2%d2_%h2%n2z.nc4' ,
  geosgcm_fcst.archive:       '%c/Y%y4' ,
  geosgcm_fcst.mode:          'instantaneous' ,
  geosgcm_fcst.frequency:      060000 ,
  geosgcm_fcst.duration:       000000 ,
  geosgcm_fcst.ref_time:       000000 ,
  geosgcm_fcst.deflate:        1 ,
  geosgcm_fcst.regrid_method: 'BILINEAR' ,
  geosgcm_fcst.vscale:         100.0 ,
  geosgcm_fcst.vunit:         'hPa' ,
  geosgcm_fcst.vvars:         'log(PLE)' , 'DYN' ,
  geosgcm_fcst.levels:         1000 975 950 925 900 850 800 750 700 600 500 400 300 250 200 150 100 70 50 30 10 7 5 3 1,
  geosgcm_fcst.fields:        'SLP'    , 'DYN'   ,
                              'U;V'    , 'DYN'   ,
                              'T'      , 'DYN'   ,
                              'PS'     , 'DYN'   ,
                              'ZLE'    , 'DYN'   , 'H'   ,
                              'Q'      , 'MOIST' , 'QV'  ,
                               ::

  geosgcm_budi.format:        'CFIO' ,
  geosgcm_budi.grid_label:     PC576x361-DC ,
  geosgcm_budi.template:      '%y4%m2%d2_%h2%n2z.nc4' ,
  geosgcm_budi.archive:       '%c/Y%y4' ,
  geosgcm_budi.mode:          'instantaneous' ,
  geosgcm_budi.frequency:      060000 ,
  geosgcm_budi.duration:       000000 ,
  geosgcm_budi.ref_time:       000000 ,
  geosgcm_budi.deflate:        1 ,
  geosgcm_budi.regrid_method: 'CONSERVE',
  geosgcm_budi.fields:        'TQV' , 'AGCM' ,
                              'TQI' , 'AGCM' ,
                              'TQL' , 'AGCM' ,
                               ::

  inst6_2d_asm_Nx.format:     'CFIO' ,
  inst6_2d_asm_Nx.grid_label:  PC576x361-DC ,
  inst6_2d_asm_Nx.template:   '%y4%m2%d2_%h2%n2z.nc4' ,
  inst6_2d_asm_Nx.archive:    '%c/Y%y4' ,
  inst6_2d_asm_Nx.mode:       'instantaneous' ,
  inst6_2d_asm_Nx.frequency:   060000 ,
  inst6_2d_asm_Nx.duration:    000000 ,
  inst6_2d_asm_Nx.ref_time:    000000 ,
  inst6_2d_asm_Nx.deflate:     1 ,
  inst6_2d_asm_Nx.regrid_method: 'BILINEAR' ,
  inst6_2d_asm_Nx.fields:     'SLP'        , 'DYN'     ,
                              'U10M;V10M'  , 'SURFACE' ,
                              'T2M'        , 'SURFACE' ,
                              'T2MDEW'     , 'SURFACE' ,
                              'CLDTT'      , 'SOLAR'   , 'TCC',
                              'MUCAPE'     , 'MOIST'   ,
                              'MUCIN'      , 'MOIST'   ,
                              'CLDBASEHGT' , 'MOIST'   ,
                               ::

  tavg3_2d_aer_x.format:      'CFIO' ,
  tavg3_2d_aer_x.grid_label:   PC${HIST_IM}x${HIST_JM}-DC ,
  tavg3_2d_aer_x.template:    '%y4%m2%d2_%h2%n2z.nc4' ,
  tavg3_2d_aer_x.archive:     '%c/Y%y4' ,
  tavg3_2d_aer_x.mode:        'time-averaged' ,
  tavg3_2d_aer_x.frequency:    030000 ,
  tavg3_2d_aer_x.duration:     000000 ,
  tavg3_2d_aer_x.ref_time:     210000 ,
  tavg3_2d_aer_x.deflate:      1 ,
  tavg3_2d_aer_x.regrid_method:'CONSERVE' ,
  tavg3_2d_aer_x.fields:      'PM25'        , 'GOCART2G' ,
                              'TOTEXTTAU'   , 'GOCART2G' ,
                               ::

  aer_comp_trigger.format:    'CFIO' ,
  aer_comp_trigger.grid_label: PC576x361-DC ,
  aer_comp_trigger.template:  '%y4%m2%d2_%h2%n2z.nc4' ,
  aer_comp_trigger.mode:      'instantaneous' ,
  aer_comp_trigger.frequency:  240000 ,
  aer_comp_trigger.duration:   000000 ,
  aer_comp_trigger.ref_time:   000000 ,
  aer_comp_trigger.deflate:    1 ,
  aer_comp_trigger.regrid_method: 'BILINEAR' ,
  aer_comp_trigger.fields:    'DUSMASS25'   , 'DU'    ,
                              'SSSMASS25'   , 'SS'    ,
                              'CA.bcSMASS'  , 'CA.bc' ,
                              'CA.ocSMASS'  , 'CA.oc' ,
                              'CA.brSMASS'  , 'CA.br' ,
                              'SO4SMASS'    , 'SU'    ,
                              'NISMASS25'   , 'NI'    ,
                              'NH4SMASS'    , 'NI'    ,
                              'DUEXTTAU'    , 'DU'    ,
                              'SSEXTTAU'    , 'SS'    ,
                              'CA.brEXTTAU' , 'CA.br' ,
                              'CA.bcEXTTAU' , 'CA.bc' ,
                              'CA.ocEXTTAU' , 'CA.oc' ,
                              'SUEXTTAU'    , 'SU'    ,
                              'NIEXTTAU'    , 'NI'    ,
                               ::
_EOF_

# Boundary Conditions & Binary Tile
setenv BCSDIR  /discover/nobackup/projects/gmao/bcs_shared/fvInput/ExtData/esm/tiles/NL3
setenv SSTDIR  /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/g5gcm/bcs/realtime/OSTIA_REYNOLDS/360x2160
setenv BCRSLV  CF0360x6C_CF0360x6C
setenv BCTAG   `basename $BCSDIR`
setenv EMISSIONS OPS_EMISSIONS
setenv YEAR    `echo $nymd0 | cut -b1-4`
chmod +x linkbcs
./linkbcs

if (! -e tile.bin) $GEOSBIN/binarytile.x tile.data tile.bin
if (! -e gwd_internal_rst) sed -i '/NCAR_NRDG:/c\NCAR_NRDG: 0' AGCM.rc

# Copy Executable & Link Restarts
/bin/cp $EXPDIR/GEOSgcm.x .
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
foreach rst ( `grep "RESTART_FILE" AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2 | sed -e 's/^[+-]//'` )
  set rfile = `/bin/ls -1 $EXPDIR/restarts/*${rst}.${edate}*`
  /bin/ln -s $rfile $rst
end

$GEOSBIN/construct_extdata_yaml_list.py GEOS_ChemGridComp.rc
touch ExtData.rc

# MPI and Parallel Runtime Environment
setenv OMP_NUM_THREADS __THRDS__
setenv I_MPI_ADJUST_ALLREDUCE 12
setenv I_MPI_ADJUST_GATHERV 3
setenv I_MPI_FABRICS shm:ofi
setenv I_MPI_OFI_PROVIDER psm3
setenv FI_PSM3_CONN_TIMEOUT 120

python bundleParser.py

# Link REPLAY forcing
set ANA_LOCATION = `grep '^\s*REPLAY_ANA_LOCATION:' AGCM.rc | cut -d: -f2`
set REPLAY_FILE  = `grep '^\s*REPLAY_FILE:' AGCM.rc | cut -d: -f2 | cut -d"/" -f1 | grep -v %`
if ( "$REPLAY_FILE" != "" ) /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE} .

# Execute GEOSgcm.x
$RUN_CMD __TOTAL_PES__ ./GEOSgcm.x __IOSERVER_OPTS__ --logging_config 'logging.yaml'

if ( -e EGRESS ) then
   echo "GEOSgcm Run Status: SUCCESS"
   /bin/mv *.geosgcm_* *.inst6_* *.tavg3_* $FCSTDIR >& /dev/null
   /bin/rm -f $FCSTDIR/*.aer_comp_trigger.*
   cd $FCSTDIR
   /bin/rm -rf $SCRDIR
   exit 0
else
   echo "GEOSgcm Run Status: FAILED"
   exit 1
endif
"""


# ===========================================================================
# Consolidated Stats CSH Template (Strict 6-Hourly & Unique Cycle Logs)
# ===========================================================================
CSH_STATS_BODY = r"""#!/bin/csh -f
#SBATCH --time=0:35:00
#SBATCH --nodes=1 --ntasks-per-node=12
#SBATCH --job-name=__EXPID___STATS___DATE__
#SBATCH --partition=__PARTITION__
#SBATCH --constraint=__CONSTRAINT__
#SBATCH --account=__ACCOUNT__
#SBATCH --output=__LOG_FILE__

umask 022
limit stacksize unlimited
set nonomatch

setenv ARCH `uname`
setenv SITE NCCS
setenv GEOSDIR  __GEOSDIR__
setenv GEOSBIN  __GEOSBIN__
setenv GEOSETC  __GEOSETC__
setenv GEOSUTIL __GEOSUTIL__

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${GEOSDIR}/lib
if ( $?BASEDIR ) setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

setenv EXPID  __EXPID__
setenv EXPDIR __EXPDIR__
setenv HOMDIR __HOMDIR__

set date0   = __DATE__
set time0   = __HOUR__
set hour    = `echo $time0 | cut -c1-2`
set fdate   = ${date0}_${hour}z
set FCST_ID = ${EXPID}_FCST
set FCST_LABL = __FCST_LABL__

setenv FCSTDIR $EXPDIR/forecasts/${FCST_LABL}_$fdate
set yyyymm = `echo $date0 | cut -b1-6`
set FDAYS  = `printf "%02d" __NDAYS__`

set fcst_files = ( `/bin/ls -1 $FCSTDIR/${FCST_ID}.geosgcm_fcst*nc4` )
set ANA_LOCATION = `grep '^\s*REPLAY_ANA_LOCATION:' $HOMDIR/AGCM.rc | cut -d: -f2`
set ANA_EXPID    = `grep '^\s*REPLAY_ANA_EXPID:'    $HOMDIR/AGCM.rc | cut -d: -f2`

echo "Gathering 6-hourly verification truth files for forecast: $fdate"

# Pre-gather verification truth files by strictly stepping 6 hours (21600s)
set self_files = ()
set fp_files   = ()
set era5_files = ()
set cur_date   = $date0
set cur_time   = $time0

@ step = 1
while ($step <= __NSTEPS_6HR__)
  set yr = `echo $cur_date | cut -b1-4`
  set mo = `echo $cur_date | cut -b5-6`
  set hr = `echo $cur_time | cut -c1-2`

  # 1. SELF (Experiment holding 6-hourly progs)
  set match_self = ( `/bin/ls -1 $EXPDIR/holding/geosgcm_prog/${yr}*/$EXPID.geosgcm_prog.${cur_date}_${hr}*nc4` )
  if ( $#match_self > 0 ) set self_files = ( $self_files $match_self )

  # 2. GEOS-FP (Operational 3D instant analysis files at HHz)
  set match_fp = ( `/bin/ls -1 /discover/nobackup/projects/gmao/geos_fp_arch/${ANA_EXPID}/ana/Y$yr/M$mo/${ANA_EXPID}.*.${cur_date}_${hr}*nc4` )
  if ( $#match_fp == 0 ) then
     set match_fp = ( `/bin/ls -1 ${ANA_LOCATION}/asm/Y$yr/M$mo/${ANA_EXPID}.*.${cur_date}_${hr}*nc4` )
  endif
  if ( $#match_fp > 0 ) set fp_files = ( $fp_files $match_fp )

  # 3. ERA5 (ERA5 pressure levels dataset)
  set match_era5 = ( `/bin/ls -1 /css/era5/pressure_level_hourly/Y$yr/M$mo/era5_pl-allvar.${cur_date}_${hr}*nc4` )
  if ( $#match_era5 > 0 ) set era5_files = ( $era5_files $match_era5 )

  # Advance date by 6 hours (21600 seconds)
  set dt_step  = `$GEOSUTIL/post/tick $cur_date $cur_time 21600`
  set cur_date = $dt_step[1]
  set cur_time = $dt_step[2]
  @ step = $step + 1
end

echo "Truth files identified (6-hourly cadence):"
echo "  SELF   : $#self_files files"
echo "  GEOS-FP: $#fp_files files"
echo "  ERA5   : $#era5_files files"

# Climatologies
set clim_m2   = ( `/bin/ls -1 /discover/nobackup/projects/gmao/geos_itv/mefrazer/METplus/clim/trimmed/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_[01][0268]z.576x361.data.nc4` )
set levs_m2   = ( 1000 975 950 925 900 875 850 825 800 775 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10 7 5 3 2 1 )

set clim_era5 = ( `/bin/ls -1 /discover/nobackup/projects/gmao/geos_itv/mefrazer/METplus/clim/trimmed/era5_mnth.mrg1_avg.1440x0721.Y1985-2014.T[01][0268]z.nc4` )
set levs_era5 = ( 1000 975 950 925 900 875 850 825 800 775 750 700 650 600 550 500 450 400 350 300 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1 )

echo "Launching all 6 verification combinations in parallel..."

# 1. SELF vs MERRA-2
( \
  set sdir = $EXPDIR/forecasts/${FCST_LABL}.stats.SELF_MERRA2.$yyyymm.f${FDAYS}day ; \
  mkdir -p $sdir ; cd $sdir ; \
  echo "[START] SELF / MERRA2 ($fdate)" ; \
  $RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $self_files -cli $clim_m2 \
                                    -levs $levs_m2 -tag ${EXPID}_vs_SELF -nfreq 060000 \
                                    -rc $GEOSUTIL/post/stats.rc -fhour __FHRS__ >& stats_SELF_MERRA2_${fdate}.log ; \
  echo "[DONE ] SELF / MERRA2 ($fdate)" \
) &

# 2. SELF vs ERA5
( \
  set sdir = $EXPDIR/forecasts/${FCST_LABL}.stats.SELF_ERA5.$yyyymm.f${FDAYS}day ; \
  mkdir -p $sdir ; cd $sdir ; \
  echo "[START] SELF / ERA5 ($fdate)" ; \
  $RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $self_files -cli $clim_era5 \
                                    -levs $levs_era5 -tag ${EXPID}_vs_SELF -nfreq 060000 \
                                    -rc $GEOSUTIL/post/stats.rc -fhour __FHRS__ >& stats_SELF_ERA5_${fdate}.log ; \
  echo "[DONE ] SELF / ERA5 ($fdate)" \
) &

# 3. GEOS-FP vs MERRA-2
( \
  set sdir = $EXPDIR/forecasts/${FCST_LABL}.stats.GEOS-FP_MERRA2.$yyyymm.f${FDAYS}day ; \
  mkdir -p $sdir ; cd $sdir ; \
  echo "[START] GEOS-FP / MERRA2 ($fdate)" ; \
  $RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $fp_files -cli $clim_m2 \
                                    -levs $levs_m2 -tag ${EXPID}_vs_GEOS-FP -nfreq 060000 \
                                    -rc $GEOSUTIL/post/stats.rc -fhour __FHRS__ >& stats_GEOS-FP_MERRA2_${fdate}.log ; \
  echo "[DONE ] GEOS-FP / MERRA2 ($fdate)" \
) &

# 4. GEOS-FP vs ERA5
( \
  set sdir = $EXPDIR/forecasts/${FCST_LABL}.stats.GEOS-FP_ERA5.$yyyymm.f${FDAYS}day ; \
  mkdir -p $sdir ; cd $sdir ; \
  echo "[START] GEOS-FP / ERA5 ($fdate)" ; \
  $RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $fp_files -cli $clim_era5 \
                                    -levs $levs_era5 -tag ${EXPID}_vs_GEOS-FP -nfreq 060000 \
                                    -rc $GEOSUTIL/post/stats.rc -fhour __FHRS__ >& stats_GEOS-FP_ERA5_${fdate}.log ; \
  echo "[DONE ] GEOS-FP / ERA5 ($fdate)" \
) &

# 5. ERA5 vs MERRA-2
( \
  set sdir = $EXPDIR/forecasts/${FCST_LABL}.stats.ERA5_MERRA2.$yyyymm.f${FDAYS}day ; \
  mkdir -p $sdir ; cd $sdir ; \
  echo "[START] ERA5 / MERRA2 ($fdate)" ; \
  $RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $era5_files -cli $clim_m2 \
                                    -levs $levs_m2 -tag ${EXPID}_vs_ERA5 -nfreq 060000 \
                                    -rc $GEOSUTIL/post/stats.rc -fhour __FHRS__ >& stats_ERA5_MERRA2_${fdate}.log ; \
  echo "[DONE ] ERA5 / MERRA2 ($fdate)" \
) &

# 6. ERA5 vs ERA5
( \
  set sdir = $EXPDIR/forecasts/${FCST_LABL}.stats.ERA5_ERA5.$yyyymm.f${FDAYS}day ; \
  mkdir -p $sdir ; cd $sdir ; \
  echo "[START] ERA5 / ERA5 ($fdate)" ; \
  $RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $era5_files -cli $clim_era5 \
                                    -levs $levs_era5 -tag ${EXPID}_vs_ERA5 -nfreq 060000 \
                                    -rc $GEOSUTIL/post/stats.rc -fhour __FHRS__ >& stats_ERA5_ERA5_${fdate}.log ; \
  echo "[DONE ] ERA5 / ERA5 ($fdate)" \
) &

wait
echo "All 6 verification stats completed successfully for $fdate."
"""


def render(template: str, rmap: dict) -> str:
    """Substitutes dictionary values and guarantees no unreplaced tokens remain."""
    out = template
    for k, v in rmap.items():
        out = out.replace(k, str(v))
    unreplaced = re.findall(r"__[A-Z0-9_]+__", out)
    if unreplaced:
        print(f"Template Error: Unreplaced tokens {set(unreplaced)}", file=sys.stderr)
        sys.exit(1)
    return out


def main():
    args = [arg for arg in sys.argv[1:] if not arg.startswith("--")]
    flags = [arg for arg in sys.argv[1:] if arg.startswith("--")]

    stats_only = any(flag in ("--stats-only", "--stats") for flag in flags)

    if not args:
        print("Usage:")
        print("  Full Workflow : python3 gcm_forecast.py <BDATE: YYYYMMDD> [EDATE: YYYYMMDD] [FREQ_DAYS]")
        print("  Stats Only    : python3 gcm_forecast.py <BDATE: YYYYMMDD> [EDATE: YYYYMMDD] [FREQ_DAYS] --stats-only")
        sys.exit(1)

    bdate_str = args[0]
    edate_str = args[1] if len(args) > 1 and args[1] != "." else bdate_str
    freq_days = int(args[2]) if len(args) > 2 else 1

    homedir = Path(os.getcwd()).parent if Path("../gcm_run.j").exists() else Path(os.getcwd())
    cfg = get_config(homedir)

    fcst_dir = Path(cfg["EXPDIR"]) / "forecasts"
    fcst_dir.mkdir(parents=True, exist_ok=True)

    curr_dt = datetime.strptime(bdate_str, "%Y%m%d")
    end_dt = datetime.strptime(edate_str, "%Y%m%d")

    while curr_dt <= end_dt:
        date_str = curr_dt.strftime("%Y%m%d")
        fcst_label = "P10800_C21600_T21600"
        fcst_type = "CYCLED_REPLAY"

        rmap = {
            "__EXPID__": cfg["EXPID"],
            "__EXPDIR__": cfg["EXPDIR"],
            "__HOMDIR__": cfg["HOMDIR"],
            "__SSDDIR__": cfg["SSDDIR"],
            "__GEOSDIR__": cfg["GEOSDIR"],
            "__GEOSBIN__": cfg["GEOSBIN"],
            "__GEOSETC__": cfg["GEOSETC"],
            "__GEOSUTIL__": cfg["GEOSUTIL"],
            "__PARTITION__": cfg["PARTITION"],
            "__CONSTRAINT__": cfg["CONSTRAINT"],
            "__ACCOUNT__": cfg["ACCOUNT"],
            "__TPNODE__": cfg["TPNODE"],
            "__TOTAL_NODES__": cfg["TOTAL_NODES"],
            "__TOTAL_PES__": cfg["TOTAL_PES"],
            "__THRDS__": cfg["THRDS"],
            "__NX__": cfg["NX"],
            "__NY__": cfg["NY"],
            "__AGCM_IM__": cfg["AGCM_IM"],
            "__AGCM_JM__": cfg["AGCM_JM"],
            "__AGCM_LM__": cfg["AGCM_LM"],
            "__IOSERVER_OPTS__": cfg["IOSERVER_OPTS"],
            "__DATE__": date_str,
            "__HOUR__": f"{cfg['FHOUR']}0000",
            "__FSEGMENT__": cfg["FSEGMENT"],
            "__NDAYS__": cfg["NDAYS"],
            "__NSTEPS_6HR__": cfg["NSTEPS_6HR"],
            "__FHRS__": cfg["FHRS"],
            "__FCST_LABL__": fcst_label,
            "__WALLTIME__": cfg["WALLTIME"],
            "__LOG_FILE__": f"{cfg['EXPDIR']}/forecasts/gcm_{fcst_type}_forecast_{fcst_label}.o{date_str}_{cfg['FHOUR']}z",
        }

        # Generate & Write Stats Batch Script
        rmap["__LOG_FILE__"] = f"{cfg['EXPDIR']}/forecasts/stats_{cfg['EXPID']}_ALL6_f{cfg['NDAYS']:02d}day.o{date_str}_{cfg['FHOUR']}z"
        stats_script = fcst_dir / f"stats_{cfg['EXPID']}_ALL6_f{cfg['NDAYS']:02d}day.j{date_str}_{cfg['FHOUR']}z"
        stats_script.write_text(render(CSH_STATS_BODY, rmap), encoding="utf-8")
        stats_script.chmod(0o755)

        print("=" * 70)
        print(f"Cycle Date: {date_str} {cfg['FHOUR']}z ({cfg['NDAYS']} days, 6-hourly verification cadence)")
        print(f"Partition : {cfg['PARTITION']} | Constraint: {cfg['CONSTRAINT']} | Account: {cfg['ACCOUNT']}")

        if stats_only:
            # Stats Only Mode: Submit directly without forecast dependency
            print(f"Mode      : STATS ONLY")
            print(f"Script    : {stats_script.name}")
            res_stats = subprocess.run(["sbatch", "--export=NONE", str(stats_script)], capture_output=True, text=True)
            if res_stats.returncode == 0:
                print(f"Submitted : {res_stats.stdout.strip()} [6 concurrent verification tasks on 1 node]")
            else:
                print(f"Submission Error: {res_stats.stderr.strip()}", file=sys.stderr)
        else:
            # Full Forecast + Stats Mode
            fcst_script = fcst_dir / f"gcm_{fcst_type}_forecast_{fcst_label}.j{date_str}_{cfg['FHOUR']}z"
            fcst_script.write_text(render(CSH_FCST_BODY, rmap), encoding="utf-8")
            fcst_script.chmod(0o755)

            if cfg["AVG_THROUGHPUT"]:
                print(f"Throughput: {cfg['AVG_THROUGHPUT']:.1f} days/day -> Estimated Walltime: {cfg['WALLTIME']}")
            print(f"Topology  : {cfg['TOTAL_NODES']} nodes | {cfg['TOTAL_PES']} MPI ranks (TPNODE={cfg['TPNODE']})")

            # Submit Forecast
            res_fcst = subprocess.run(["sbatch", "--export=NONE", str(fcst_script)], capture_output=True, text=True)
            if res_fcst.returncode != 0:
                print(f"Forecast Submission Failed: {res_fcst.stderr.strip()}", file=sys.stderr)
                curr_dt += timedelta(days=freq_days)
                continue

            fcst_out = res_fcst.stdout.strip()
            print(f"Forecast  : {fcst_out}")
            job_id = re.search(r"\b(\d+)\b", fcst_out).group(1)

            # Submit Stats Job with Dependency
            res_stats = subprocess.run(
                ["sbatch", "--export=NONE", f"--dependency=afterok:{job_id}", str(stats_script)],
                capture_output=True,
                text=True,
            )
            if res_stats.returncode == 0:
                print(f"Stats (6) : {res_stats.stdout.strip()} (afterok:{job_id})")
            else:
                print(f"Stats Submission Failed: {res_stats.stderr.strip()}", file=sys.stderr)

        curr_dt += timedelta(days=freq_days)

    print("=" * 70)


if __name__ == "__main__":
    main()

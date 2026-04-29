import pytest
from pathlib import Path
import os

from gcmpy.run_tools.cap_files_ops import *

nymdc = "20260408"
nhmsc = "210000"

cap_restart_text = f"{nymdc} {nhmsc}"

nymde = "29990302"
nhmse = "210000"
nymds = "00000015"
nhmss = "000000"

cap_rc_text = f"""
ROOT_NAME: GCM
ROOT_CF: AGCM.rc
HIST_CF: HISTORY.rc

BEG_DATE:     18910301 000000
END_DATE:     {nymde} {nhmse}
JOB_SGMT:     {nymds} {nhmss}
NUM_SGMT:     20
HEARTBEAT_DT:       1200

USE_SHMEM: 0
USE_EXTDATA2G: .TRUE.

# Parameters for Cycled REPLAY Forecasts
# --------------------------------------
 BEG_REPDATE: YYYYMMDD
 END_REPDATE: YYYYMMDD
FCST_SEGMENT: 00000000

#PERPETUAL_YEAR:   YYYY
#PERPETUAL_MONTH:    MM
#PERPETUAL_DAY:      DD

MAPL_ENABLE_TIMERS: YES
MAPL_ENABLE_MEMUTILS: NO
PRINTSPEC: 0  # (0: OFF, 1: IMPORT & EXPORT, 2: IMPORT, 3: EXPORT)
"""

def test_get_date_from_cap_rc():
    cap_rc_file = "CAP.rc"
    result = nymde, nhmse, nymds, nhmss
    Path(cap_rc_file).write_text(cap_rc_text)
    answer = get_date_from_cap_rc(cap_rc_file)
    os.remove(cap_rc_file)

    assert answer == result

def test_get_date_from_cap_restart():
    cap_restart_file = "cap_restart"
    result = nymdc, nhmsc
    Path(cap_restart_file).write_text(cap_restart_text)
    answer = get_date_from_cap_restart(cap_restart_file)
    os.remove(cap_restart_file)

    assert answer == result



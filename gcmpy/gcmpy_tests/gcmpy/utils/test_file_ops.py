import pytest
from pathlib import Path
import os

from gcmpy.utils.file_ops import *

cap_rc_text = """
ROOT_NAME: GCM
ROOT_CF: AGCM.rc
HIST_CF: HISTORY.rc

BEG_DATE:     18910301 000000
END_DATE:     29990302 210000
JOB_SGMT:     00000015 000000
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

@pytest.fixture
def create_cap_rc():
    # ------------------
    # SETUP PHASE
    # ------------------
    cap_rc_file = "CAP.rc"
    #Path(cap_rc_file).write_text(cap_rc_text)
    with open(cap_rc_file, "r") as fid:
        fid.write(cap_rc_text)
    print(f"\n[SETUP] Created {cap_rc_file}")

    # Pass the resource to the test
    yield cap_rc_file

    # ------------------
    # TEARDOWN PHASE
    # ------------------
    if Path(cap_rc_file).is_file():
        os.remove(cap_rc_file)
    print(f"\n[TEARDOWN] Removed {cap_rc_file}")


#@pytest.mark.parametrize("cap_rc_file, var_name, result", 
#                         [(create_cap_rc, "BEG_DATE", "18910301 000000"), 
#                          (create_cap_rc, "END_DATE: ", "29990302 210000"), 
#                          (create_cap_rc, "HEARTBEAT_DT:", "1200"), 
#                          (create_cap_rc, "USE_EXTDATA2G:", ".TRUE."),  
#                          (create_cap_rc, "PRINTSPEC:", "0")])
#def test_get_val_from_rc_file(cap_rc_file, var_name: str, result: str):
#
#    var_val = get_val_from_rc_file(cap_rc_file, var_name)
#
#    assert var_val == result

@pytest.mark.parametrize("var_name, result", 
                         [pytest.param("BEG_DATE", "18910301 000000", id="begdate"), 
                          pytest.param("END_DATE: ", "29990302 210000", id="enddate"), 
                          pytest.param("HEARTBEAT_DT:", "1200", id="dt"), 
                          pytest.param("USE_EXTDATA2G:", ".TRUE.", id="extdata"),  
                          pytest.param("PRINTSPEC:", "0", id="printspec")])
def test_get_val_from_rc_file(var_name: str, result: str):
    cap_rc_file = "CAP.rc" 
    Path(cap_rc_file).write_text(cap_rc_text)
    var_val = get_val_from_rc_file(cap_rc_file, var_name)
    os.remove(cap_rc_file) 
    assert var_val == result


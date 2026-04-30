import pytest
from pathlib import Path
import os

from gcmpy.batch_tools.node_ops import *

history_rc_text = f"""
COLLECTIONS: 'geosgcm_prog'
#             'prog.eta'
             'geosgcm_surf'
             'geosgcm_ocn'
             'geosgcm_moist'
             'geosgcm_turb'
#             'geosgcm_budi'
#             'geosgcm_parasol'
#             'geosgcm_modis'
             'geosgcm_buda'
#             'geosgcm_tend'
::
"""

@pytest.mark.parametrize("io_server, nx, ny, num_cpus, result",
                         [
                             (True, 6, 36, 120, (3, 1, 6)),
                             (False, 6, 36, 120, (2, 0, 0)),
                             (True, 24, 48, 68, (19, 2, 3)),
                             (False, 24, 48, 68, (17, 0, 0))
                             ]
                         )
def test_determine_nodes(io_server: int, nx: int, ny: int, 
                         num_cpus: int, result: tuple[int]) -> bool:
    history_rc_file = "HISTORY.rc"
    Path(history_rc_file).write_text(history_rc_text)
    answer = determine_nodes(io_server, nx, ny, num_cpus, history_rc_file)
    os.remove(history_rc_file)

    assert answer == result




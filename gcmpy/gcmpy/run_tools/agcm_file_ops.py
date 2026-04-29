
import sys
import datetime as dttm
from pathlib import Path

from gcmpy.utils.clock_ops import advance_clock
from gcmpy.utils.regex_ops import search_replace_in_file
from gcmpy.utils.regex_ops import extract_line_starting_with
from gcmpy.utils.regex_ops import grep_file
from gcmpy.utils.file_ops import get_val_from_rc_file

num_seconds_per_day = 86400

def get_use_waves(agcm_rc_file: Path | str) -> int:
    """
    Implement the C-shell command:

    set USE_WAVES = `grep '^\s*USE_WAVES:' AGCM.rc| cut -d: -f2`
    """
    search_string = "USE_WAVES:"
    USE_WAVES = get_val_from_rc_file(agcm_rc_file, search_string)

    return int(USE_WAVES)

def get_nx_ny(agcm_rc_file: Path | str) -> tuple[int]:
    """
    Implement the C-shell command:

       set NX = `grep '^\s*NX:' AGCM.rc | cut -d: -f2`
       set NY = `grep '^\s*NY:' AGCM.rc | cut -d: -f2`

    """
    NX = get_val_from_rc_file(agcm_rc_file, "NX:")
    NY = get_val_from_rc_file(agcm_rc_file, "NY:")

    return int(NX), int(NY)

def get_agcm_im_jm_lm(agcm_rc_file: Path | str) -> tuple[int]:
    """
    Implement the C-shell command:

       set AGCM_IM  = `grep '^\s*AGCM_IM:' AGCM.rc | cut -d: -f2`
       set AGCM_JM  = `grep '^\s*AGCM_JM:' AGCM.rc | cut -d: -f2`
       set AGCM_LM  = `grep '^\s*AGCM_LM:' AGCM.rc | cut -d: -f2`
    """
    AGCM_IM = get_val_from_rc_file(agcm_rc_file, "AGCM_IM:")
    AGCM_JM = get_val_from_rc_file(agcm_rc_file, "AGCM_JM:")
    AGCM_LM = get_val_from_rc_file(agcm_rc_file, "AGCM_JM:")

    return int(AGCM_IM), int(AGCM_JM), int(AGCM_LM)

def get_ogcm_im_jm(agcm_rc_file: Path | str) -> tuple[int]:
    """
    Implement the C-shell command:

       set OGCM_IM = `grep '^\s*OGCM\.IM_WORLD:' AGCM.rc | cut -d: -f2`
       set OGCM_JM = `grep '^\s*OGCM\.JM_WORLD:' AGCM.rc | cut -d: -f2`

    """
    OGCM_IM = get_val_from_rc_file(agcm_rc_file, r"OGCM\.IM_WORLD:")
    OGCM_JM = get_val_from_rc_file(agcm_rc_file, r"OGCM\.JM_WORLD:")

    return int(OGCM_IM), int(OGCM_JM)

def get_ioserver_params(agcm_rc_file: Path | str) -> tuple[int]:
    """
    Implement the C-shell command:

    set NUM_OSERVER_NODES = `grep '^\s*IOSERVER_NODES:' AGCM.rc | cut -d: -f2`
    set NUM_BACKEND_PES = `grep '^\s*NUM_BACKEND_PES:' AGCM.rc | cut -d: -f2`

    """
    NUM_OSERVER_NODES = get_val_from_rc_file(agcm_rc_file, r"IOSERVER_NODES:")
    NUM_BACKEND_PES = get_val_from_rc_file(agcm_rc_file, r"NUM_BACKEND_PES:")

    return int(NUM_OSERVER_NODES), int(NUM_BACKEND_PES)

def get_GIGATRAJ(agcm_rc_file: Path | str) -> str:
    """
    Implement the C-shell command:

    set GIGATRAJ = `grep '^\s*GIGATRAJ_PARCELS_FILE:' AGCM.rc | cut -d: -f2`

    """
    GIGATRAJ = get_val_from_rc_file(agcm_rc_file, r"GIGATRAJ_PARCELS_FILE:")

    return GIGATRAJ

def get_rst_files(agcm_rc_file: Path | str) -> list():
    """
    We implement the C-shell commands:

    set rst_files      = `grep "RESTART_FILE"    AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
    set rst_file_names = `grep "RESTART_FILE"    AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2`
    """
    search_string = "RESTART_FILE"
    answer_grep = grep_file(agcm_rc_file, search_string)
    rst_files_list = answer_grep.splitlines()
    rst_files_list = [line for line in rst_files_list if "VEGDYN" not in line and "#" not in line]

    rst_files = [line.strip().split(f"_{search_string}")[0] for line in rst_files_list]
    rst_file_names = [line.strip().split(f":")[1].strip() for line in rst_files_list]

    return rst_files, rst_file_names

def get_chk_files(agcm_rc_file: Path | str) -> list():
    """
    We implement the C-shell commands:

    set chk_files      = `grep "CHECKPOINT_FILE" AGCM.rc | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
    set chk_file_names = `grep "CHECKPOINT_FILE" AGCM.rc | grep -v "#" | cut -d ":" -f2`
    """
    search_string = "CHECKPOINT_FILE"
    answer_grep = grep_file(agcm_rc_file, search_string)
    chk_files_list = answer_grep.splitlines()
    chk_files_list = [line for line in chk_files_list if "#" not in line]

    chk_files = [line.strip().split(f"_{search_string}")[0] for line in chk_files_list]
    chk_file_names = [line.strip().split(f":")[1].strip() for line in chk_files_list]

    return chk_files, chk_file_names


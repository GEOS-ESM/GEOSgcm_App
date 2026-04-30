import os
from pathlib import Path
import math
from collections import Counter

def is_slurm_environment() -> bool:
    """
    Checks if running in a SLURM environment.

    Returns
    -------
    in_slurm : bool
        True if running under SLURM, False otherwise.
    """
    # SLURM_JOB_ID is always present when a job is allocated/running
    in_slurm = 'SLURM_JOB_ID' in os.environ

    return in_slurm

def get_all_slurm_env_vars() -> dict:
    """
    Get all environment variables that start with "SLURM".
    These are typically set by the SLURM job scheduler on 
    HPC systems.

    We iterate over all environment variables and filter
    out those that include the word "SLURM".  

    Returns
    -------
    slurm_env_vars : dict
        A dictionary where the keys are the names of the 
        SLURM environment variables and the values are their 
        corresponding values.
    """
    if is_slurm_environment():
        slurm_env_vars = {k: v for k, v in os.environ.items() if 'SLURM' in k}

    return slurm_env_vars

def compute_slurm_resources(nx: int, ny: int, env_dict: dict) -> dict:
    model_npes = nx * ny

    if "SLURM_NTASKS" in env_dict:
        ncpus = int(env_dict["SLURM_NTASKS"])
    else:
        pass

    # --- NCPUS_PER_NODE ---
    if "SLURM_NTASKS_PER_NODE" in env_dict:
        ncpus_per_node = int(env_dict["SLURM_NTASKS_PER_NODE"])
    elif "SLURM_CPUS_ON_NODE" in env_dict:
        ncpus_per_node = int(env_dict["SLURM_CPUS_ON_NODE"])
   
    num_model_nodes = math.ceil(model_npes / ncpus_per_node)

    cpu_dict = {
        "NCPUS": ncpus,
        "MODEL_NPES": model_npes,
        "NCPUS_PER_NODE": ncpus_per_node,
        "NUM_MODEL_NODES": num_model_nodes,
    }

    return cpu_dict

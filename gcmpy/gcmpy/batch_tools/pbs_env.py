import os
from pathlib import Path
import math
from collections import Counter

def is_pbs_environment() -> bool:
    """
    Checks if running in a PBS environment.

    Returns
    -------
    in_pbs : bool
        True if running under PBS, False otherwise.
    """
    # PBS_JOBID is always present when a job is allocated/running
    in_pbs = 'PBS_JOBID' in os.environ

    return in_pbs

def get_all_pbs_env_vars() -> dict:
    """
    Get all environment variables that start with "PBS".
    These are typically set by the PBS job scheduler on 
    HPC systems.

    We iterate over all environment variables and filter
    out those that include the word "PBS".  

    Returns
    -------
    pbs_env_vars : dict
        A dictionary where the keys are the names of the 
        PBS environment variables and the values are their 
        corresponding values.
    """
    if is_pbs_environment():
        pbs_env_vars = {k: v for k, v in os.environ.items() if 'PBS' in k}

    return pbs_env_vars


def compute_pbs_resources(nx: int, ny: int, env_dict: dict) -> dict:
    model_npes = nx * ny
            
    pbs_nodefile = Path(env_dict["PBS_NODEFILE"])
    ncpus = sum(1 for line in pbs_nodefile.open())    

    # Count occurrences of each node (like sort | uniq -c)
    with pbs_nodefile.open() as f:
        node_counts = Counter(line.strip() for line in f)

    # Take the first node's CPU count (assuming homogeneous cluster)
    ncpus_per_node = next(iter(node_counts.values()))

    num_model_nodes = math.ceil(model_npes / ncpus_per_node)

    cpu_dict = {
        "NCPUS": ncpus,
        "MODEL_NPES": model_npes,
        "NCPUS_PER_NODE": ncpus_per_node,
        "NUM_MODEL_NODES": num_model_nodes,
    }

    return cpu_dict
"""
Utility functions to extract SLURM and PBS 
environment variables.
"""
import os
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
    slurm_env_vars = {k: v for k, v in os.environ.items() if 'SLURM' in k}

    return slurm_env_vars

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
    pbs_env_vars = {k: v for k, v in os.environ.items() if 'PBS' in k}

    return pbs_env_vars

def get_pbs_total_cpus(pbs_nodefile: str) -> int:
    """
    Determine the total number of processors requested
    in a PBS job.

    Parameters
    ----------
    pbs_nodefile : str
        The file containing the list nodes used.
        Each node appears as many times there are CPUs in it.

    Returns
    -------
    total_cpus : int
        The total number of CPUs requested.
    """
    with open(pbs_nodefile, 'r') as fid:
        lines = fid.readlines()
        # Read all non-empty lines
        nodes = [line.strip() for line in lines if line.strip()]
        total_cpus = len(nodes)

    return total_cpus

def get_pbs_cpus_per_node(pbs_nodefile: str) -> int:
    """
    Determines the number of CPUs allocated per node 
    in a PBS environment.

    Parameters
    ----------
    pbs_nodefile : str
        The file containing the list nodes used.
        Each node appears as many times there are CPUs in it.

    Returns:
    cpus_per_node : int
        The number of CPUs per node.
    """
    with open(pbs_nodefile, 'r') as fid:
        lines = fid.readlines()
        # Read all non-empty lines
        nodes = [line.strip() for line in lines if line.strip()]

    # Count how many times each node appears
    node_counts = Counter(nodes)

    # Assuming homogeneous nodes, 
    # return the CPU count for the first node in the list
    first_node = nodes[0]
    return node_counts[first_node]

if __name__ == "__main__":
    slurm_env_vars = get_all_slurm_env_vars()
    for key, value in slurm_env_vars.items():
        print(f"{key:<30} = {value}")




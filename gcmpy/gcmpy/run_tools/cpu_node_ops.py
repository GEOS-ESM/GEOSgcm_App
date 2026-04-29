"""
"""

from gcmpy.utils.slurm_ops import (
        is_pbs_environment,
        get_all_pbs_env_vars,
        is_slurm_environment,
        get_all_slurm_env_vars,
        get_total_pbs_cpus
        )

def check_cpu_ressources(NX: int, NY: int):
    """
    """
    MODEL_PES = NX * NY

    if is_slurm_environment():
        slurm_env_vars = get_all_slurm_env_vars()
        NCPUS = slurm_env_vars["SLURM_NTASKS"]
        if "SLURM_NTASKS_PER_NODE" in slurm_env_vars:
            NCPUS_PER_NODE = slurm_env_vars["SLURM_NTASKS_PER_NODE"]
        else:
            NCPUS_PER_NODE = slurm_env_vars["SLURM_CPUS_ON_NODE"]
    elif is_pbs_environment():
        pbs_env_vars = get_all_pbs_env_vars()
        NCPUS = get_pbs_total_cpus(pbs_env_vars["PBS_NODEFILE"])
        NCPUS_PER_NODE =
    else:
        NCPUS = None
        

import slurm_env
import pbs_env
import interactive_env

def load_env_vars() -> dict:
    """
    Loads the correct enivornment varibles into a dictionary
    """
    env_dict = {}
    if slurm_env.is_slurm_environment():
        env_dict = slurm_env.get_all_slurm_env_vars()
    elif pbs_env.is_pbs_environment():
        env_dict = pbs_env.get_all_pbs_env_vars()

    return env_dict

def get_batch_resources(nx: int, ny: int, env_dict: dict) -> dict:
    cpu_dict = {}
    if slurm_env.is_slurm_environment():
        cpu_dict = slurm_env.compute_slurm_resources(nx, ny, env_dict)
    elif pbs_env.is_pbs_environment():
        cpu_dict = pbs_env.compute_pbs_resources(nx, ny, env_dict)
    else:
        cpu_dict = interactive_env.compute_interactive_resources(nx, ny)

    return cpu_dict
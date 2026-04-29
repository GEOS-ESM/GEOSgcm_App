"""
"""
import os
from pathlib import Path

from gcmpy.utils.path_ops import (
        clean_dir,
        create_dir,
        check_dir_exists,
        create_symbolic_link
        )

def create_scratchdir(exp_dir: str, 
                      scratch_dir_name: str) -> str:
    """
    Create the scratch directory.
    Dependending on the environment variable settings,
    construct the full path to the scratch directory.
    If the directory already exists, delete it and
    create a new one.

    Parameters
    ----------
    exp_dir : str
       The experiment directory where the run is carried out.
    scratch_dir_name : str
       The last portion (last child directory) of the
       scratch directory.

    Returns
    -------
    scratch_dir : str
       The full path of the scratch directory.
    """
    all_env_vars = os.environ
    dir_name = f"{exp_dir}/{scratch_dir_name}"

    # The "TSE_TMPDIR" environment variable only exists 
    # at NCCS so first we check if it is defined.
    if "TSE_TMPDIR" in all_env_vars: 
        # We might not want to always use TSE_TMPDIR 
        # as the scratch if we need a permanent scractch
        # directory for debugging or other # purposes. 
        # So we can set a flag use_tse_tmpdir to True 
        # if we want and we default to True.
        use_tse_tmpdir = True

        # If we want to use TSE_TMPDIR as the scratch, 
        # we create a scratch directory under 
        # TSE_TMPDIR and link it to scratch_dir
        if use_tse_tmpdir:
            # Finally, we should be careful as there is 
            # a possibility we could collide if two runs 
            # use the same TSE_TMPDIR (for example a packable job). 
            # So we use $SLURM_JOB_ID and, if defined, the
            # SLURM_ARRAY_TASK_ID environment variable 
            # to create a unique scratch directory.
            tse_tmpdir_name = all_env_vars["SLURM_JOB_ID"]
            if "SLURM_ARRAY_TASK_ID" in all_env_vars:
                tse_tmpdir_name = f"{tse_tmpdir_name}_{all_env_vars['SLURM_ARRAY_TASK_ID']}"

            scratch_dir = f"{all_env_vars['TSE_TMPDIR']}/{tse_tmpdir_name}/{scratch_dir_name}"

            if check_dir_exists(dir_name) or Path(dir_name).is_symlink():
                clean_dir(dir_name)

            if check_dir_exists(f"{scratch_dir}"):
                clean_dir(f"{scratch_dir}")
            create_dir(f"{scratch_dir}")

            create_symbolic_link(link_location=f"{exp_dir}/{scratch_dir_name}",
                                 target_location=f"{scratch_dir}")
        else:
            # If use_tse_tmpdir is False, we create a
            # scratch directory as we did before under
            # the experiment directory.
            scratch_dir = dir_name
            if check_dir_exists(f"{scratch_dir}"):
                clean_dir(f"{scratch_dir}")
            create_dir(f"{scratch_dir}")
    else:
        # If TSE_TMPDIR is not defined, we are not
        # at NCCS and we have to act as we did before
        # and create a scratch directory under the
        # experiment directory
        scratch_dir = dir_name
        if check_dir_exists(f"{scratch_dir}"):
            clean_dir(f"{scratch_dir}")
        create_dir(f"{scratch_dir}")

    return scratch_dir


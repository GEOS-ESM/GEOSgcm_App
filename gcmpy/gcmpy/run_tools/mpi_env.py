"""
mpi_env.py

Applies literal csh `setenv VAR VALUE` text blocks (as computed once at
experiment-setup time by SetupGCM.mpistacksettings() -- run_config
['mpi_config'] and run_config['ocean']['mpt_shepherd']) to the current
process's environment, since gcm_run.sh/gcm_run.py no longer render or
execute gcm_run.j as a csh script.

Replaces lines 955-959 of gcm_run.j.
"""

import os


def apply_setenv_block(text: str) -> None:
    """
    Parse a multi-line block of csh `setenv VAR VALUE` statements
    (comments and blank lines ignored) and apply each as an environment
    variable in the current process.

    Args:
        text: The literal csh setenv block text (may be empty/None).
    """
    if not text:
        return

    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        if not line.startswith("setenv "):
            continue

        parts = line[len("setenv "):].split(None, 1)
        if len(parts) != 2:
            continue

        var_name, value = parts
        os.environ[var_name] = value.strip()

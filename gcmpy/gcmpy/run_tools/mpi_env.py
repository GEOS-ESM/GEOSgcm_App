"""
mpi_env.py

Applies structured MPI environment settings to the current process.
Legacy run_config['mpi_config'] and ocean['mpt_shepherd'] csh-style blocks
remain supported for existing experiments, without executing a shell.

Replaces lines 955-959 of gcm_run.j.
"""

import os
import re
from collections.abc import Mapping


def apply_mpi_environment(run_config: dict) -> None:
    """Apply legacy blocks first, then mpi_environment overrides.

    Mapping values must be strings (including empty strings) or None to
    remove a variable. Validate the entire mapping before changing anything.
    Values are literal, without shell expansion or quote processing.
    """
    settings = run_config.get("mpi_environment", {})
    if not isinstance(settings, Mapping):
        raise ValueError("mpi_environment must be a mapping of names to strings or null")
    for name, value in settings.items():
        if not isinstance(name, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name):
            raise ValueError(f"Invalid mpi_environment variable name: {name!r}")
        if value is not None and (not isinstance(value, str) or "\0" in value):
            raise ValueError(
                f"mpi_environment[{name!r}] must be a string without NUL characters "
                "or null; quote numeric and boolean-like values"
            )

    apply_setenv_block(run_config.get("mpi_config"))
    apply_setenv_block(run_config.get("ocean", {}).get("mpt_shepherd"))
    for name, value in settings.items():
        if value is None:
            os.environ.pop(name, None)
        else:
            os.environ[name] = value


def apply_setenv_block(text: str) -> None:
    """
    Parse a multi-line block of csh environment statements
    (comments and blank lines ignored) and apply each as an environment
    variable in the current process. `setenv VAR` sets an empty value;
    `unsetenv VAR` removes a literal variable name, even if it is absent.
    Statements are applied in order. This is not a shell interpreter:
    values retain the existing literal handling (no expansion or unquoting),
    and unsetenv patterns are not expanded.

    Args:
        text: The literal csh setenv block text (may be empty/None).
    """
    if not text:
        return

    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        statement = line.split(None, 1)
        if len(statement) != 2:
            continue
        command, arguments = statement
        parts = arguments.split(None, 1)
        if command == "unsetenv":
            if len(parts) == 1:
                os.environ.pop(parts[0], None)
            continue
        if command != "setenv":
            continue

        var_name = parts[0]
        value = parts[1] if len(parts) == 2 else ""
        os.environ[var_name] = value.strip()

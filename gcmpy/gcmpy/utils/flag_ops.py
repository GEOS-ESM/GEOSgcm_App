"""
flag_ops.py

Helper for interpreting the Jinja "line-prefix" flag strings used
throughout the setup_tools config dicts (ocean/atmos/land/gocart), e.g.
ocean['MOM6'], ocean['CICE6'], ocean['coupled'], singularity_build,
native_build.

These values are not booleans: they are literal strings meant to be
prepended to a template line so that, after Jinja rendering, the line is
either left active ('') or turned into a comment ('#DELETE'-style),
causing it to later be stripped out by SetupGCM.cleanup(). Since
run_<expid>.yaml is written *before* Jinja rendering, these come through
as raw strings when read back in gcm_run.py / run_tools, so callers need
to interpret them explicitly rather than relying on Python truthiness.
"""


def is_enabled(flag_value) -> bool:
    """
    Interpret a Jinja line-prefix flag value.

    Parameters
    ----------
    flag_value : str
        The raw flag value (e.g. ocean['MOM6']). '' means the
        corresponding template line stays active (True); any other value
        (e.g. '#DELETE') means the line is commented out/skipped (False).

    Returns
    -------
    bool
        True if the flag is "active" (empty string), False otherwise.
    """
    return flag_value == ""

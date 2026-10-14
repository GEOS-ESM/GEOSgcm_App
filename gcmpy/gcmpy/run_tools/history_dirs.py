"""
history_dirs.py

Creates the experiment's base subdirectories and the per-HISTORY-collection
output/holding directories.

Replaces lines 65-71 and 355-384 of gcm_run.j.
"""

from pathlib import Path

from gcmpy.run_tools.history_file_ops import read_history_rc, get_collection_list


def ensure_exp_base_dirs(exp_dir: Path | str) -> None:
    """
    Create the experiment's base subdirectories, if they don't already exist.

    Replaces:
        if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
        if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
        if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
        if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
        if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot

    Args:
        exp_dir: Experiment directory.
    """
    exp_dir = Path(exp_dir)
    for sub_dir in ("restarts", "holding", "archive", "post", "plot"):
        (exp_dir / sub_dir).mkdir(parents=True, exist_ok=True)


def create_history_collection_dirs(exp_dir: Path | str, history_rc_path: Path | str) -> list:
    """
    Top-level entry point: create per-collection output and holding
    directories based on the HISTORY.rc COLLECTIONS list.

    Replaces:
        set collections = ''
        foreach line ("`cat HISTORY.rc`")
           ...
        end
        done:
           foreach collection ( $collections )
              if (! -e $EXPDIR/$collection )         mkdir $EXPDIR/$collection
              if (! -e $EXPDIR/holding/$collection ) mkdir $EXPDIR/holding/$collection
           end

    Args:
        exp_dir:         Experiment directory.
        history_rc_path: Path to the experiment's HISTORY.rc.

    Returns:
        List of collection name strings (needed later for the HISTORY-to-
        holding move at the end of each segment).
    """
    exp_dir = Path(exp_dir)

    history_dict = read_history_rc(str(history_rc_path))
    collections = get_collection_list(history_dict)

    for collection in collections:
        (exp_dir / collection).mkdir(exist_ok=True)
        (exp_dir / "holding" / collection).mkdir(exist_ok=True)

    return collections

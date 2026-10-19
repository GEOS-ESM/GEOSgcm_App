"""
run_resources.py

Resolves per-job experiment run parameters (NX/NY/AGCM grid dims, IO
server node counts, and CPU/node resource allocation), and checks for
over-specification of CPU resources against the actual batch allocation.

Replaces lines 178-273 of gcm_run.j.
"""

import math
import os
from pathlib import Path

from gcmpy.run_tools.agcm_file_ops import (
    get_nx_ny,
    get_agcm_im_jm_lm,
    get_ogcm_im_jm,
    get_ogcm_coupled_overrides,
    get_ioserver_params,
)
from gcmpy.utils.flag_ops import is_enabled


class ResourceOverspecifiedError(RuntimeError):
    """Raised when the requested CPU/node resources exceed the batch allocation."""


def _resolve_ncpus_per_node(run_config: dict) -> int:
    """
    Determine the number of CPUs per node.

    Replaces:
        if ($?SLURM_NTASKS_PER_NODE) then
           set NCPUS_PER_NODE = $SLURM_NTASKS_PER_NODE
        else if ($?SLURM_CPUS_ON_NODE) then
           set NCPUS_PER_NODE = $SLURM_CPUS_ON_NODE
        else if ($?PBS_NODEFILE) then
           set NCPUS_PER_NODE = `sort $PBS_NODEFILE | uniq -c | head -1 | awk '{print $1}'`
        else
           set NCPUS_PER_NODE = {{ num_CPUs }}
        endif
    """
    if "SLURM_NTASKS_PER_NODE" in os.environ:
        return int(os.environ["SLURM_NTASKS_PER_NODE"])
    if "SLURM_CPUS_ON_NODE" in os.environ:
        return int(os.environ["SLURM_CPUS_ON_NODE"])
    if "PBS_NODEFILE" in os.environ:
        from collections import Counter
        with open(os.environ["PBS_NODEFILE"]) as f:
            node_counts = Counter(line.strip() for line in f)
        return next(iter(node_counts.values()))

    # Desktop/laptop default, from gcm_setup.
    return int(run_config["num_CPUs"])


def _resolve_ncpus() -> int | None:
    """
    Determine the total number of allotted CPUs, or None ("NULL") if
    running outside a batch environment (desktop path).

    Replaces:
        if ($?SLURM_NTASKS) then
           set  NCPUS = $SLURM_NTASKS
        else if ($?PBS_NODEFILE) then
           set  NCPUS = `cat $PBS_NODEFILE | wc -l`
        else
           set  NCPUS = NULL
        endif
    """
    if "SLURM_NTASKS" in os.environ:
        return int(os.environ["SLURM_NTASKS"])
    if "PBS_NODEFILE" in os.environ:
        with open(os.environ["PBS_NODEFILE"]) as f:
            return sum(1 for _ in f)

    return None


def resolve_run_resources(agcm_rc_file: Path | str, run_config: dict) -> dict:
    """
    Top-level entry point: resolve experiment run parameters and check for
    over-specification of CPU resources.

    Args:
        agcm_rc_file: Path to the experiment's AGCM.rc.
        run_config:   The experiment's run_<expid>.yaml dictionary.

    Returns:
        dict with keys: NX, NY, AGCM_IM, AGCM_JM, AGCM_LM, OGCM_IM, OGCM_JM,
        USE_IOSERVER, NUM_OSERVER_NODES, NUM_BACKEND_PES, NCPUS,
        NCPUS_PER_NODE, NUM_MODEL_NODES, TOTAL_PES.

    Raises:
        ResourceOverspecifiedError: If the requested resources exceed the
        allotted CPUs (mirrors `exit 1` in gcm_run.j).
    """
    agcm_rc_file = Path(agcm_rc_file)

    nx, ny = get_nx_ny(agcm_rc_file)
    agcm_im, agcm_jm, agcm_lm = get_agcm_im_jm_lm(agcm_rc_file)
    ogcm_im, ogcm_jm = get_ogcm_im_jm(agcm_rc_file)

    if is_enabled(run_config["ocean"]["coupled"]):
        nx, ny, _ogcm_lm = get_ogcm_coupled_overrides(agcm_rc_file)

    use_ioserver = int(run_config["expConfig"]["io_server"])
    num_oserver_nodes, num_backend_pes = get_ioserver_params(agcm_rc_file)

    model_npes = nx * ny
    ncpus = _resolve_ncpus()
    ncpus_per_node = _resolve_ncpus_per_node(run_config)
    num_model_nodes = math.ceil(model_npes / ncpus_per_node)

    if ncpus is not None:
        if use_ioserver == 1:
            total_nodes = num_model_nodes + num_oserver_nodes
            total_pes = total_nodes * ncpus_per_node

            if total_pes > ncpus:
                raise ResourceOverspecifiedError(
                    "CPU Resources are Over-Specified\n"
                    "--------------------------------\n"
                    f"Allotted  NCPUs: {ncpus}\n"
                    f"Requested NCPUs: {total_pes}\n\n"
                    f"Specified NX: {nx}\n"
                    f"Specified NY: {ny}\n\n"
                    f"Specified model nodes: {num_model_nodes}\n"
                    f"Specified oserver nodes: {num_oserver_nodes}\n"
                    f"Specified cores per node: {ncpus_per_node}"
                )
        else:
            total_pes = model_npes

            if total_pes > ncpus:
                raise ResourceOverspecifiedError(
                    "CPU Resources are Over-Specified\n"
                    "--------------------------------\n"
                    f"Allotted  NCPUs: {ncpus}\n"
                    f"Requested NCPUs: {total_pes}\n\n"
                    f"Specified NX: {nx}\n"
                    f"Specified NY: {ny}\n\n"
                    f"Specified model nodes: {num_model_nodes}\n"
                    f"Specified cores per node: {ncpus_per_node}"
                )
    else:
        # Desktop path -- no batch allocation to check against.
        total_pes = model_npes

    return {
        "NX": nx,
        "NY": ny,
        "AGCM_IM": agcm_im,
        "AGCM_JM": agcm_jm,
        "AGCM_LM": agcm_lm,
        "OGCM_IM": ogcm_im,
        "OGCM_JM": ogcm_jm,
        "USE_IOSERVER": use_ioserver,
        "NUM_OSERVER_NODES": num_oserver_nodes,
        "NUM_BACKEND_PES": num_backend_pes,
        "NCPUS": ncpus,
        "NCPUS_PER_NODE": ncpus_per_node,
        "NUM_MODEL_NODES": num_model_nodes,
        "TOTAL_PES": total_pes,
    }

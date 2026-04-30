def compute_interactive_resources(nx: int, ny: int) -> dict:
    #ncpus_per_node = @NCPUS_PER_NODE
    ncpus_per_node = 120

    cpu_dict = {
        "NCPUS": None,
        "MODEL_NPES": nx * ny,
        "NCPUS_PER_NODE": ncpus_per_node,
        "NUM_MODEL_NODES": None,
    }

    return cpu_dict
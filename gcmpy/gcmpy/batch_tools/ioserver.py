def calculate_total_pes(ncpus, nx, ny, num_model_nodes, ncpus_per_node, num_oserver_nodes):
	#if @USE_IOSERVER == 1:
	if 0 == 1:
		total_nodes = num_model_nodes + num_oserver_nodes
    	total_pes = total_nodes * ncpus_per_node
    else:
    	total_pes = nx * ny
    if total_pes > ncpus:
    	fail(ncpus, total_pes, nx, ny, num_model_nodes, ncpus_per_node, num_oserver_nodes)

    return total_pes

def fail(ncpus, total_pes, nx, ny, num_model_nodes, ncpus_per_node, num_oserver_nodes):
    print("CPU Resources are Over-Specified")
    print("--------------------------------")
    print(f"Allotted  NCPUs: {ncpus}")
    print(f"Requested NCPUs: {total_pes}")
    print()
    print(f"Specified NX: {nx}")
    print(f"Specified NY: {ny}")
    print()

    print(f"Specified model nodes: {num_model_nodes}")
    if num_oserver_nodes is not None:
        print(f"Specified oserver nodes: {num_oserver_nodes}")

    print(f"Specified cores per node: {ncpus_per_node}")

    sys.exit(1)

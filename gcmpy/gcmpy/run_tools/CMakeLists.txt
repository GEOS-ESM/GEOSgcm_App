
set (files
	__init__.py
	cap_files_ops.py
	cpu_node_ops.py
	agcm_file_ops.py  
	history_file_ops.py
        restart_file_ops.py
        scratchdir_ops.py
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/run_tools
   )

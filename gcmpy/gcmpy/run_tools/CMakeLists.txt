
set (files
	__init__.py
	cap_files_ops.py
	agcm_file_ops.py  
	history_file_ops.py
        restart_file_ops.py
        dir_creation_ops.py
	rc_parser.py
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/run_tools
   )

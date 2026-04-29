
set (files
	__init__.py
	clock_ops.py  
	color_ops.py  
	file_ops.py
	logger_ops.py  
	path_ops.py 
	regex_ops.py
	run_cmd_ops.py  
	slurm_ops.py
	strip_name.py
	yaml_ops.py
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/utils
   )


set (files
	__init__.py
	computing_sites.py  
	node_ops.py
	slurm_ops.py
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/batch_tools
   )

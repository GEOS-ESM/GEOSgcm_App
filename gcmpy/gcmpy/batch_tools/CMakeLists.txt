add_subdirectory(templates)

set (files
	__init__.py
	computing_sites.py  
	node_ops.py
        ioserver.py         
	batch_environ.py    
	slurm_env.py
        interactive_env.py  
	pbs_env.py        
	site_nodes.yaml
	pbs_catalog.yaml  
	slurm_catalog.yaml
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/batch_tools
   )

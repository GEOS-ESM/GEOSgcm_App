
set (files
        __init__.py  
	read_batch_tmpl.py
	gcm_run_pbs.j.tmpl
      	gcm_run_slurm.j.tmpl
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/batch_tools/templates
   )

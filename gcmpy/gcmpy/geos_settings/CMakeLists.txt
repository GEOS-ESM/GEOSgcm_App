
set (files
	__init__.py
	computing_sites.py  
	node_ops.py
	horizontal_resolution.py
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/geos_settings
   )

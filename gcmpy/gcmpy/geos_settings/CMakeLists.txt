
set (files
	__init__.py
	horizontal_resolution.py
	hres_catalog.yaml
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/geos_settings
   )

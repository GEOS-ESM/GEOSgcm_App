set (programs
	atmosphere_catalog.yaml
	ocean_catalog.yaml
	land_catalog.yaml
	gocart_catalog.yaml
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy/setup_tools/catalogs)
endforeach ()

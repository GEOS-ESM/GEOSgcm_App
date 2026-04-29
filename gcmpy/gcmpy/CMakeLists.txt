add_subdirectory(utils)
add_subdirectory(geos_settings)
add_subdirectory(run_tools)
add_subdirectory(setup_tools)

set (programs
	__init__.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy)
endforeach ()

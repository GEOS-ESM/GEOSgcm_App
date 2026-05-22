add_subdirectory(gcmpy_doc)
add_subdirectory(gcmpy_tests)
add_subdirectory(gcmpy)

set (programs
	run_gcm_setup.py
	run_gcm_run.py
	README.md
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()

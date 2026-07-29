add_subdirectory(gcmpy_doc)
add_subdirectory(gcmpy_tests)
add_subdirectory(gcmpy)

set (programs
	gcm_configure.py
	gcm_create.py
	gcm_setup.py
	README.md
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()

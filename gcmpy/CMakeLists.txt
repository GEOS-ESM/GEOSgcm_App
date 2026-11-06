add_subdirectory(gcmpy_doc)
add_subdirectory(gcmpy_tests)
add_subdirectory(gcmpy)

# Keep the Python runner's shell wrapper with its source, but install it
# alongside g5_modules.sh so runtime and scheduler paths remain unchanged.
install(PROGRAMS gcm_run.sh DESTINATION bin)

set (programs
	gcm_configure.py
	gcm_create.py
	gcm_setup.py
	gcm_run.py
	README.md
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()

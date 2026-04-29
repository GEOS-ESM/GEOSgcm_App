
set (programs
	__init__.py
	test_cap_files_ops.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy_tests/gcmpy/run_tools)
endforeach ()

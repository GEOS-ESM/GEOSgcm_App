
set (programs
	__init__.py
	test_path_ops.py  
	test_regex_ops.py  
	test_run_cmd_ops.py  
	test_clock_ops.py
	test_text_file_ops.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy_tests/gcmpy/utils)
endforeach ()


set (programs
	__init__.py
	test_path_ops.py  
	test_regex_ops.py  
	test_run_cmd_ops.py  
	test_clock_ops.py
	test_file_ops.py
	)

install (
	FILES ${programs}
	DESTINATION bin/gcmpy/gcmpy_tests/gcmpy/utils
	)

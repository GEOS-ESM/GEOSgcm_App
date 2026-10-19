
set (programs
	__init__.py
	test_archiver.py
	test_cap_files_ops.py
	test_executable_setup.py
	test_extdata.py
	test_gcm_run_sh_env.py
	test_gcm_run_submit.py
	test_history_file_ops.py
	test_mpi_runner.py
	test_post_run.py
	test_pre_run.py
	test_stage_restarts.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy_tests/gcmpy/run_tools)
endforeach ()


set (programs
	__init__.py
	test_atmosphere_config.py
	test_bcs_staging.py
	test_catalog_tokens.py
	test_config_heartbeat.py
	test_config_key_coverage.py
	test_csh_template_aliases.py
	test_ocean_config.py
	test_run_code_staging.py
	test_template_vars.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy_tests/gcmpy/setup_tools)
endforeach ()

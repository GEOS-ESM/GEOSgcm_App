
set (programs
	__init__.py
	test_node_ops.py
	test_site_nodes.py
	)

install (
        FILES ${programs}
        DESTINATION bin/gcmpy/gcmpy_tests/gcmpy/batch_tools
        )

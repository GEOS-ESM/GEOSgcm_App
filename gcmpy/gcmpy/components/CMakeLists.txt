set (programs
	__init__.py
	base.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy/components)
endforeach ()

add_subdirectory(GOCART)

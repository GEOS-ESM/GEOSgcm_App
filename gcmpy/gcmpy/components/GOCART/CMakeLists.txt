set (programs
	__init__.py
	component.yaml
	component.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy/components/GOCART)
endforeach ()

# questions.yaml and catalog.yaml are plain data: no @ONLY substitution.
install (
   FILES questions.yaml catalog.yaml
   DESTINATION bin/gcmpy/gcmpy/components/GOCART
   )

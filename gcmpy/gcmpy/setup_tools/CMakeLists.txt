add_subdirectory(questions)

set (programs
	__init__.py
	atmosphere.py  
	clone.py  
	generate_question.py  
	gocart.py  
	land.py  
	ocean.py  
	process_questions.py  
	geos_ops.py
	setup_envs.py  
	setup_main.py
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy/setup_tools)
endforeach ()

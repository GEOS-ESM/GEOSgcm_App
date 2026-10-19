set (programs
	__init__.py
	load_questions.py
	atmospheric_model.yaml  
	exp_setup.yaml
	ocean_model.yaml
        directory_setup.yaml    
	gocart.yaml     
	land_model.yaml
	)

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy/setup_tools/questions)
endforeach ()

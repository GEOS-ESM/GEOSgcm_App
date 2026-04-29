
set (files
	__init__.py
	load_questions.py  
	atmospheric_model.yaml  
	exp_setup.yaml
	ocean_model.yaml
        directory_setup.yaml    
	gocart.yaml     
	land_model.yaml  
	mpi_config.yaml
	)

install (
   FILES ${files}
   DESTINATION bin/gcmpy/gcmpy/setup_tools/config
   )

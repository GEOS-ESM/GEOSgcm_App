set (programs
  atmospheric_model.yaml
  directory_setup.yaml
  exp_setup.yaml
  gocart.yaml
  land_model.yaml
  mpi_config.yaml
  ocean_model.yaml
  )

install (
   PROGRAMS ${programs}
   DESTINATION bin/gcmpy/yaml
   )

# processed files
set (setup_scripts)

foreach (file ${setup_scripts})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()

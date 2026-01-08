set (files
  atmospheric_model.yaml
  directory_setup.yaml
  exp_setup.yaml
  gocart.yaml
  land_model.yaml
  mpi_config.yaml
  ocean_model.yaml
  )

install (
   FILES ${files}
   DESTINATION bin/gcmpy/yaml
   )



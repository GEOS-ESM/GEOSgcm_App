# cp (makes exe)
set (programs
   construct_extdata_yaml_list.py
   )

install (
   PROGRAMS ${programs}
   DESTINATION bin/gcmpy
   )

# processed files
set (setup_scripts
   gcm_setup
   gmichem_setup
   geoschemchem_setup
   stratchem_setup
   )

foreach (file ${setup_scripts})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()



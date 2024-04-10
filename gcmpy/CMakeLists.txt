# cp (makes exe)
add_subdirectory(scripts)
add_subdirectory(yaml)

set (programs)

install (
   PROGRAMS ${programs}
   DESTINATION bin/gcmpy
   )

# processed files
set (setup_scripts)

foreach (file ${setup_scripts})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy)
endforeach ()



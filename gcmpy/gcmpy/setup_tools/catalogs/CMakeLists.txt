set (programs
	atmosphere_catalog.yaml
	ocean_catalog.yaml
	land_catalog.yaml
	gocart_catalog.yaml
	)

# Values for the configure_file() tokens in the catalogs.  This mirrors
# the platform split in setup_envs.py (envdict preload_command) and
# gcm_setup (PRELOAD_COMMAND): without these definitions configure_file
# substitutes the empty string and the coupled ocean preload collapses.
if (APPLE)
   set (PRELOAD_COMMAND "DYLD_INSERT_LIBRARIES")
else ()
   set (PRELOAD_COMMAND "LD_PRELOAD")
endif ()

foreach (file ${programs})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin/gcmpy/gcmpy/setup_tools/catalogs)
endforeach ()

ecbuild_add_executable (
  TARGET GEOSgcm.x
  SOURCES GEOSgcm.F90
  LIBS GEOSgcm_GridComp ESMF::ESMF OpenMP::OpenMP_Fortran
  )
# NOTE Due to Gigatraj being a C++ code, CMake apparently
# gets confused if building with it and thinks GEOSgcm.x
# is a C++ code and tries to link as C++. This will fail.
# To fix this, we explicitly set the linker language to Fortran.
set_target_properties (GEOSgcm.x PROPERTIES LINKER_LANGUAGE Fortran)

ecbuild_add_executable (
  TARGET idfupd.x
  SOURCES idfupd.F90
  LIBS GEOSgcm_GridComp ESMF::ESMF
  )
set_target_properties (idfupd.x PROPERTIES LINKER_LANGUAGE Fortran)

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

file (GLOB templates CONFIGURE_DEPENDS *.tmpl)

# Now that we will use configure_file on AGCM.rc.tmpl, we
# need to remove it from the list of templates, since
# we'll need to install the configured file instead.
list (REMOVE_ITEM templates AGCM.rc.tmpl)

# Next, we want to define a variable based on whether or not
# BUILD_WITH_GIGATRAJ is set to ON. If it is on, we want
# the variable to be empty, otherwise we want it to be #
# so that the line in AGCM.rc.tmpl is commented out.
if (BUILD_WITH_GIGATRAJ)
  set (GIGATRAJ_ENABLED "")
else ()
  set (GIGATRAJ_ENABLED "#")
endif ()
configure_file(AGCM.rc.tmpl AGCM.rc.tmpl @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/AGCM.rc.tmpl DESTINATION etc)

set (programs
   gcm_run.j
   gcm_regress.j
   gcm_post.j
   gcm_archive.j
   gcm_quickplot.csh
   gcm_moveplot.j
   gcm_forecast.setup
   gcm_emip.setup
   scm_setup
   gcm_quickstat.j
   scm_run.j
   construct_extdata_yaml_list.py
   )

install (
   FILES ${templates}
   DESTINATION etc
   )

install (
   PROGRAMS ${programs}
   DESTINATION bin
   )

install (
   FILES fvcore_layout.rc saverst.rc logging.yaml
   DESTINATION etc
   )

if(INSTALL_SOURCE_TARFILE)
  set(CFG_INSTALL_SOURCE_TARFILE TRUE)
else()
  set(CFG_INSTALL_SOURCE_TARFILE FALSE)
endif()

set (setup_scripts
   gcm_setup
   gmichem_setup
   geoschemchem_setup
   stratchem_setup
   )

foreach (file ${setup_scripts})
   configure_file(${file} ${file} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${file} DESTINATION bin)
endforeach ()

configure_file(.AGCM_VERSION .AGCM_VERSION @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/.AGCM_VERSION DESTINATION etc)


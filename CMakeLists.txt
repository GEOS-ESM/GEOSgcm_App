ecbuild_add_executable (
  TARGET GEOSgcm.x
  SOURCES GEOSgcm.F90
  LIBS GEOSgcm_GridComp esmf OpenMP::OpenMP_Fortran
  )

ecbuild_add_executable (
  TARGET idfupd.x
  SOURCES idfupd.F90
  LIBS GEOSgcm_GridComp esmf
  )

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

file (GLOB templates CONFIGURE_DEPENDS *.tmpl)

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


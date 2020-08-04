ecbuild_add_executable (
  TARGET GEOSgcm.x
  SOURCES GEOSgcm.F90
  LIBS GEOSgcm_GridComp 
  )

ecbuild_add_executable (
  TARGET idfupd.x
  SOURCES idfupd.F90
  LIBS GEOSgcm_GridComp 
  )

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

#link_directories (${MKL_LIBRARIES})
target_link_libraries (GEOSgcm.x ${OpenMP_Fortran_LIBRARIES})
set_target_properties(GEOSgcm.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_include_directories (GEOSgcm.x PUBLIC ${INC_ESMF})

file (GLOB templates *.tmpl)

set (programs
   gcm_setup
   gcm_run.j
   gcm_regress.j
   gcm_post.j
   gcm_archive.j
   gcm_convert.j
   gcm_quickplot.csh
   gcm_moveplot.j
   gcm_forecast.setup
   gcm_emip.setup
   scm_setup
   gmichem_setup
   geoschemchem_setup
   gcm_quickstat.j
   scm_run.j
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

configure_file(stratchem_setup stratchem_setup @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/stratchem_setup DESTINATION bin)

configure_file(.AGCM_VERSION .AGCM_VERSION @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/.AGCM_VERSION DESTINATION etc)


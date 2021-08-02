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

target_link_libraries (GEOSgcm.x OpenMP::OpenMP_Fortran)
target_include_directories (GEOSgcm.x PUBLIC ${INC_ESMF})

file (GLOB templates CONFIGURE_DEPENDS *.tmpl)

set (programs
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

if(HYDROSTATIC)
   set(CFG_HYDROSTATIC TRUE)
else()
   set(CFG_HYDROSTATIC FALSE)
endif()

# Did we build for AMD Rome hardware (aka EPYC)?
cmake_host_system_information(RESULT proc_decription QUERY PROCESSOR_DESCRIPTION)
if (${proc_decription} MATCHES "EPYC")
  set(CFG_BUILT_ON_ROME TRUE)
else ()
  set(CFG_BUILT_ON_ROME FALSE)
endif ()

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


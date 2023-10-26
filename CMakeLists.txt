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
if (GPU_PHYSICS)
  # We need the SHELL prefix to get around the error where the flags are quoted as in
  # gfortran: error: unrecognized command-line option '-fopenacc -foffload=nvptx-none’
  target_link_options(GEOSgcm.x PRIVATE SHELL:${GEOS_Fortran_OpenMP_Offload_Flags_Libs})
  target_link_options(idfupd.x PRIVATE SHELL:${GEOS_Fortran_OpenMP_Offload_Flags_Libs})
endif ()

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

if(HYDROSTATIC)
   set(CFG_HYDROSTATIC TRUE)
else()
   set(CFG_HYDROSTATIC FALSE)
endif()

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


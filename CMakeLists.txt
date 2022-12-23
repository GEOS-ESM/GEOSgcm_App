ecbuild_add_executable (
  TARGET GEOSgcm.x
  SOURCES GEOSgcm.F90
  LIBS GEOSgcm_GridComp esmf OpenMP::OpenMP_Fortran
  )

#target_link_libraries(GEOSgcm.x ${modele_model_lib} ${modele_profiler_lib}  ${modele_dd2d_lib} ${modele_gisslsm_lib} ${modele_ent_lib} ${modele_solvers_lib} ${modele_mpi_lib} ${modele_shared_lib} )
#target_link_libraries(GEOSgcm.x ${modele_profiler_lib}  ${modele_dd2d_lib} ${modele_gisslsm_lib} ${modele_ent_lib} ${modele_solvers_lib} ${modele_mpi_lib} ${modele_shared_lib} ${modele_model_lib})
#target_link_libraries(GEOSgcm.x ${modele_model_lib} ${modele_shared_lib}  ${modele_mpi_lib} ${modele_solvers_lib} ${modele_ent_lib} ${modele_gisslsm_lib} ${modele_dd2d_lib} ${modele_profiler_lib})
#target_link_libraries(GEOSgcm.x ${modele_shared_lib}  ${modele_mpi_lib} ${modele_solvers_lib} ${modele_ent_lib} ${modele_gisslsm_lib} ${modele_dd2d_lib} ${modele_profiler_lib} ${modele_model_lib})

# these are not the modele base order 
target_include_directories(GEOSgcm.x PRIVATE ${modele_inc_dir})

# I think the next 8 lines are the right order
#target_link_libraries(GEOSgcm.x  ${modele_model_lib})
#target_link_libraries(GEOSgcm.x  ${modele_prof_lib})
#target_link_libraries(GEOSgcm.x  ${modele_dd2d_lib})
#target_link_libraries(GEOSgcm.x  ${modele_gisslsm_lib})
#target_link_libraries(GEOSgcm.x  ${modele_ent_lib})
#target_link_libraries(GEOSgcm.x  ${modele_solvers_lib})
#target_link_libraries(GEOSgcm.x  ${modele_mpi_lib})
#target_link_libraries(GEOSgcm.x  ${modele_shared_lib})

#target_link_libraries(GEOSgcm.x  ${modele_shared_lib})
#target_link_libraries(GEOSgcm.x  ${modele_mpi_lib})
#target_link_libraries(GEOSgcm.x  ${modele_solvers_lib})
#target_link_libraries(GEOSgcm.x  ${modele_gisslsm_lib})
#target_link_libraries(GEOSgcm.x  ${modele_ent_lib})
#target_link_libraries(GEOSgcm.x  ${modele_dd2d_lib})
#target_link_libraries(GEOSgcm.x  ${modele_prof_lib})
#target_link_libraries(GEOSgcm.x  ${modele_model_lib})

target_link_directories(GEOSgcm.x PRIVATE ${CMAKE_BINARY_DIR}/lib)
target_link_libraries(GEOSgcm.x model profiler dd2d giss_LSM Ent solvers MPI_Support shared)

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

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


add_executable (GEOSgcm.x GEOSgcm.F90)

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

#link_directories (${MKL_LIBRARIES})
target_link_libraries (GEOSgcm.x GEOSgcm_GridComp MAPL_Base GMAO_pFIO ${OpenMP_Fortran_LIBRARIES})
set_target_properties(GEOSgcm.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_include_directories (GEOSgcm.x PUBLIC ${INC_ESMF})
install(TARGETS GEOSgcm.x DESTINATION ${CMAKE_BINARY_DIR}/bin)

set(templates
  AGCM.rc.tmpl
  CAP.rc.tmpl
  GCMPROG.rc.tmpl
  GCMTRAJ.rc.tmpl
  HISTAENS.rc.tmpl
  HISTFDDA.rc.tmpl
  HISTFDDAL.rc.tmpl
  HISTORY.AGCM.rc.tmpl
  HISTORY.rc.tmpl
  HISTORY_GMICHEM.rc.tmpl
  HISTORY_MERRA2-DD.rc.tmpl
  HISTORY_MERRA2.rc.tmpl
  HISTORY_MERRA_CFIO.rc.tmpl
  HISTORY_STRATCHEM.rc.tmpl
  IDFUPD.rc.tmpl
  gcm_forecast.tmpl
  gcm_plot.tmpl
  fvcore_layout.rc
  gcm_run.j
  gcm_regress.j
  gcm_stats.j
  gcm_post.j
  gcm_archive.j
  gcm_convert.j
  gcm_quickplot.csh
  gcm_moveplot.j
  gcm_forecast.setup
  gcm_emip.setup
  .AGCM_VERSION
  )

# Need to copy various files into build directory.  Source tree cannot know where the build tree is ...
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/gcm_setup ${CMAKE_CURRENT_BINARY_DIR}/gcm_setup COPYONLY)
foreach(template ${templates})
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${template} ${CMAKE_CURRENT_BINARY_DIR}/${template} COPYONLY)
endforeach()


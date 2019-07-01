ecbuild_add_executable (
  TARGET GEOSgcm.x
  SOURCES GEOSgcm.F90
  LIBS GEOSgcm_GridComp 
  )

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

#link_directories (${MKL_LIBRARIES})
target_link_libraries (GEOSgcm.x ${OpenMP_Fortran_LIBRARIES})
set_target_properties(GEOSgcm.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_include_directories (GEOSgcm.x PUBLIC ${INC_ESMF})

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
   fvcore_layout.rc
   .AGCM_VERSION
   )

set (programs
   gcm_setup
   gcm_forecast.tmpl
   gcm_plot.tmpl
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
   )


install (
   FILES ${templates}
   DESTINATION bin
   )

install (
   PROGRAMS ${programs}
   DESTINATION bin
   )

install (
   FILES fvcore_layout.rc saverst.rc
   DESTINATION etc
   )

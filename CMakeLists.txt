cmake_minimum_required (VERSION 3.17)

project (
  GEOSgcm_App
  VERSION 1.0.0
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  message(SEND_ERROR "In-source builds are disabled. Please issue cmake command in separate build directory.")
endif ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")

# Most users of this software do not (should not?) have permissions to
# install in the cmake default of /usr/local (or equiv on other os's).
# Below, the default is changed to a directory within the build tree
# unless the user explicitly sets CMAKE_INSTALL_PREFIX in the cache.
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/install" CACHE PATH "default install path" FORCE )
  message(STATUS "*** Setting default install prefix to ${CMAKE_INSTALL_PREFIX}.")
  message(STATUS "*** Override with -DCMAKE_INSTALL_PREFIX=<path>.")
endif()

add_subdirectory(src)

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
   gcm_setup.py
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

# This is to fill in the BASEDIR.rc file
set(BASEDIR_WITHOUT_ARCH "/path/to/baselibs")
configure_file(BASEDIR.rc.in BASEDIR.rc @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/BASEDIR.rc DESTINATION etc)

# This is to fill in the SITE.rc file
set(GEOS_SITE "NCCS")
configure_file(SITE.rc.in SITE.rc @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/SITE.rc DESTINATION etc)

# https://www.scivision.dev/cmake-auto-gitignore-build-dir/
# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

# Piggyback that file into install
install(
   FILES ${PROJECT_BINARY_DIR}/.gitignore
   DESTINATION ${CMAKE_INSTALL_PREFIX}
   )

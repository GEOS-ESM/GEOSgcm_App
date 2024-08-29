#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Archive Job
#######################################################################

#@BATCH_TIME@ARCHIVE_T
#@ARCHIVE_P
#@BATCH_JOBNAME@ARCHIVE_N
#@ARCHIVE_Q
#@BATCH_GROUP
#@BATCH_OUTPUTNAMEOUTPUT

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

@SETENVS

@MPT_SHEPHERD

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSBIN          @GEOSBIN

source $GEOSBIN/g5_modules
setenv @LD_LIBRARY_PATH_CMD ${LD_LIBRARY_PATH}
if ( $?BASEDIR ) then
    setenv @LD_LIBRARY_PATH_CMD ${@LD_LIBRARY_PATH_CMD}:${BASEDIR}/${ARCH}/lib
endif

#######################################################################
#                         Archive Commands
#######################################################################


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

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSBIN          @GEOSBIN

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#                         Archive Commands           
#######################################################################


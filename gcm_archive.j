#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Archive Job
#######################################################################

#{{ batch_time }}{{ archive_t }}
#{{ archive_p }}
#{{ batch_jobname }}{{ archive_n }}
#{{ archive_q }}
#{{ batch_group_full }}
#{{ batch_outputname }}OUTPUT

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

{{ mpi_config }}

{{ ocean.mpt_shepherd }}

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             {{ site }}
setenv GEOSBIN          {{ bin_path }}

source $GEOSBIN/g5_modules
setenv {{ ld_library_path_command }} ${LD_LIBRARY_PATH}
if ( $?BASEDIR ) then
    setenv {{ ld_library_path_command }} ${{'{'}}{{ ld_library_path_command }}{{'}'}}:${BASEDIR}/${ARCH}/lib
endif

#######################################################################
#                         Archive Commands
#######################################################################


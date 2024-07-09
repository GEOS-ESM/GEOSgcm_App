#!/bin/csh -x

#######################################################################
#                Batch Parameters for Post-Processing Job
#######################################################################

#{{ BATCH_TIME }}{{ POST_T }}
#{{ POST_P }}
#{{ BATCH_JOBNAME }}{{ POST_N }}_@COLLECTION.@YYYYMM
#{{ POST_Q }}
#{{ BATCH_GROUP }}
#{{ BATCH_OUTPUTNAME }}@POST_O
#{{ BATCH_JOINOUTERR }}

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

@SETENVS

{{ MPT_SHEPHERD }}

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             {{ SITE }}
setenv GEOSBIN          {{ GEOSBIN }}
setenv GEOSUTIL         {{ GEOSSRC }}
setenv BATCHNAME       "{{ POST_N }}"

source $GEOSBIN/g5_modules
setenv {{ LD_LIBRARY_PATH_CMD }} ${LD_LIBRARY_PATH}
if ( $?BASEDIR ) then
    setenv {{ LD_LIBRARY_PATH_CMD }} ${{{ LD_LIBRARY_PATH_CMD }}}:${BASEDIR}/${ARCH}/lib
endif

if( $?SLURM_NTASKS ) then
      setenv RUN_CMD "{{ RUN_CMD }}"
      set NCPUS = $SLURM_NTASKS
else if( $?PBS_NODEFILE ) then
      setenv RUN_CMD "{{ RUN_CMD }}"
      set NCPUS = `cat $PBS_NODEFILE | wc -l`
else
      set NCPUS = NULL
endif

#######################################################################
#                      Perform Post Processing
#######################################################################

$GEOSUTIL/post/gcmpost.script -source {{ EXPDIR }} -ncpus $NCPUS -collections @COLLECTION -rec_plt @YYYYMM

exit

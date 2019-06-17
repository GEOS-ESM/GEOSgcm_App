#!/bin/csh -x

#######################################################################
#                Batch Parameters for Post-Processing Job
#######################################################################

#SBATCH -t @POST_T
#@POST_P
#SBATCH --job-name=@POST_N
#@POST_Q
#@POST_S
#@BATCH_GROUP
#SBATCH -o @POST_O

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
setenv GEOSUTIL         @GEOSSRC/GMAO_Shared/GEOS_Util
setenv BATCHNAME       "@POST_N"

if( $?PBS_NODEFILE ) then
      setenv RUN_CMD "@RUN_CMD"
      set NCPUS = `cat $PBS_NODEFILE | wc -l`
else
      set NCPUS = NULL
endif

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#                      Perform Post Processing
#######################################################################

$GEOSUTIL/post/gcmpost_CPLFCSTpart.script -source @EXPDIR -ncpus $NCPUS -nostrict -expiy @EXPYR -expie @ENSM

exit

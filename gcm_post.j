#!/bin/csh -x

#######################################################################
#                Batch Parameters for Post-Processing Job
#######################################################################

#PBS -l walltime=@POST_T
#@POST_P
#PBS -N @POST_N
#@POST_Q
#@BATCH_GROUP
#PBS -o @POST_O
#PBS -j oe

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
setenv GEOSUTIL         @GEOSSRC
setenv BATCHNAME       "@POST_N"

setenv MODINIT @MODINIT
setenv SITEMODULES modules.${SITE}

if ($?MODINIT) then
if ( -e $GEOSBIN/$SITEMODULES) then
  source $MODINIT
  module purge
  module load $GEOSBIN/$SITEMODULES
else if ( -e $GEOSBIN/modules) then
  source $MODINIT
  module purge
  module load $GEOSBIN/modules
endif
endif

setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

if( $?SLURM_NTASKS ) then
      setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "
      set NCPUS = $SLURM_NTASKS
else if( $?PBS_NODEFILE ) then
      setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "
      set NCPUS = `cat $PBS_NODEFILE | wc -l`
else
      set NCPUS = NULL
endif

#######################################################################
#                      Perform Post Processing
#######################################################################

$GEOSUTIL/post/gcmpost.script -source @EXPDIR -ncpus $NCPUS -collections @COLLECTION -rec_plt @YYYYMM

exit

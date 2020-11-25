#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Plot Job
#######################################################################

#SBATCH --time=06:00:00
#SBATCH --nodes=@NODES --ntasks-per-node=1
#SBATCH --job-name=quickstat
#SBATCH --output=quickstat.out
#SBATCH --@QUEUE=@PARTITION
#SBATCH --array=1-@NTASKS

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv GEOSBIN          @GEOSBIN
setenv GEOSUTIL         @GEOSSRC

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#                         Quickstat Commands           
#######################################################################

cd $GEOSUTIL/plots

@ ID = ${SLURM_ARRAY_TASK_ID}


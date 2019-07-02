#!/bin/csh -f
alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

#######################################################################
#                     Batch Parameters for Plot Job
#######################################################################

#SBATCH --time=12:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=quickstat
#SBATCH --constraint=hasw
#SBATCH  -o quickstat.out
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

#######################################################################
#                         Quickstat Commands           
#######################################################################

cd $GEOSUTIL/plots

@ ID = ${SLURM_ARRAY_TASK_ID}


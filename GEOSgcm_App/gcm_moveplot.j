#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Plot Job
#######################################################################

#PBS -l walltime=1:00:00
#PBS -l select=1:ncpus=1
#PBS -N @MOVE_N
#@MOVE_Q
#@BATCH_GROUP
#PBS -o gcm_moveplot.o

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

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    EXPID   @EXPID
setenv    EXPDIR  @EXPDIR
setenv    HOMDIR  @HOMDIR

#######################################################################
#                           MOVE Commands
#######################################################################

cd $EXPDIR/plot

set  FREQUENCY = `grep PLOT_FREQUENCY: plot.rc | cut -d'#' -f1 | cut -d':' -f2 | tr "[:lower:]" "[:upper:]"`
set  MOVE_OPTS = `grep PLOT_MOVE: plot.rc | cut -d'#' -f1 | cut -d':' -f2`
set   NUM_OPTS = $#MOVE_OPTS

if( $FREQUENCY == "CLIM" ) then
     cd $EXPDIR/plots_CLIM
     $GEOSUTIL/plots/moveplot -plotrc $EXPDIR/plot/plot.rc $MOVE_OPTS[2-$NUM_OPTS]
else
     set ENDDATE = `grep -i enddate MASTERLIST | grep -v tseries`
     @ n = 1
     while( "$ENDDATE[$n]" != "-enddate" )
     @ n = $n + 1
     end
     @ n = $n + 1
     set YEAR = `echo $ENDDATE[$n] | cut -c1-4`

     cd $EXPDIR/plots_Y$YEAR
     $GEOSUTIL/plots/moveplot -plotrc $EXPDIR/plot/plot.rc -year Y$YEAR $MOVE_OPTS[2-$NUM_OPTS]
endif


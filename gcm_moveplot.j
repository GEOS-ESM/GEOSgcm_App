#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Plot Job
#######################################################################

#{{ batch_time }}1:00:00
#{{ move_p }}
#{{ batch_jobname }}{{ move_n }}
#{{ move_q }}
#{{ batch_group_full }}
#{{ batch_outputname }}gcm_moveplot.o

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
setenv GEOSUTIL         {{ install_path }}

source $GEOSBIN/g5_modules
setenv {{ ld_library_path_command }} ${LD_LIBRARY_PATH}
if ( $?BASEDIR ) then
    setenv {{ ld_library_path_command }} ${{'{'}}{{ ld_library_path_command }}{{'}'}}:${BASEDIR}/${ARCH}/lib
endif

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    EXPID   {{ expConfig.experiment_id }}
setenv    EXPDIR  {{ exp_dir }}
setenv    HOMDIR  {{ exp_dir }}

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


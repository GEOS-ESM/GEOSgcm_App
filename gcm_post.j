#!/bin/csh -x

#######################################################################
#                Batch Parameters for Post-Processing Job
#######################################################################

#{{ batch_time }}{{ post_t }}
#{{ post_p }}
#{{ batch_jobname }}{{ post_n }}_@COLLECTION.@YYYYMM
#{{ post_q }}
#{{ batch_group_full }}
#{{ batch_outputname }}@POST_O
#{{ batch_joinouterr }}

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
setenv BATCHNAME       "{{ post_n }}"

source $GEOSBIN/g5_modules
setenv {{ ld_library_path_command }} ${LD_LIBRARY_PATH}
if ( $?BASEDIR ) then
    setenv {{ ld_library_path_command }} ${{'{'}}{{ ld_library_path_command }}{{'}'}}:${BASEDIR}/${ARCH}/lib
endif

if( $?SLURM_NTASKS ) then
      setenv RUN_CMD "{{ run_command }}"
      set NCPUS = $SLURM_NTASKS
else if( $?PBS_NODEFILE ) then
      setenv RUN_CMD "{{ run_command }}"
      set NCPUS = `cat $PBS_NODEFILE | wc -l`
else
      set NCPUS = NULL
endif

#######################################################################
#                      Perform Post Processing
#######################################################################

$GEOSUTIL/post/gcmpost.script -source {{ exp_dir }} -ncpus $NCPUS -collections @COLLECTION -rec_plt @YYYYMM

exit

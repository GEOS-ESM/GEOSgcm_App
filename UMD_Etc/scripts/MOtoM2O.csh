#!/bin/csh -v

#SBATCH --time=8:00:00
#SBATCH --ntasks=1200
#SBATCH --ntasks-per-node=27
#SBATCH --job-name=re-init
##SBATCH --qos=high
#SBATCH -A g0609
#SBATCH -o re-init.o%j
#SBATCH -e re-init.e%j

#######################################################################
#                         System Settings
#######################################################################       

umask 022
limit stacksize unlimited

#######################################################################
#           BEGIN USER INPUT           
#######################################################################         

setenv GEOSDIR  SANDBOX_DIR #/home/aborovik/esma/yuri-S2S-2_1_UNSTABLE/GEOSodas  #  /gpfsm/dnb42/projects/p17/ehackert/geos5/sandbox_try2/GEOSodas/
setenv HOMDIR  MY_HOME_DIR #/home/aborovik/exp/S2S-2_1_REINIT_002

set yyyy           = ${1}
set mmmdd          = ${2}
set EXPERIMENT_NAME = $mmmdd
set EXPERIMENT_PATH = ${HOMDIR}/Y${yyyy}/${mmmdd}/   #/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/GEOSodas5_4p1/expa05o05/EH_SG_REINIT/Y${yyyy}/${mmmdd}/   # <----- !!!!! User defined

#######################################################################
#           END USER INPUT           
#######################################################################         

set dd = ` echo $mmmdd | awk '{print substr($0,4,2)}' `
set mmm = ` echo $mmmdd | awk '{print substr($0,1,3)}' `
if ($mmm == 'jan')then
 set mm = 01
endif
if ($mmm == 'feb')then
 set mm = 02
endif
if ($mmm == 'mar')then
 set mm = 03
endif
if ($mmm == 'apr')then
 set mm = 04
endif
if ($mmm == 'may')then
 set mm = 05
endif
if ($mmm == 'jun')then
 set mm = 06
endif
if ($mmm == 'jul')then
 set mm = 07
endif
if ($mmm == 'aug')then
 set mm = 08
endif
if ($mmm == 'sep')then
 set mm = 09
endif
if ($mmm == 'oct')then
 set mm = 10
endif
if ($mmm == 'nov')then
 set mm = 11
endif
if ($mmm == 'dec')then
 set mm = 12
endif

######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###################

setenv EMISSIONS MERRA2
setenv ARCH `uname`


cd ${HOMDIR}

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################         

setenv RUN_CMD         "mpirun -np "
module purge
set MODS = `$GEOSDIR/Linux/bin/g5_modules modules`
module load $MODS

#######################################################################         

#mkdir $EXPERIMENT_PATH/RESTART

./MOtoM2O.py $yyyy $mmmdd

setenv    EXPID   $EXPERIMENT_NAME
setenv    EXPDIR  $EXPERIMENT_PATH
setenv    SCRDIR  $EXPDIR/scratch

# Copy ocean_das_config & HISTORY.rc to  EXPERIMENT_PATH
cp ./ocean_das_config $EXPERIMENT_PATH/ocean_das_config.bak
cp ./HISTORY.rc $EXPERIMENT_PATH/HISTORY.rc.bak

mkdir $EXPERIMENT_PATH/ocean_das
cp -r $GEOSDIR/src/Applications/UMD_Etc $EXPERIMENT_PATH/ocean_das
cp -r scratch_tmplt $EXPERIMENT_PATH/scratch

cd $EXPERIMENT_PATH

# Edit ocean_das_config
/usr/bin/sed "s?EXPERIMENT_NAME?${EXPERIMENT_NAME}?" ocean_das_config.bak > ocean_das_config.1
/usr/bin/sed "s?EXPERIMENT_PATH?${EXPERIMENT_PATH}?" ocean_das_config.1 > ocean_das_config
rm ocean_das_config.bak ocean_das_config.1

# Edit HISTORY.rc
/usr/bin/sed "s?EXPERIMENT_NAME?${EXPERIMENT_NAME}?" HISTORY.rc.bak > HISTORY.rc

# Copy sub-seasonal AGCM restarts in scratch
cp *_rst ./scratch

# Copy sub-seasonal OGCM restarts in scratch
rm -rf ./scratch/INPUT
cp -r RESTART ./scratch/INPUT
mkdir ./scratch/RESTART

cp /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OCEAN_DAS_RC/RST/rst-720x410x40-tri/grid_spec.nc $SCRDIR/INPUT
cp /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OCEAN_DAS_RC/RST/rst-720x410x40-tri/roughness_length.nc $SCRDIR/INPUT
cp /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OCEAN_DAS_RC/RST/rst-720x410x40-tri/tideamp.nc $SCRDIR/INPUT
cp -R /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OCEAN_DAS_RC/RST/rst-720x410x40-tri/vegdyn.data $SCRDIR

# Copy cap stuff to scratch
cp cap_restart ./scratch
cp CAP.rc ./scratch
cp HISTORY.rc ./scratch


####################################################################### 
####################################################################### 
#######################################################################  
# Now that we have cap_restart and CAP.rc, link correct BCs and GOCART files 

cd $EXPERIMENT_PATH/scratch

#set nymdc = $yyyy$mm$dd
#echo $nymdc

# Set Time Variables for Current_(c), Ending_(e), and Segment_(s) dates 
# ---------------------------------------------------------------------
set nymdc = `cat cap_restart | cut -c1-8`
set nymdf = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c6-13`
echo nymdc $nymdc
echo nymdf $nymdf

######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###################
###  THIS STUFF IS TIME_DEPENDENT, THAT'S WHY IT IS HERE RATHER THAN IN setup_reinit.j

#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep           NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY  = `grep           NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM  = `grep      AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM  = `grep      AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM  = `grep      AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM  = `grep      OGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM  = `grep      OGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`

set  OGCM_LM  = `grep OGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NX = `grep  OGCM_NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY = `grep  OGCM_NY: $HOMDIR/AGCM.rc | cut -d':' -f2`


echo AGCM_LM, ${AGCM_LM}


# Select proper MERRA-2 GOCART Emission RC Files
# (NOTE: MERRA2-DD has same transition date)
# ----------------------------------------------
if( ${EMISSIONS} == MERRA2 | \
    ${EMISSIONS} == MERRA2-DD ) then
    set MERRA2_Transition_Date = 20000401

    if( $nymdc < ${MERRA2_Transition_Date} ) then
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/19600101-20000331

######***********************NB***NB***NB*********************###################
# This is quick an dirty for REINIT only; revisit for ANALYSIS, especially for early dates and across the transition date: APRIL 01, 1999
         #if( $nymdf > ${MERRA2_Transition_Date} ) then
         # set nymdf = ${MERRA2_Transition_Date}
         # set oldstring = `cat CAP.rc | grep END_DATE:`
         # set newstring = "END_DATE: $nymdf $nhmsf"
         # /bin/mv CAP.rc CAP.tmp
         #            cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
         #endif
    else
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/20000401-present
    endif

    if( $AGCM_LM == 72 ) then
        /bin/cp --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          /bin/cp $file dummy
              cat       dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > `basename $file`
        end
    endif

endif

if(-e ExtData.rc )    /bin/rm -f   ExtData.rc
set  extdata_files = `/bin/ls -1 *_ExtData.rc`
cat $extdata_files > ExtData.rc 


######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###################
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###################
######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###################



# Cycle ocean DAS
cd $EXPERIMENT_PATH/scratch
set NX = 10     # Assumes 1200 cores !!!!! Should probably get the layout from AGCM.rc
set NY = 120    #
@  NPES = $NX * $NY
echo $NPES
$EXPERIMENT_PATH/ocean_das/UMD_Etc/UMD_scripts/oda_run.j $NX $NY

# Clean up
rm $EXPERIMENT_PATH/ocean_das/
rm -rf $EXPERIMENT_PATH/ocean_das/ana
rm -rf $EXPERIMENT_PATH/ocean_das/bkg
rm -rf $EXPERIMENT_PATH/ocean_das/ana_ts
rm -rf $EXPERIMENT_PATH/ocean_das/incr
rm -rf $EXPERIMENT_PATH/ocean_das/recenter
rm -rf $EXPERIMENT_PATH/ocean_das/oana-*/ana
rm -rf $EXPERIMENT_PATH/ocean_das/oana-*/bkg
rm -rf $EXPERIMENT_PATH/ocean_das/oana-*/incr
rm $EXPERIMENT_PATH/ocean_das/oana-*/*
rm $EXPERIMENT_PATH/ocean_das/oana-*/ocean_observer_*/*.dat
rm $EXPERIMENT_PATH/ocean_das/oana-*/ocean_observer_*/*.dat.nc
rm $EXPERIMENT_PATH/ocean_das/tmpcice*







#!/bin/csh
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title: ocean_observer.csh
# *
# * Description: Wrapper to run Steve Penny's Ocean observer.
# *
# * Args:
# *              yyyy           : 4 digit year
# *              mm             : 2 digit month
# *              dd             : 2 digit day
# *              hh             : 2 digit hour             
# *              seq            : Assimilation sequence 
# *              inovation_type : 'omf' or 'oma'
# *
# * Example:
# *              ocean_observer.csh 2003 06 15 18 1
# *
# * @author: Guillaume Vernieres
# */
#
# /////////////////////////////////////////////////////////////////////////
# Date: Dec 2015    

source ../ocean_das_config
module purge
set MODS = `$GEOSDIR/Linux/bin/g5_modules modules`
module load $MODS

limit stacksize unlimited

set Ne = ${ODAS_Ne}
set logit_transform = $ODAS_logit_transform

set yyyy           = ${1}
set mm             = ${2}
set dd             = ${3}
set hh             = ${4}
set seq            = ${5}
set inovation_type = ${6}
set ANADIR         = ${7}

if ( $seq == 1 ) then
    set OBS_TYPE = `echo $ODAS_OBS_TYPE_1`
endif
if ( $seq == 2 ) then
    set OBS_TYPE = `echo $ODAS_OBS_TYPE_2`
endif

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################    

if (! -e ${EXPDIR}/ocean_das            ) mkdir -p ${EXPDIR}/ocean_das 

setenv    UMD_OBSERVERDIR  $EXPDIR/ocean_das/${ANADIR}/ocean_observer_${yyyy}${mm}${dd}_${hh}/
if (! -e $UMD_OBSERVERDIR            ) mkdir -p $UMD_OBSERVERDIR

cd $UMD_OBSERVERDIR

###############################################################################
#   Copy rc/exec neccesary to run the observer
###############################################################################

ln -s ${ODAS_RC}/BKGERR/anom-${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}-${ODAS_GRID_TYPE}/grid_spec_${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}.nc grid_spec.nc     # Link Ocean grid
#ln -s ${ODAS_RC}/BKGERR/anom-${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}-${ODAS_GRID_TYPE}/ocean_temp_salt_mask_${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}.nc .     # Link Ocean mask
    
cp $UMD_LETKFRC/* .                    # Copy namelists
cp $UMD_LETKFSRC/oceanda.x .           # LETKF
cp $UMD_LETKFSRC/oceanobsop.x .        # Observing operator
cp $UMD_LETKFSRC/oceanobs_nc2bin.x .   # gmao obs format to binary readable by oceanobsop.x 
cp $UMD_LETKFUTILS/ocean_recenter.x .    # recentering when running with static members 

#######################################################################
#               Get Ocean Observations
####################################################################### 
echo '============================================='
echo 'Create observation files for '${OBS_TYPE}' for '${yyyy}' '${mm}' '${dd}' '${hh}
echo '============================================='
${UMD_LETKFUTILS}/ocean_obs.py ${yyyy} ${mm} ${dd} ${hh} ${OBS_TYPE}
set  Nobs = `cat Nobs`
if ( $Nobs == 0 ) then
    touch $SCRDIR'/OCN_OBS_'${yyyy}${mm}${dd}'_'${hh}   # Tell the scheduler that we're done!
    rm -r $UMD_OBSERVERDIR
    exit
endif

#######################################################################
#               Get Ocean State
####################################################################### 

# if hh = 00_z => Wait!!!  For some reason, the 00z files take time to get to the file system ... might be due to pchem stuff?
if ( ${hh} == '00') then
    echo 'Waiting ...'
    sleep 60 
    echo 'Done waiting ...'
endif

#if ( $ODAS_GRID_TYPE == tri ) then
#    set OCN3D = geosgcm_ocn3dT
#    set OCN2D = geosgcm_ocn2dT
#    set SEAICE = geosgcm_seaiceT
#endif

#if ( $ODAS_GRID_TYPE == reg ) then
#    set OCN3D = geosgcm_ocn3d
#    set OCN2D = geosgcm_ocn2d
#    set SEAICE = geosgcm_seaice
#endif

set center_fname3d    = ${EXPDIR}/scratch/${EXPID}.${OCN3D}.${yyyy}${mm}${dd}_${hh}00z.nc4
set center_fname2d    = ${EXPDIR}/scratch/${EXPID}.${OCN2D}.${yyyy}${mm}${dd}_${hh}00z.nc4
set center_fname_cice = ${EXPDIR}/scratch/${EXPID}.${SEAICE}.${yyyy}${mm}${dd}_${hh}00z.nc4

###############################################################################
#   Recenter
###############################################################################

#source ${EXPDIR}/ocean_recenter.csh $UMD_OBSERVERDIR K 
source ${UMD_LETKFSCRIPTS}/ocean_recenter.csh $UMD_OBSERVERDIR K

###############################################################################
# Run observer, get OMF's
###############################################################################

echo '============================================='
echo 'Running Observer over '${ODAS_Ne}' members (OMF) for '$OBS_TYPE
echo '============================================='
mpirun -np ${ODAS_Ne} ./oceanobsop.x -obsin ${yyyy}${mm}${dd}.dat -gues bkg/ # Get omf's
                                                                             # observer is done => write script to ping scheduler
${UMD_LETKFUTILS}/mean_ods.py ${yyyy} ${mm} ${dd} ${hh} ${inovation_type}    # Compute mean omf's

touch $SCRDIR'/OCN_OBS_'${yyyy}${mm}${dd}'_'${hh}   # Tell the scheduler that we're done!

#exit

# Analyse to get a proxy of the oma <== Quite slow and not needed/nor right! But a good indication of what the oma look like
#----------------------------------
#cp ${UMD_LETKFRC}/input_prof.nml input.nml	
#cp -r  $UMD_OBSERVERDIR/bkg $UMD_OBSERVERDIR/ana
#mpirun -np ${ODAS_Ne}  ./oceanda.x                                            # Analyze
#mpirun -np ${ODAS_Ne} ./oceanobsop.x -obsin ${yyyy}${mm}${dd}.dat -gues ana/ # Get oma's
#${UMD_LETKFUTILS}/mean_ods.py ${yyyy} ${mm} ${dd} ${hh} oma             # Compute mean oma's

# Clean up
#---------
rm -r $UMD_OBSERVERDIR/bkg
rm -r $UMD_OBSERVERDIR/ana
rm -r $UMD_OBSERVERDIR/recenter
rm -r $UMD_OBSERVERDIR/states
rm -r $UMD_OBSERVERDIR/tmpcice.*.nc


#set OBS_TYPE = "Argo CTD XBT TAO PIRATA RAMA Jason-1 Jason-2 Saral ERS-1 ERS-2 TOPEX GEOSAT-2 Envisat HY-2A"
#${UMD_LETKFUTILS}/ocean_obs.py ${yyyy} ${mm} ${dd} ${hh} ${OBS_TYPE} 
#./oceanobs_nc2bin.x -y ${yyyy} -m ${mm} -d ${dd} -indir1 gmao-obs- -outdir .
#mpirun -np ${ODAS_Ne} ./oceanobsop.x -obsin ${yyyy}${mm}${dd}.dat -gues bkg/ # Get oma's
#${UMD_LETKFUTILS}/mean_ods.py ${yyyy} ${mm} ${dd} ${hh} omf             # Compute mean oma's





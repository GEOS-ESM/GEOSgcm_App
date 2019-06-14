#!/bin/csh -fv

#Stuff to do: Test without -f ...

limit stacksize unlimited
setenv I_MPI_DAPL_UD enable

####################################################################### 
#                  Forecast and run Ocean Observer
#######################################################################                                                       
set NX = ${1}
set NY = ${2}

#Save date of analysis            
#---------------------                                                                                                     
set yyyy  = `cat cap_restart | cut -c1-4`
set mm    = `cat cap_restart | cut -c5-6`
set dd    = `cat cap_restart | cut -c7-8`
set hh    = `cat cap_restart | cut -c10-11`

cp $SCRDIR/cap_restart $SCRDIR/cap_restart_ana

if (! -e ${EXPDIR}/ocean_das ) mkdir -p ${EXPDIR}/ocean_das 
setenv DA_seq  1   #Obsolete ... need to remove ...
setenv ANADIR  oana-${yyyy}${mm}${dd}_${hh}
mkdir ${EXPDIR}/ocean_das/$ANADIR

#Prepare MERRA-2 SST
#-------------------
source ${EXPDIR}/ocean_das_config
set NP = $JOB_NDAYS
@ NP = $NP + 2
rm $SCRDIR/sst_*_1200z.nc $SCRDIR/AICE_*_1200z.nc
ln -s ./INPUT/grid_spec.nc .
mpirun -np $NP $UMD_LETKFUTILS/ocean_sponge.py $yyyy $mm $dd > ocean_sponge.out
rm temp_salt_sponge.nc 
$UMD_LETKFUTILS/ocean_iau.x -DO_SPONGE ${ODAS_dt_restore_sst}                           # Create sst relaxation files but no ocean increments
cp temp_salt_sponge.nc $SCRDIR/INPUT/
ln -s temp_salt_sponge.nc $SCRDIR/INPUT/temp_sponge_coeff.nc
ln -s temp_salt_sponge.nc $SCRDIR/INPUT/temp_sponge.nc

# Start scheduler for the observers
#----------------------------------
pkill python # Make sure obs scheduler is not running 
rm $SCRDIR/OCN_OBS_????????_??             
source ${EXPDIR}/ocean_das_config
#${EXPDIR}/ocean_observer.py           omf ${ANADIR} True $DA_seq &
${UMD_LETKFSCRIPTS}/ocean_observer.py omf ${ANADIR} True $DA_seq &

# Start the Forecast
#-------------------
@ NPES = $NX * $NY
$RUN_CMD $NPES ./GEOSgcm.x | tee geos.out

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc

#######################################################################
#                  Rewind and run the Ocean DAS
#######################################################################                                                                

# Save date for end of da window
#------------------------------                                                                                                     
set yyyy_e  = `cat cap_restart | cut -c1-4`
set mm_e  = `cat cap_restart | cut -c5-6`
set dd_e  = `cat cap_restart | cut -c7-8`
set hh_e  = `cat cap_restart | cut -c10-11`

pkill python                                         # Stop the scheduler ... a bit dangerous since it will kill everything python ...  
${UMD_LETKFUTILS}/wait4observer.py                   # Need to make sure the observers are done before starting the letkf               
echo "${yyyy}${mm}${dd} ${hh}0000" > cap_restart     # Rewind cap_restart                                                               
${UMD_LETKFSCRIPTS}/ocean_das.csh ${yyyy} ${mm} ${dd} ${hh}  > ocean_das_${yyyy}${mm}${dd}${hh}.out
    
##################################################################################                                                                
#                  Retrospective Forecast and run Ocean Observer to collect oma's
##################################################################################                                                                

# Remove stuff relevant to properly running the observer
mkdir $SCRDIR/fcst_hist
mv $SCRDIR/*.oletkf_*.nc4 $SCRDIR/fcst_hist/
pkill python         # Make sure obs scheduler is not running 
rm $SCRDIR/OCN_OBS_????????_??             
#  output OMA's
if( $ODAS_OUTPUT_OMA == True )then
    ${UMD_LETKFSCRIPTS}/ocean_observer.py oma ${ANADIR} True $DA_seq &
endif

# Add offset to sea-level
#${UMD_LETKFUTILS}/get_slv_offset.py $BASEDIR $SCRDIR $SCRDIR/INPUT/ocean_barotropic.res.nc > ${EXPDIR}/slv_offset_${yyyy}${mm}${dd}${hh}.out
${UMD_LETKFUTILS}/get_slv_offset.py $BASEDIR $SCRDIR $SCRDIR/INPUT/ocean_barotropic.res.nc > ${EXPDIR}/slv_offset.out
    
rm $SCRDIR/RESTART/* #Clean up to allow writting of ocean restarts
$RUN_CMD $NPES ./GEOSgcm.x | tee geos_ana.out
if( $ODAS_OUTPUT_OMA == True )then
    pkill python
    ${UMD_LETKFUTILS}/wait4observer.py 
endif

if( -e EGRESS ) then
    set rc = 0
else
    set rc = -1
endif
echo GEOSgcm Run Status: $rc



##################################################################
# Remove increments and sponges
##################################################################
rm $SCRDIR/INPUT/temp_increment.nc
rm $SCRDIR/INPUT/salt_increment.nc
rm $SCRDIR/INPUT/eta_increment.nc
rm temp_salt_sponge.nc $SCRDIR/INPUT/

##################################################################
# Rename and Move Intermediate Checkpoints
##################################################################
#Get date from cap_restart            
#-------------------------                                                                                                 
set yyyy  = `cat cap_restart | cut -c1-4`
set mm    = `cat cap_restart | cut -c5-6`
set dd    = `cat cap_restart | cut -c7-8`
set hh    = `cat cap_restart | cut -c10-11`

set RSTDIR = ${EXPDIR}/ocean_das/restarts/
mkdir $RSTDIR
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

#OGCM
#----
/bin/cp -r ./RESTART/ $RSTDIR

#AGCM
#----
/bin/cp -f *_checkpoint* $RSTDIR
cd $RSTDIR
$GEOSDIR/Linux/bin/stripname _checkpoint _rst
tar cvf  restarts.${edate}.tar *_rst RESTART
/bin/rm -rf *_rst RESTART

##################################################################
# cleanup stuff 
##################################################################

rm $SCRDIR/ocnobs_????????_??.e
rm $SCRDIR/ocnobs_????????_??.o
rm ${EXPDIR}/ocean_das/oana-*/ocean_observer_*/*.x
rm ${EXPDIR}/ocean_das/oana-*/*.grd
rm -rf ${EXPDIR}/ocean_das/oana-*/recenter



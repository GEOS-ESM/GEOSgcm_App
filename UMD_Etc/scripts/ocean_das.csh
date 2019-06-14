#!/bin/csh 

# ocean_das - run Ocean Static LETKF 
#
#      May 2016   Vernieres Initial script
#------------------------------------------------------------------

limit stacksize unlimited

source ../ocean_das_config
module purge
set MODS = `$GEOSDIR/Linux/bin/g5_modules modules`
module load $MODS
set BASEDIR = `$GEOSDIR/Linux/bin/g5_modules basedir`

#######################################################################
#               Set Analysis Date Time
####################################################################### 

set yyyy       = ${1} 
set mm         = ${2} 
set dd         = ${3} 
set hh         = ${4} 

set ANADIR  = oana-${yyyy}${mm}${dd}_${hh}

#######################################################################
#               Get Ocean Restarts
####################################################################### 


set DAHH = ${ODAS_IAU_TIME}          # Analyzed at IC time + 18hrs = Time for IAU

set center_ctrl       = ${EXPDIR}/scratch/${EXPID}.${OCNCTRL}.${yyyy}${mm}${dd}_${DAHH}00z.nc4   # T & S on native grid
set center_fname3d    = ${EXPDIR}/scratch/${EXPID}.${OCN3D}.${yyyy}${mm}${dd}_${DAHH}00z.nc4
set center_fname2d    = ${EXPDIR}/scratch/${EXPID}.${OCN2D}.${yyyy}${mm}${dd}_${DAHH}00z.nc4
set center_fname_cice = ${EXPDIR}/scratch/${EXPID}.${SEAICE}.${yyyy}${mm}${dd}_${DAHH}00z.nc4

#Convert T in center_ctrl in deg C
${BASEDIR}/Linux/bin/ncap -O -s "temp=(temp-273.15)" $center_ctrl $center_ctrl   # !!! DANGER !!! the background file at the time 
										 #                of the analysis is now in C
                                                                                 #                while the others are in K  

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################    

if (! -e $UMD_LETKFSCRDIR            ) mkdir -p $UMD_LETKFSCRDIR

###############################################################################
#   Move to Scratch Clean and Copy rc/exec neccesary to run the ocean das
###############################################################################

cd $UMD_LETKFSCRDIR

#Clean stuff
#-----------
rm obs010*.dat
rm NOUT-0*
rm recenter*.nc
rm -r ana bkg states
rm *.out
rm gmao-obs-*.nc
rm *.txt
rm -r ana
rm -r bkg
rm -r omfs
rm -r omas

# Get resource files and binaries
#--------------------------------
rm grid_spec.nc
ln -s ${ODAS_RC}/BKGERR/anom-${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}-${ODAS_GRID_TYPE}/grid_spec_${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}.nc grid_spec.nc     # Link Ocean grid
cp $UMD_LETKFRC/* .                      # Copy namelists
cp $UMD_LETKFSRC/oceanda.x .             # LETKF
cp $UMD_LETKFSRC/oceanobsop.x .          # Observing operator
cp $UMD_LETKFSRC/oceanobs_nc2bin.x .     # gmao obs format to binary readable by oceanobsop.x 
cp $UMD_LETKFUTILS/ocean_recenter.x .    # recentering when running with static members 

# Need to change permissions of ,v in repository ...
chmod +x ${UMD_LETKFUTILS}/mean_ods.py
chmod +x ${UMD_LETKFUTILS}/rst2hist.py

echo '============================================='
echo 'Analysis valid for '${yyyy}${mm}${dd}' '${DAHH}
echo '============================================='

#source ${EXPDIR}/ocean_recenter.csh ${UMD_LETKFSCRDIR} K #C
source ${UMD_LETKFSCRIPTS}/ocean_recenter.csh ${UMD_LETKFSCRDIR} K #C

cp -r ${UMD_LETKFSCRDIR}/bkg ${UMD_LETKFSCRDIR}/ana

# Concatenate obs*.dat files from multiple observers
#---------------------------------------------------
set n = 0
while ( $n < $ODAS_Ne )
    @ n++
    if ($n<10) then
        set nnn = '00'$n
    endif
    if ($n>9) then
        set nnn = '0'$n
    endif
    if ($n>99) then
        set nnn = $n
    endif
    echo obs01${nnn}.dat
    cat ${EXPDIR}/ocean_das/${ANADIR}/ocean_observer_*/obs*${nnn}.dat > ${UMD_LETKFSCRDIR}/obs01${nnn}.dat
end	 

echo '============================================='
echo 'Run NCEP Ocean DAS'
echo '============================================='

# NOTE: DA-1, DA-2 & DA-3 are independent and could/should be done in parallel.  
set MLD_FILTER = False
set S_infl = 1.0

# DA-1: Assimilation of T, S, SST & ADT   
${UMD_LETKFUTILS}/ocean_letkf_nml.py   # Create namelist for oceanda.x

mpirun -np $ODAS_NPE_ocean ./oceanda.x

# DA-2: Assimilation of ice fraction
if (${ODAS_do_aice} == 'True') then
    rm -r ${EXPDIR}/ocean_das/ana_ts
    cp -r ${EXPDIR}/ocean_das/ana ${EXPDIR}/ocean_das/ana_ts #Save intermediate analysis
    ${UMD_LETKFUTILS}/ocean_letkf_nml.py -c T  # Use sea-ice specific localization length scale
    mpirun -np $ODAS_NPE_cice ./oceanda.x
    
    # Recombine analyses
    set n = 0
    while ( $n < $ODAS_Ne )
	@ n++
	if ($n<10) then
	    set nnn = '00'$n
	endif
	if ($n>9) then
	    set nnn = '0'$n
	endif
	if ($n>99) then
	    set nnn = $n
	endif
	echo obs01${nnn}.dat
	cp ${EXPDIR}/ocean_das/ana_ts/${nnn}/ocean_temp_salt.res.nc ${EXPDIR}/ocean_das/ana/${nnn}/ocean_temp_salt.res.nc
	cp ${EXPDIR}/ocean_das/ana_ts/${nnn}/ocean_barotropic.res.nc ${EXPDIR}/ocean_das/ana/${nnn}/ocean_barotropic.res.nc
    end	 
endif

# DA-3: Assimilation of ice thickness
#cp ${UMD_LETKFRC}/input_hice.nml input.nml
#mpirun -np 264 ./oceanda.x
#cp -r ana ana_hice    

#exit

#mpirun -np 84 ./oceanda.x #| tee oda.out

# oceanda.x profiling, Nobs = 507,207 Ne = 20
#    CPUS     TIME (s)
#    16       223.6
#    32       100.46
#    64       65.18
#   128       50.56
#   256       49.5


# oceanda.x profiling, Nobs = 75,075 Ne = 20
#    CPUS     TIME (s)
#    16       45.74
#    20       41.65
#    32       44
#    64       
#   128       
#   256       

# Post process
#-------------
echo '============================================='
echo 'Postprocess'
echo '============================================='
#get mom's namelist
cp $EXPDIR/scratch/input.nml $UMD_LETKFSCRDIR                        # Get MOM's namelist (used by ocean_moments.x))
mpirun -np ${ODAS_Ne} ${UMD_LETKFUTILS}/ocean_moments.x              # Creates stats of analysis under /mean_ana_restart/*.nc

###############################################################################
#   Mapping from ocean das grid to model grid
###############################################################################

echo '============================================='
echo 'Regridding analysis'
echo '============================================='    

set bkg_temp_salt = ${center_ctrl}
set bkg_saltwater = $EXPDIR/scratch/saltwater_internal_rst

if (${ODAS_do_aice} == 'True') then
    echo "Saving mean ocean-sea-ice analysis"
    ${UMD_LETKFUTILS}/ana2model.py ${OCEAN_Nx} ${OCEAN_Ny} ${OCEAN_Nz} $EXPDIR/ocean_das/mean_ana_restart/incr.nc ${bkg_temp_salt} ANA-ocean_temp_salt.res.nc $EXPDIR/ocean_das/mean_ana_restart/sea-ice.nc
else
    echo "Saving mean ocean analysis assuming no sea-ice update"
    ${UMD_LETKFUTILS}/ana2model.py ${OCEAN_Nx} ${OCEAN_Ny} ${OCEAN_Nz} $EXPDIR/ocean_das/mean_ana_restart/incr.nc ${bkg_temp_salt} ANA-ocean_temp_salt.res.nc
endif
cp ${bkg_temp_salt}  ${EXPDIR}/ocean_das/${ANADIR}/BKG-ocean_temp_salt.res.nc  # Save background (native grid)
#cp ANA-ocean_temp_salt.res.nc ${bkg_temp_salt}                                # Over-write background with analysis
cp ANA-ocean_temp_salt.res.nc ${EXPDIR}/ocean_das/${ANADIR}/                   # Save analysis (native grid)
cp ANA-seaice.nc ${EXPDIR}/ocean_das/${ANADIR}/	

###############################################################################
#   Dump Sea-Ice analysis in saltwater/temp_salt
###############################################################################
if (${ODAS_do_aice} == 'True') then
    cp $UMD_LETKFUTILS/anaice2rst.x .
    rm grid_spec.nc                    # Remove analysis grid
    ln -s $SCRDIR/INPUT/grid_spec.nc . # Link native ocean grid
    cp $SCRDIR/tile.data .
    cp $bkg_saltwater .
    if( ! $?ODAS_do_reinit ) then
       echo 'ODAS_do_reinit not defined, assuming False' 
       ./anaice2rst.x -rst ${bkg_saltwater} -ana $SCRDIR/AICE_${yyyy}${mm}${dd}_1200z.nc -ana_fmt regular -do_aice .true. -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice .false. -do_hsno .false. -orst ANA-ocean_temp_salt.res.nc
    else 
        if (${ODAS_do_reinit} == 'True') then
          ./anaice2rst.x -rst ${bkg_saltwater} -ana ANA-seaice.nc -ana_fmt oletkf -do_aice .true. -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice .false. -do_hsno .false. -orst ANA-ocean_temp_salt.res.nc
        else
          ./anaice2rst.x -rst ${bkg_saltwater} -ana $SCRDIR/AICE_${yyyy}${mm}${dd}_1200z.nc -ana_fmt regular -do_aice .true. -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice .false. -do_hsno .false. -orst ANA-ocean_temp_salt.res.nc
        endif 
    endif
    cp $bkg_saltwater ${EXPDIR}/ocean_das/$ANADIR/saltwater_internal_rst_ORIG.nc4
    cp ocean_temp_salt.res.OUT.nc ${EXPDIR}/ocean_das/$ANADIR/ANA-ocean_temp_salt.res.nc
    cp saltwater_internal_rst_OUT.nc4 ${EXPDIR}/ocean_das/$ANADIR/
    cp saltwater_internal_rst_OUT.nc4 ${bkg_saltwater}
    #Add sea-ice mass increment to eta_t in barotropic restart
    #set mass_incr = `cat seaice_mass_incr.txt`
    #echo 'Mass incr = '$mass_incr
    #${BASEDIR}/Linux/bin/ncap -O -s "eta_t=(eta_t+${mass_incr})" $SCRDIR/INPUT/ocean_barotropic.res.nc $SCRDIR/INPUT/ocean_barotropic.res.nc > add_si_mass.out
endif

###############################################################################
#   Compute increment for MOM's IAU
###############################################################################
cp ${EXPDIR}/ocean_das/$ANADIR/ANA-ocean_temp_salt.res.nc .
cp ${bkg_temp_salt} ./ocean_temp_salt.res.nc
#cp ${EXPDIR}/ocean_das/seaice_mass_incr.txt 
rm grid_spec.nc                    # Remove analysis grid
ln -s $SCRDIR/INPUT/grid_spec.nc . # Link native ocean grid
cp $SCRDIR/input.nml .             # Get MOM's input.nml
if (${ODAS_do_aice} == 'True') then
$UMD_LETKFUTILS/ocean_iau.x -DO_ANA_INCR -DO_MASS_INCR
else
    $UMD_LETKFUTILS/ocean_iau.x -DO_ANA_INCR
endif


#Revert T in center_ctrl back to K
${BASEDIR}/Linux/bin/ncap -O -s "temp=(temp+273.15)" $center_ctrl $center_ctrl > ncap_back2K.out
cp temp_increment.nc $SCRDIR/INPUT/
cp salt_increment.nc $SCRDIR/INPUT/
cp eta_increment.nc $SCRDIR/INPUT/
mv temp_increment.nc ${EXPDIR}/ocean_das/$ANADIR/
mv salt_increment.nc ${EXPDIR}/ocean_das/$ANADIR/
mv eta_increment.nc ${EXPDIR}/ocean_das/$ANADIR/

###############################################################################
#   Clean up
###############################################################################
mv *.png $ANADIR
mv ana $ANADIR
mv bkg $ANADIR
mv incr $ANADIR
mv mean_ana_restart $ANADIR  
mv recenter $ANADIR
mv *.out $ANADIR
mv NOUT* $ANADIR
mv gmao-obs*.nc $ANADIR
mv moments*.nc $ANADIR
mv ${yyyy}${mm}${dd}.dat $ANADIR 
mv *.dat $ANADIR
mv *.dat.nc $ANADIR 
mv *.grd $ANADIR 
cp *.nml $ANADIR
rm -rf states







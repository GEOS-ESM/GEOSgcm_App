#!/bin/csh

set RECENTER_WRKDIR = ${1}
set TEMP_UNITS      = ${2}  # Temperature units in center3d.nc C or K

###############################################################################
#   Recenter
###############################################################################

limit stacksize unlimited

#source ../ocean_das_config

set logit_transform = $ODAS_logit_transform

echo '============================================='
echo 'Observer centered at '${yyyy}${mm}${dd}' '${hh}
echo '============================================='

#Recenter ensemble
#-----------------
echo '============================================='
echo 'Setting Ensemble size and Recentering'
echo '============================================='

echo ${ODAS_Ne} > EnsembleSize.txt

cp $center_fname3d $RECENTER_WRKDIR/center3d.nc
cp $center_fname2d $RECENTER_WRKDIR/center2d.nc
if ($logit_transform == 'True') then
    cp $center_fname_cice $RECENTER_WRKDIR/center_cice_raw.nc
    ${UMD_LETKFUTILS}/cice_hack.py center_cice_raw.nc center_cice.nc $logit_transform False > cicestuff.out    #logit of AICE
else
    cp $center_fname_cice $RECENTER_WRKDIR/center_cice.nc
endif
${UMD_LETKFUTILS}/rst2hist.py center2d.nc center3d.nc
${UMD_LETKFSCRIPTS}/ocean_recenter.py ${yyyy} ${mm} ${dd} center3d.nc center2d.nc center_cice.nc ${ODAS_Ne} ${TEMP_UNITS}

mkdir recenter
mv recenter*.nc recenter
mv moments*${yyyy}${mm}${dd}.nc recenter

echo '============================================='
echo 'Recentered '${ODAS_Ne}' members around '${center_fname3d}' and '${center_fname2d}
echo '============================================='

#Organize bkg files
#------------------
mkdir bkg
set ENSDIR = ./recenter

# AICE, HICE, HSNO, DRAFT, FREEBOARD
#-----------------------------------
set n = 0
foreach ens_member (`ls -d ${ENSDIR}/recenter2d-${yyyy}${mm}${dd}.*.nc`)
    @ n++
    if ($n<10) then
        set ensmdir = '00'$n
    endif
    if ($n>9) then
        set ensmdir = '0'$n
    endif
    if ($n>99) then
        set ensmdir = $n
    endif
    echo '****************************'
    echo './bkg/'$ensmdir
    mkdir ./bkg/$ensmdir

    # Create file with sea-ice state
    set tmpcice = tmpcice.$n.nc
    ${UMD_LETKFUTILS}/cice_hack.py $ens_member $tmpcice False True     #logit of AICE
    cp ${RECENTER_WRKDIR}/$tmpcice ./bkg/${ensmdir}/ocean_velocity.res.nc
end

# T, S
#-----
set n = 0
foreach ens_member (`ls -d ${ENSDIR}/recenter3d-${yyyy}${mm}${dd}.*.nc`)
    @ n++
    if ($n<10) then
        set ensmdir = '00'$n
    endif
    if ($n>9) then
        set ensmdir = '0'$n
    endif
    if ($n>99) then
        set ensmdir = $n
    endif
    #mkdir ./bkg/$ensmdir

    cp ${RECENTER_WRKDIR}/$ens_member ./bkg/${ensmdir}/ocean_temp_salt.res.nc

    #link template sbc and barotropic
    #set TMPLRST = ${ODAS_RC}
    #set TMPLRST = /gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/ocean_stuff/mom4/gmao_letkf/rc/
    #cp ${TMPLRST}/${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}-${ODAS_GRID_TYPE}/ocean_sbc.res.nc ./bkg/${ensmdir}/ocean_sbc.res.nc
end

# SSH
#----
set n = 0
foreach ens_member (`ls -d ${ENSDIR}/recenter2d-${yyyy}${mm}${dd}.*.nc`)
    @ n++
    if ($n<10) then
        set ensmdir = '00'$n
    endif
    if ($n>9) then
        set ensmdir = '0'$n
    endif
    if ($n>99) then
        set ensmdir = $n
    endif
    #mkdir ./bkg/$ensmdir
    cp ${RECENTER_WRKDIR}/$ens_member ./bkg/${ensmdir}/ocean_barotropic.res.nc
end
echo $n members

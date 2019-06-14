#!/bin/csh
#
#########################################################################################################
# Usage:    convrstSALT.sh 19840416
#
# Function: corrects the saltwater and ocean_temp_salt restarts for use in subX forecast
# 
# Input:    original restarts are taken from Yury perpetual run and ODAS 
# Output:   is placed to /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/H54/OutData
# 20110809 20110814 missing as of 20161222 (taken the closest day)
# 20130416 20131023 missing as of 20161222 (taken the closest day)
#########################################################################################################
#

set IYR = 1999
#set MONS="1 2 3 4 5 6 7 8 9 10 11 12"
set MONS="4"

foreach IMO ( $MONS )

if ( $IMO == 1 )  set DAYS="01 06 11 16 21 26 31"
if ( $IMO == 2 )  set DAYS="05 10 15 20 25"
if ( $IMO == 3 )  set DAYS="02 07 12 17 22 27"
if ( $IMO == 4 )  set DAYS="01 06 11 16 21 26"
if ( $IMO == 5 )  set DAYS="01 06 11 16 21 26 31"
if ( $IMO == 6 )  set DAYS="05 10 15 20 25 30"
if ( $IMO == 7 )  set DAYS="05 10 15 20 25 30"
if ( $IMO == 8 )  set DAYS="04 09 14 19 24 29"
#if ( $IMO == 8 )  set DAYS="09"
if ( $IMO == 9 )  set DAYS="03 08 13 18 23 28"
if ( $IMO == 10 ) set DAYS="03 08 13 18 23 28"
if ( $IMO == 11 ) set DAYS="02 07 12 17 22 27"
if ( $IMO == 12 ) set DAYS="02 07 12 17 22 27"

foreach DAY1 ( $DAYS )

#set ICDATE = $1

if ($IMO < 10 ) set ICDATE = ${IYR}0${IMO}${DAY1}
if ($IMO >  9 ) set ICDATE = ${IYR}${IMO}${DAY1}

set YEAR = `echo $ICDATE | cut -c1-4`

echo "PROCESSING $YEAR $IMO : $ICDATE -----------------------------------------------------------------"

set UMD_Utils = /discover/nobackup/bzhao/yuri-Heracles-5_2_BETA/GEOSodas/src/Applications/GEOSgcm_App/UMD_Etc/UMD_utils
set executable = $UMD_Utils/anaice2rst_subX.x

#TILEFILE
# all 3 the same, but executable needs the hardwired name of tile (tile_a180x1080_o720x41.data)
#              /discover/nobackup/yvikhlia/coupled/Forcings/a180x1080_o720x410/CF0180x6C_TM0720xTM0410-Pfafstetter.til
#              /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2/OutData/CF0180x6C_TM0720xTM0410-Pfafstetter.til
set tilefile = tile_a180x1080_o720x41.data

#SEAICE
#              /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/seaice_aice_hice_????0416.nc
#set ana_file = /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${YEAR}/seaice_aice_hice_${ICDATE}.nc
# Zhao's old files
set ana_file = /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/backupold_201706/OUTPUT/${YEAR}/seaice_aice_hice_${ICDATE}.nc

#SALTWATER
# BIN's location
#set saltwater_in = /discover/nobackup/bzhao/seaice/odas_exp/subsea/${ICDATE}/old/saltwater_internal_rst (BIN location)
#set ocean_rst_in = /discover/nobackup/bzhao/seaice/odas_exp/subsea/${ICDATE}/old/ocean_temp_salt.res.nc (BIN location)
#set outdir = /discover/nobackup/bzhao/seaice/odas_exp/subsea/${ICDATE}/new
# location of saltwater for 1984-1986 experiment
#                  /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/H54/InData/saltwater_internal_rst${ICDATE}

# Deepthi's Free run location of saltwater
#   /gpfsm/dnb02/dvarier/subx_NOAA/aogcm_rs/cgcm_bkg_Yury/19910416/h54p1c180o05_2.saltwater_internal_rst.e19910416_00z.Heracles-5_4_p1.a180x1080_o720x410_CF0180x6C_DE0360xPE0180.nc4
# Yury location of saltwater
#   /gpfsm/dnb02/dvarier/subx_NOAA/aogcm_rs/salt_bkg_Yury/saltwater_internal_19990416
                
#cd /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/H54/InData  # Lena's

cd /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/restart/H54/InData # Anna's location
/bin/rm -f saltwater_internal_rst
ln -sf /gpfsm/dnb02/dvarier/subx_NOAA/aogcm_rs/salt_bkg_Yury/saltwater_internal_${ICDATE}_00z saltwater_internal_rst

#set saltwater_in = /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/H54/InData/saltwater_internal_rst
set saltwater_in = /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/restart/H54/InData/saltwater_internal_rst

#ODAS
set ocean_rst_in = /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/ODAS/RESTART.e${ICDATE}_00z/ocean_temp_salt.res.nc

#OUTDIR 
#set salt_dir_out = /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/H54/OutData # Lena's
set salt_dir_out = /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/restart/H54/OutData # Anna's location

ls -l ${saltwater_in}
set ostatus = $status
if ($ostatus > 0) then
   echo "SALT  NOT AVAILABLE, EXIT"
   exit
endif
ls -l ${ocean_rst_in}
set ostatus = $status
if ($ostatus > 0) then
   echo "OCEAN NOT AVAILABLE, EXIT"
   exit
endif
ls -l ${ana_file}
set ostatus = $status
if ($ostatus > 0) then
   echo "OBS   NOT AVAILABLE, EXIT"
   exit
endif
echo "READING ${ocean_rst_in}"
echo "READING ${ana_file}"
echo "WRITING ${salt_dir_out}"


#source /discover/nobackup/projects/gmao/t2ssp/build/hera/GEOSodas/src/g5_modules
module purge
module load comp/intel-15.0.2.164
module load mpi/impi-5.0.3.048
module load lib/mkl-15.0.2.164
module load other/comp/gcc-4.6.3-sp1
module load other/SIVO-PyD/spd_1.20.0_gcc-4.6.3-sp1_mkl-15.0.0.090
module load other/git-2.3.1

#cd /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/util # Lena's
cd /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/util # Anna's location

echo CREATE NEW SALTWATER FILES
$executable -rst $saltwater_in -ana $ana_file -do_aice .true. -do_all_1st .false. -hi_new 0.5 -do_skin_flush .true. -do_hice .true. -do_hsno .false. -orst $ocean_rst_in -tilefile $tilefile -target_tile 10000000 

echo MOVE DATA TO THE RESTARTS LOCATION
mkdir -p ${salt_dir_out}/${YEAR}
/bin/mv saltwater_internal_rst_OUT.nc4 ${salt_dir_out}/${YEAR}/saltwater_internal_rst${ICDATE}
/bin/mv ocean_temp_salt.res.OUT.nc     ${salt_dir_out}/${YEAR}/ocean_temp_salt.res.nc${ICDATE}

#py plot_tw_adjust.py $saltwater_in $out_dir/saltwater_internal_rst $2 
#/bin/mv TW_adj*.png $out_dir/
#py plot_sw_adjust.py $saltwater_in $out_dir/saltwater_internal_rst $2 
#/bin/mv SW_adj*.png $out_dir/

end

echo "COMPLETED $IYR $IMO"
sleep 10
end
echo "COMPLETED $IYR $MONS"

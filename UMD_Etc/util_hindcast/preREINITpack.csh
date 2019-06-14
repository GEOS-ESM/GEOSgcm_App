#!/bin/csh -fv

limit stacksize unlimited
setenv I_MPI_DAPL_UD enable

##################################################################
# Rename and Package pre-re-init restarts
##################################################################

echo "Enter year YYYY"
#set yyyy = $<
set yyyy = $1

echo "Enter month and day mmmdd, i.e. jul05"
#set mmmdd = $<
set mmmdd = $2

echo DATE: ${yyyy} ${mmmdd}

setenv GEOSDIR  /home/aborovik/esma/yuri-S2S-2_1_UNSTABLE/GEOSodas

set RUN_DIR = /gpfsm/dnb42/projects/p17/bzhao/geos5/exp/restart

set suffix = 'Heracles-5_4_p1.a180x1080_o720x410_CF0180x6C_DE0360xPE0180'
set ganymedtag = ".Ganymed-4_0_BETA10.Ganymed-4_0_MERRA-2_CF0180x6C_DE1440xPE0720.bin"

# ocean files location
set ocean_dir = '/discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/ODAS/'   #RESTART.e20120705_00z/
#-------------------------     
# saltwater files location
set saltwater_dir = '/gpfsm/dnb42/projects/p17/bzhao/geos5/exp/restart/restart/H54/OutData/'  #YYYY
#-------------------------     
# MERRA2 re-tiled files locations
set merra2_dir = '/gpfsm/dnb42/projects/p17/bzhao/geos5/exp/restart/restart/MERRA2/OutData/'  #YYYYMMDD
#-------------------------     
# Deepthi's replay files locations
set replay_dir = '/discover/nobackup/projects/gmao/t2ssp//subx/agcm_replay/rs_m200z/'  
#-------------------------     
# re-packaged pre-re-init restarts location
set reinit_dir = '/gpfsm/dnb42/projects/p17/bzhao/geos5/exp/restart/restart/REINIT/'

# parse the date     
#-------------------------      
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

#-------------------------     
set hh    = '00'

# Date of agcm replay
#------------------------------------------------------------------------------------------------------------
set icdate = ${yyyy}${mm}${dd}
@ icdatem1 = $icdate - 1
@ iym1 = $yyyy - 1
if ($mmmdd == 'jan01') set icdatem1 = ${iym1}1231
if ($mmmdd == 'apr01') set icdatem1 = ${yyyy}0331
if ($mmmdd == 'may01') set icdatem1 = ${yyyy}0430
set icdatem1tag = rp_${icdatem1}21z

set edate = ${yyyy}${mm}${dd}_${hh}z
echo $edate

mkdir ${reinit_dir}/tmp # to be over-written for each date
#OGCM
#----
/bin/cp -r ${ocean_dir}/RESTART.e${edate}/ ${reinit_dir}/tmp
/bin/cp -f ${saltwater_dir}/${yyyy}/ocean_temp_salt.res.nc${yyyy}${mm}${dd} ${reinit_dir}/tmp/RESTART.e${edate}/ocean_temp_salt.res.nc

#AGCM
#----
/bin/cp ${replay_dir}/restarts.e${icdate}_00z.tar ${reinit_dir}/tmp

cd ${reinit_dir}/tmp

# untar replay files
tar xvf restarts.e${edate}.tar
ls
# rename
#----
/bin/mv ${icdatem1tag}.fvcore_internal_rst.e${edate}${ganymedtag} ${mmmdd}.fvcore_internal_rst
/bin/mv ${icdatem1tag}.irrad_internal_rst.e${edate}${ganymedtag} ${mmmdd}.irrad_internal_rst
/bin/mv ${icdatem1tag}.moist_import_rst.e${edate}${ganymedtag} ${mmmdd}.moist_import_rst
/bin/mv ${icdatem1tag}.moist_internal_rst.e${edate}${ganymedtag} ${mmmdd}.moist_internal_rst
/bin/mv ${icdatem1tag}.pchem_internal_rst.e${edate}${ganymedtag} ${mmmdd}.pchem_internal_rst
/bin/mv ${icdatem1tag}.solar_internal_rst.e${edate}${ganymedtag} ${mmmdd}.solar_internal_rst
/bin/mv ${icdatem1tag}.surf_import_rst.e${edate}${ganymedtag} ${mmmdd}.surf_import_rst
/bin/mv ${icdatem1tag}.turb_internal_rst.e${edate}${ganymedtag} ${mmmdd}.turb_internal_rst
/bin/mv ${icdatem1tag}.turb_import_rst.e${edate}${ganymedtag} ${mmmdd}.turb_import_rst

rm -f ${icdatem1tag}.catch_internal_rst.e${edate}${ganymedtag}
rm -f ${icdatem1tag}.lake_internal_rst.e${edate}${ganymedtag}
rm -f ${icdatem1tag}.landice_internal_rst.e${edate}${ganymedtag}
rm -f ${icdatem1tag}.saltwater_*_rst.e${edate}${ganymedtag}

rm -f restarts.e${edate}.tar

# add re-tiled files
/bin/cp -f ${merra2_dir}/${yyyy}${mm}${dd}/* ${reinit_dir}/tmp
#rename
#----
mv catch_internal_rst ${mmmdd}.catch_internal_rst
mv lake_internal_rst ${mmmdd}.lake_internal_rst
mv landice_internal_rst ${mmmdd}.landice_internal_rst

# add saltwater
/bin/cp -f ${saltwater_dir}/${yyyy}/saltwater_internal_rst${yyyy}${mm}${dd} ${reinit_dir}/tmp
$GEOSDIR/Linux/bin/stripname _rst${yyyy}${mm}${dd} _rst
#rename
#----
mv saltwater_internal_rst ${mmmdd}.saltwater_internal_rst

# add tag to all AGCM files
$GEOSDIR/Linux/bin/stripname _rst _rst.e${yyyy}${mm}${dd}_${hh}z.${suffix}
rm vegdyn_internal_rst*

# pack up
#-------------------------
tar cvf restarts.e${edate}.tar *_rst* RESTART.e${edate}
mv restarts.e${edate}.tar ${reinit_dir}

# clean up tmp
#-------------------------
cd ${reinit_dir}/
rm -rf tmp


#!/bin/csh 
# script to run MERRA-2 AGCM replay for 3 hours starting from every 5 days for a single year to 
# generate MERRA-2 initial conditions for 00z for S2S reforecasts (last updated: Jun 2017)
# input: year

set yrm = $1
set yr = $yrm - 1
# change below home directory
set $home = /discover/nobackup/usrename/agcm_replay/

set mn = 12
set dy = 31
set hr = 21
set incr_sec = 10800  # 3 hours in secs to set end date in tick
set deltat = 432000  # 5 day increment to advance to next IC
set cmax = 73 # number of ICs (73 per year for 5-day interval ICs)

set MYBIN = /gpfsm/dnb02/dvarier/Ganymed-4_0_BETA10_Jul2016/GEOSagcm/src/GMAO_Shared/GEOS_Util/post/
set tmplxp = exp_tmpl
set tmple = ${home}/Y${yrm}/
set tmplh = ${home}/Y${yrm}/
set src = ${home}/Y${yrm}/setup/


#set days = "31:28:31:30:31:30:31:31:30:31:30:31"
#endif 
#set dym = `echo $days | cut -f${mn} -d:`

#echo $dym

if ( $mn <= 9 ) then
set mon = '0'$mn
else
set mon = $mn
endif 

if ( $dy <= 9 ) then
set date = '0'$dy
else
set date = $dy
endif

###############################################

set count = 1 
while ( $count <= $cmax ) # loop through days and submit one forecast run per day 

cd $src

rm -fr ${src}/files_temp
rm -fr ${src}/scratch
mkdir ${src}/files_temp

set eid = rp_$yr$mon$date${hr}z
set rid = rp_$yr$mon$date
set edir = $tmple/$eid
set hdir = $tmplh/$eid
set beg_day = $yr$mon$date
set beg_hour = ${hr}0000
set end_time = ( `${MYBIN}/tick $beg_day $beg_hour $incr_sec` )
echo 'print end_time'
echo $end_time

rm -fr ${tmple}/${eid}
rm -fr ${tmplh}/${eid}

cp -r  ${tmple}/${tmplxp} ${tmple}/${eid}  
cp -r  ${tmplh}/${tmplxp} ${tmplh}/${eid}  

cp -r ${src}/files/* ${src}/files_temp 
cd ${src}/files_temp

foreach file ( * )
echo $file 
sed -i s/@rid/${rid}/g $file
sed -i "s|@edir|$edir|g" $file
sed -i "s|@hdir|$hdir|g" $file
sed -i s/@eid/${eid}/g $file
sed -i s/@beg_day/${beg_day}/g $file
sed -i s/@beg_hour/${beg_hour}/g $file
sed -i "s/@end_time/${end_time}/g" $file
end 

# copy new files to exp and home directories
 mv -f CAP.rc ${hdir}
 mv -f gcm_run.j ${hdir}
 mv -f HISTORY.rc ${hdir}
# mv -f gcm_archive.j ${edir}/archive
 mv -f gcm_post.j ${edir}/post
 mv -f dot.HOMDIR ${edir}/.HOMDIR
 mv -f cap_restart ${edir}

# get restarts from archive and copy to exp dir
##############################################
echo 'getting restart files'

if ( $yr  >= 1980 & $yr <= 1991 ) then
set mdir = d5124_m2_jan79
endif
if ( $yr  >= 1992 & $yr <= 2000 ) then 
set mdir = d5124_m2_jan91
endif 

if ( $yr  >= 2001 & $yr <= 2010 ) then
set mdir = d5124_m2_jan00
endif

if ( $yr  >= 2011 & $yr <= 2016 ) then
set mdir = d5124_m2_jan10
endif

set arch = /archive/u/dao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/${mdir}/rs/Y${yr}/M${mon}/
echo ${arch}
echo 'getting files for '${beg_day}_${hr}z

cd ${arch}
dmget *fvcore_internal*${beg_day}_${hr}z*
dmget *moist_internal*${beg_day}_${hr}z*
dmget *catch_internal*${beg_day}_${hr}z*
dmget *lake_internal*${beg_day}_${hr}z*
dmget *landice_internal*${beg_day}_${hr}z*

dmget *moist_import*${beg_day}_${hr}z*
dmget *surf_import*${beg_day}_${hr}z*
dmget *turb_import*${beg_day}_${hr}z*
dmget *solar_internal*${beg_day}_${hr}z*
dmget *irrad_internal*${beg_day}_${hr}z*
dmget *turb_internal*${beg_day}_${hr}z*
dmget *pchem_internal*${beg_day}_${hr}z*

dmget *saltwater_internal*${beg_day}_${hr}z*
dmget *saltwater_import*${beg_day}_${hr}z*

rm -f ${edir}/*internal_rst
rm -f ${edir}/*import_rst

cp *fvcore_internal*${beg_day}_${hr}z* ${edir}/fvcore_internal_rst
cp *moist_internal*${beg_day}_${hr}z* ${edir}/moist_internal_rst
cp *catch_internal*${beg_day}_${hr}z* ${edir}/catch_internal_rst
cp *lake_internal*${beg_day}_${hr}z* ${edir}/lake_internal_rst
cp *landice_internal*${beg_day}_${hr}z* ${edir}/landice_internal_rst

cp *moist_import*${beg_day}_${hr}z* ${edir}/moist_import_rst
cp *surf_import*${beg_day}_${hr}z* ${edir}/surf_import_rst
cp *turb_import*${beg_day}_${hr}z* ${edir}/turb_import_rst

cp *solar_internal*${beg_day}_${hr}z* ${edir}/solar_internal_rst
cp *irrad_internal*${beg_day}_${hr}z* ${edir}/irrad_internal_rst
cp *turb_internal*${beg_day}_${hr}z* ${edir}/turb_internal_rst
cp *pchem_internal*${beg_day}_${hr}z* ${edir}/pchem_internal_rst

cp *saltwater_internal*${beg_day}_${hr}z* ${edir}/saltwater_internal_rst
cp *saltwater_import*${beg_day}_${hr}z* ${edir}/saltwater_import_rst

cd  ${edir}

echo 'ls output'
echo `ls -lrt *_rst | wc -l`
if ( `ls -lrt *_rst | wc -l` != 14 ) then 
echo 'restart files not complete, exiting forecast setup'
exit 
else 
echo 'restart files complete'
endif 

# submit run job 
cd ${hdir}
qsub gcm_run.j
echo 'submitted job #' rp_$yr$mon$date${hr}z

if ( $yrm % 4 == 0 & $count == 12 ) then
set deltat = 518400
else
set deltat = 432000
endif

set ndate = ( `${MYBIN}/tick $beg_day $beg_hour $deltat` )
set yr = ( `echo $ndate | cut -c 1-4` )
set mon = ( `echo $ndate | cut -c 5-6` )
set date = ( `echo $ndate | cut -c 7-8` )
set hr  =  ( `echo $ndate | cut -c 10-11` )

echo $yr $mon $date $hr

@ count++
end # count loop ends 

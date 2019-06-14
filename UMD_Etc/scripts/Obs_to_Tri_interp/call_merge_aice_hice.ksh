#!/usr/bin/ksh 
#-xv
#-----------------------------------------------

####   usage:##########
### call_merge_aicer_hice.ksh 1999
#############
### this script merge aice output and hice output into one file using nco command
### crashes if aice or hice files are missing
###
#############

year=$1

ncodir=/usr/local/other/SLES11.1/nco/4.4.4/intel-12.1.0.233/bin

remainder=$(($year%4))
if [ $remainder -eq 0 ]; then
set -A DAYS 31 29 31 30 31 30 31 31 30 31 30 31
set -A DAY_INTERVAL 30 28 30 29 30 29 30 30 29 30 29 30
else
set -A DAYS 31 28 31 30 31 30 31 31 30 31 30 31
set -A DAY_INTERVAL 30 27 30 29 30 29 30 30 29 30 29 30
fi

######################
   mkdir -p /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice_hice/
   mkdir -p /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/PNG/
   mon=1
   while [ $mon -le 12 ] ; do
   dd=1
   nn=$((mon-1))
   while [ $dd -le ${DAYS[${nn}]} ] ; do
   yyyymmdd=$((year*10000+mon*100+dd))
   echo $yyyymmdd
   aicefile=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/seaice_aice_$yyyymmdd.nc
   hicefile=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/hice/seaice_hice_$yyyymmdd.nc
   aice_hice_file=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice_hice/seaice_aice_hice_$yyyymmdd.nc
   if [ -e $aicefile ] && [ -e $hicefile ] ; then
   cd /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice_hice/
   /bin/cp $hicefile .
   /bin/cp $aicefile .
   $ncodir/ncks -A seaice_hice_$yyyymmdd.nc seaice_aice_$yyyymmdd.nc
   mv -f seaice_aice_$yyyymmdd.nc $aice_hice_file
   /bin/rm seaice_hice_$yyyymmdd.nc
   else
   echo 'no file: ' $yyyymmdd
   exit
   fi
   chmod 755 -R /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/*
   dd=$((dd +1))
   done
   mon=$((mon+1))
   done
#
#if [ "$(find  /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/missing/ -mindepth 1)" ] ; then
#./check_missing.sh $year
#fi
##### tar output files and push to archive
#else
#tar -cvf seaice_aice_hice_$year.tar /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/$year
#mv seaice_aice_hice_$year.tar /archive/u/zli7/Sub_seasonal/seaice_aice_hice/
#fi



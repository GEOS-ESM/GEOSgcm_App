#!/usr/bin/ksh 
#-xv
#-----------------------------------------------

####
### call_check_push_archive.ksh 1999
####

year=$1

remainder=$(($year%4))
if [ $remainder -eq 0 ]; then
set -A DAYS 31 29 31 30 31 30 31 31 30 31 30 31
set -A DAY_INTERVAL 30 28 30 29 30 29 30 30 29 30 29 30
else
set -A DAYS 31 28 31 30 31 30 31 31 30 31 30 31
set -A DAY_INTERVAL 30 27 30 29 30 29 30 30 29 30 29 30
fi

ncodir=/usr/local/other/SLES11.1/nco/4.4.4/intel-12.1.0.233/bin
######### check missing files in OUTPUT#############
   mkdir -p /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/missing/
   kk= ls /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/ |wc -l
   rm -f /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/missing/missingoutput*
   mon=1
   while [ $mon -le 12 ] ; do
   dd=1
   nn=$((mon-1))
   while [ $dd -le ${DAYS[${nn}]} ] ; do
   yyyymmdd=$((year*10000+mon*100+dd))


   year0=$year
   year1=$year
   year2=$year

   day1=$((yyyymmdd-1))
   checkyear=$((day1-year*10000-100))
   checkmonth=$((day1-year*10000-mon*100))
   if [ $checkyear -eq 0 ]; then
   day1=$(((year-1)*10000+12*100+31))
   year1=$((year-1))
   fi
   if [ $checkyear -ne 0 ] && [ $checkmonth -eq 0 ]; then
   xx=$((nn-1))
   day1=$((year*10000+(mon-1)*100+${DAYS[${xx}]}))
   fi
   day0=$((day1-2))
   checkyear2=$((day0-year*10000-100))
   checkmonth2=$((day0-year*10000-mon*100))
   if [ $checkyear -eq 0 ] && [ $checkyear2 -le 0 ]; then
   day0=$(((year-1)*10000+12*100+31-2))
   year0=$((year-1))
   fi
   if [ $checkyear -ne 0 ] && [ $checkyear2 -le 0 ]; then
   day0=$(((year-1)*10000+12*100+31+checkyear2))
   year0=$((year-1))
   fi
   if [ $checkmonth -eq 0 ] && [ $checkyear2 -gt 0 ] && [ $checkmonth2 -le 0 ]; then
   xx=$((nn-1))
   day0=$((year*10000+(mon-1)*100+${DAYS[${xx}]}-2))
   fi
   if [ $checkmonth -ne 0 ] && [ $checkyear2 -gt 0 ] && [ $checkmonth2 -le 0 ]; then
   xx=$((nn-1))
   day0=$((year*10000+(mon-1)*100+${DAYS[${xx}]}+checkmonth2))
   fi

   day2=$((yyyymmdd+1))
   checkyear3=$((day2-year*10000-12*100-32))
   if [ $checkyear3 -eq 0 ]; then
   day2=$(((year+1)*10000+100+1))
   year2=$((year+1))
   fi
   checkmonth3=$((day2-year*10000-mon*100-${DAYS[${nn}]}))
   if [ $checkyear3 -ne 0 ] && [ $checkmonth3 -gt 0 ]; then
   day2=$((year*10000+(mon+1)*100+1))
   fi
   
   rm -f /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/missing/missingoutput_$yyyymmdd
   filemissing=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/seaice_aice_$yyyymmdd.nc
   tempfile1=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/temp1_$yyyymmdd.nc
   tempfile2=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/temp2_$yyyymmdd.nc
   tempfile3=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/temp3_$yyyymmdd.nc
   tempfile4=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/temp4_$yyyymmdd.nc
   tempfile5=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/temp5_$yyyymmdd.nc
   chmod 755 /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/* 
   if [ ! -e $filemissing ] ; then
   aiceday0=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year0}/aice/seaice_aice_$day0.nc
   aiceday1=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year1}/aice/seaice_aice_$day1.nc
   aiceday2=/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year2}/aice/seaice_aice_$day2.nc
   #echo 'checkyear2=' $checkyear2
   #echo 'checkmonth2=' $checkmonth2
   echo "---------------------------"
   echo 'missingfile:' $yyyymmdd
   echo "aice0=" $day0
   echo "aice1=" $day1
   echo "aice2=" $day2
   echo $yyyymmdd,$year,$mon,$dd >> /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/missing/missingoutput_$yyyymmdd
   cd /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/
   cdo mulc,-0.125 $aiceday0 aice$day0.nc
   cdo mulc,0.75 $aiceday1 aice$day1.nc
   cdo mulc,0.375 $aiceday2 aice$day2.nc
   cdo -enssum aice$day0.nc aice$day1.nc aice$day2.nc $tempfile1 
   $ncodir/ncap2 -s "where(AICE<0) AICE = 0" $tempfile1 $tempfile2
   $ncodir/ncap2 -s "where(AICE>1) AICE = 1" $tempfile2 $tempfile3
   chmod 755 /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/${year}/aice/*
   $ncodir/ncap2 -s 'defdim("Time",1);AICE[Time,y, x]=AICE' $tempfile3 $tempfile4 
   $ncodir/ncrename -h -O -d y,yaxis_1 $tempfile4 $tempfile5
   $ncodir/ncrename -h -O -d x,xaxis_1 $tempfile5 $filemissing
   rm -f aice$day0.nc aice$day1.nc aice$day2.nc $tempfile1 $tempfile2 $tempfile3 $tempfile4 $tempfile5
   fi
   dd=$((dd +1))
   done
   mon=$((mon+1))
   done



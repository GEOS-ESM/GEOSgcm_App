##########################################################################################
#### CHECK THE SEASONAL FORECAST EXECUTION ###############################################
#### ENSEMBLE MEMBER IS ALWAYS 1 FOR HINDCAST ############################################
##########################################################################################
#  assumptions: 10 months , 9 collections:
#  assumptions: 10 months ,19 collections:
#  available on tape:   9 monthly (10 months)  9 daily (10 months) 2 diurnal (10 months)
#  available on tape:  19 monthly (10 months) 19 daily (10 months) 2 diurnal (10 months)
#  available on disk:  19 monthly (10 months)  2 daily (10 months) 2 diurnal (10 months) 2 daily (3 months jul25)
############################################################################################
# PROVIDE YEAR AND EXPERIMENT ID 
#  YEAR: YYYY (1999-2015)
#  ID  : MMMDD (jan-dec 1-31)
############################################################################################
set YEAR=$1
set ID=$2 

set DD = `echo $ID | cut -c4-5`

set ENSN=1
set JOBNAME = "${ID}_${YEAR}R$ENSN"
set NCOLLS=19

if ( $NCOLLS == 9 ) set ntape=200
if ( $NCOLLS == 9 ) set ndisk=114
if ( $NCOLLS == 14 ) set ntape=300
if ( $NCOLLS == 14 ) set ndisk=164
if ( $NCOLLS == 19 ) set ntape=400
if ( $NCOLLS == 19 ) set ndisk=236
set MM = 2
if ( $DD > 15 ) then
     set ndisk = 239
     set MM = 3
endif

if ( -e $GEOSS2S/runh/$YEAR/${ID}/ens$ENSN/cap_restartIC ) then
  set cap_sta1 = `cat  $GEOSS2S/runh/$YEAR/${ID}/ens$ENSN/cap_restartIC`
else
  echo  "NO EXPERIMENT SUBMITTED: $YEAR $ID $ENSN "
  exit
endif
echo 
echo "________________________________________________________________________________________"
echo JOB QUEUE STATUS:
qme | grep ${JOBNAME}

set cap_sta2 = `cat  $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/cap_restart`
set cap_sta3 = `cat  $GEOSS2S/runh/$YEAR/${ID}/ens$ENSN/CAP.rc | grep END_DATE | cut -c15-29`
echo
echo "_______________________________________________"
echo "____________DATE $YEAR $ID $ENSN"
echo "____________DATE START ${cap_sta1}"
echo "____________DATE DONE  ${cap_sta2}"
echo "____________DATE LAST  ${cap_sta3}"
echo "_______________________________________________"
@ nn = 0

@ afn = 0
echo 1- CHECK:
echo 1- IF ARCHIVE FILE NUMBER = 400: monthly 19x10+ diurnal 2x10 + daily 19x10
set afn1 = `ls -1 $ARCHIVE/GEOS_S2S/seasonal/Y$YEAR/${ID}/ens$ENSN/geosgcm_*/*nc4 | wc -l`
set afn2 = `ls -1 $ARCHIVE/GEOS_S2S/seasonal/Y$YEAR/${ID}/ens$ENSN/geosgcm*/*tar | wc -l`
@ afnn = $afn1 + $afn2
if ($afnn == $ntape) then
   @ nn = $nn + 1
   echo OK
else
   echo "   NOT READY: NUMBER FILES IN ARCHIVE IS: $afnn = $afn1 + $afn2"
endif
echo 2- CHECK:
echo "2- IF DISK FILE NUMBER ==   ${ndisk}: monthly 19x10+ diurnal 2x10 + daily subx 3x${MM}+ daily vis 2x10"
set mfnu1 = `ls -1 $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/geosgcm_*/*monthly*nc4 | wc -l`
set mfnu2 = `ls -1 $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/geosgcm_*/diurnal/*diurnal*nc4 | wc -l`
set mfnu3 = `ls -1 $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/geosgcm_*/*tar | wc -l`
@ mfnu = $mfnu1 + $mfnu2
@ mfnu = $mfnu + $mfnu3
if ($mfnu == $ndisk) then
   @ nn = $nn + 1
   echo OK
else
   echo "   NOT READY: NUMBER FILES ON DISK IS: $mfnu = $mfnu1 + $mfnu2 + $mfnu3"
endif
echo 3- CHECK:
echo 3- IF RUN IS FINISHED BASED ON cap_restart
set lcap = `cat $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/cap_restart | cut -c1-8`
set endd = `cat $GEOSS2S/runh/$YEAR/${ID}/ens$ENSN/CAP.rc | grep END_DATE | cut -c15-22`
set endh = `cat $GEOSS2S/runh/$YEAR/${ID}/ens$ENSN/CAP.rc | grep END_DATE | cut -c15-20`
if ($lcap == $endd) then
   @ nn = $nn + 1
   echo OK
else
   echo "   NOT READY: cap_restart VALUE NOT YET LAST: $lcap VS $endd"
   set runtag="${ID}_${YEAR}R$ENSN"
   qme | grep $runtag
   set qstat = $status
   if ($qstat  != 0) echo "   JOB $runtag IS NOT RUNNING"
endif
echo  4- CHECK:
echo "4- IF HOLDING CONTENT IS ONLY IN $endh"
set nhld1 = `ls -1 $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/holding/geos*/*/*nc4 | wc -l`
set qstat = $status
set nhld2 = `ls -1 $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/holding/geos*/$endh/*nc4 | wc -l`
if ( ! -e $GEOSS2S/runx/$YEAR/${ID}/ens$ENSN/holding) then
   echo "   NO ACTION NECESSARY"
   exit
endif
if ( ($qstat == 0) & ($nhld1 == $nhld2) ) then
   @ nn = $nn + 1
   echo OK
else
   echo "   NOT READY: HOLDING CONTENT: $nhld1 $nhld2"
endif
echo SUMMARY:
if ($nn == 4) then
   echo "   COMPLETED: ALL CONDITIONS MET TO CLEAN DISK"
else
   echo "   NOT READY: $nn CONDITIONS MET FOR CLEANING"
endif

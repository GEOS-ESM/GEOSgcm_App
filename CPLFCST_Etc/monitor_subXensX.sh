#!/bin/csh
#############################################
# Provide year and ensemble to monitor the run
# Number collections is set to constant 9
#############################################
set YEAR = $1
set ENSN=$2

set ncollections=9
set ncollections=19

ls -1 $GEOSS2S/util/submitted/ens${ENSN}gcm_setup${YEAR}*
set exists = $status
if ($exists > 0 ) then
   echo "$YEAR FOR ENSEMBLE $ENSN is not available for monitoring- either completed or not submitted, EXIT"
   exit
endif
set datesIC = `ls -1 $GEOSS2S/util/submitted/ens${ENSN}gcm_setup${YEAR}* | cut -c111-118`
if ( $ENSN > 9 ) set datesIC = `ls -1 $GEOSS2S/util/submitted/ens${ENSN}gcm_setup${YEAR}* | cut -c112-119`

echo "PROCESSING $datesIC"
@ arccount = 0

foreach dateIC ( $datesIC )

# get year
set yyIC = `echo $dateIC | cut -c1-4`
# get month
set mmIC = `echo $dateIC | cut -c5-6`
# get day
set ddIC = `echo $dateIC | cut -c7-8`
# get month name

if ($mmIC == '01') set mn = 'jan'
if ($mmIC == '02') set mn = 'feb'
if ($mmIC == '03') set mn = 'mar'
if ($mmIC == '04') set mn = 'apr'
if ($mmIC == '05') set mn = 'may'
if ($mmIC == '06') set mn = 'jun'
if ($mmIC == '07') set mn = 'jul'
if ($mmIC == '08') set mn = 'aug'
if ($mmIC == '09') set mn = 'sep'
if ($mmIC == '10') set mn = 'oct'
if ($mmIC == '11') set mn = 'nov'
if ($mmIC == '12') set mn = 'dec'

# check if archived
set stage = 0
set arcd = 0
if ( -e $GEOSS2S/runh/${yyIC}/${mn}${ddIC}/ens${ENSN}/ARC_YES${ncollections} ) set stage = $ncollections
if ( $stage > 0 ) then
  set arcd = `ls -1 $GEOSS2S/runx/${yyIC}/${mn}${ddIC}/ens${ENSN}/holding/ARC_YES_${mn}${ddIC}_geosgcm_* | wc -l`
  if (${arcd} == $stage ) goto PRINTSTATE
endif

# check what is running
set quelineR = `qme | grep $mn${ddIC}_${yyIC}R${ENSN} | wc -l`
set quelineA = `qme | grep $mn${ddIC}_${yyIC}A${ENSN} | wc -l`
set capline = `diff $GEOSS2S/runh/$yyIC/$mn$ddIC/ens$ENSN/cap_restartIC $GEOSS2S/runx/$yyIC/$mn$ddIC/ens$ENSN/cap_restart | wc -l`

if ( $quelineR == 0 ) then
   if ($capline > 0 ) set stage = 2
   if ($capline == 0 ) set stage = 0
else
   set stage = 1
endif

if ($quelineA > 0 ) set stage = 3

# print state of the runs
PRINTSTATE:
if ($stage > 3) then
   @ nndd = 45
   @ arccount = $arccount + 1
   set checkdu = `du -ck $GEOSS2S/runx/${yyIC}/${mn}${ddIC}/ens${ENSN}/holding/geosgcm_* | grep total   | cut -c1-9`
   echo "$ENSN $dateIC  - ARCHIVED ALL ${arcd} COLLECTIONS; DISK 13574928 = ${checkdu}"
else if ( $stage != 1 ) then
   set nnff = `ls -1 $GEOSS2S/runx/${yyIC}/${mn}${ddIC}/ens${ENSN}/holding/geosgcm_*/*nc4 | wc -l`
   @ nndd = $nnff / 25
endif
if ($stage == 3) echo "$ENSN $dateIC - ARCHIVING.... ; HOLDING HAS $nndd DAYS"
if ($stage == 2) echo "$ENSN $dateIC - RUN COMPLETED ; HOLDING HAS $nndd DAYS"
if ($stage == 0) echo "$ENSN $dateIC - CRASHED...... ; HOLDING HAS $nndd DAYS"
if ($stage == 1) then
   echo
   echo "$ENSN $dateIC - RUNNING...... "
   qme | grep $mn${ddIC}_${yyIC}R${ENSN}
endif

if ( -e $GEOSS2S/runx/${yyIC}/${mn}${ddIC}/ens${ENSN}/holding/ARC_NOT_${mn}${ddIC}_rst) then
   echo "$ENSN $dateIC - RESTARTS NOT MOVED TO DIRAC ? CHECK...."
   if ( -e $GEOSS2S/runx/${yyIC}/${mn}${ddIC}/ens${ENSN}/holding/ARC_YES_${mn}${ddIC}_rst) echo "$ENSN $dateIC - RESTARTS OK"
endif

end

if ( $arccount > 0 ) echo "${ENSN} ${YEAR}: ARCHIVED ${arccount} forecasts __________"
if ( $arccount == 0 ) echo "${ENSN} ${YEAR}: ARCHIVED NONE FOR THIS YEAR"

set RUNNING = `qme | grep '12:00 R' | wc -l`
set INQUEUE = `qme | grep '12:00 Q' | wc -l`
echo "TOTAL JOBS RUNNING : $RUNNING"
echo "TOTAL JOBS IN QUEUE: $INQUEUE"

exit

   

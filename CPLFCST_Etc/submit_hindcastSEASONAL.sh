#!/bin/csh

######################################################
#
# script to submit the series of seasonal hindcasts
# uses do_one_hindcast.sh, which provides IC locations
# Latter are modified as the assimilation progresses
#
######################################################

#########################################################
# PROVIDE FORECAST INITIALIZATION INFO 
# mon1, mon2 - start and end months [1 to 12];
# yr1, yr2   - start and end year   [1999 to 2015];
# DOs2s      - choice of the queue for the coupled run
#  2   - jobs submitted to gmaodev queue (only seasonal)
#  1   - jobs submitted to s2s preops queue
#  0   - jobs submitted to gmaofcst queue
#########################################################

@ mon1 = 4
@ mon2 = 4

@ yr1 = 1999
@ yr2 = 1999

set DOs2s = 1

#########################################################
# PROVIDE FORECAST EXECUTION PREFERENCE                  
# ENSEMBLE MEMBER:
#  1   - unperturbed member for seasonal forecast
#  2-4 - perturbed members  for subseasonal forecast
#  5   - unperturbed member for subseasonal forecast
# DOYEARS:
#  1   - execute forecasts initialized on mon1 for years yr1:yr2
#  0   - execute forecasts initialized on yr1  for months mon1:mon2
######################################################### 

set ENSEMBLE_MEMBER = 1
set DOYEARS = 1

if (${DOYEARS} == 0 ) then
@ yr2 = $yr1
@ nfcsts = ( $mon2 - $mon1 ) + 1
echo "SUBMITTING $nfcsts FOR MONTHS $mon1 TO $mon2"
else
@ mon2 = $mon1
@ nfcsts = ( $yr2 - $yr1 ) + 1
echo "SUBMITTING $nfcsts FOR YEARS $yr1 TO $yr2"
endif

@ n = 0
while( $n < $nfcsts )

@ MO = $n + $mon1
@ YR = $n + $yr1
if (${DOYEARS} == 0 ) @ YR = $yr1
if (${DOYEARS} == 1 ) @ MO = $mon1
if (${MO} < 10 ) set MN = 0${MO}
if (${MO} > 9 ) set MN = ${MO}

if ( $MO == 1 )  set DAYS="16 21 26 31"
if ( $MO == 2 )  set DAYS="10 15 20 25"
if ( $MO == 3 )  set DAYS="12 17 22 27"
if ( $MO == 4 )  set DAYS="11 16 21 26"
if ( $MO == 5 )  set DAYS="16 21 26 31"
if ( $MO == 6 )  set DAYS="15 20 25 30"
if ( $MO == 7 )  set DAYS="15 20 25 30"
if ( $MO == 8 )  set DAYS="14 19 24 29"
if ( $MO == 9 )  set DAYS="13 18 23 28"
if ( $MO == 10 ) set DAYS="13 18 23 28"
if ( $MO == 11 ) set DAYS="12 17 22 27"
if ( $MO == 12 ) set DAYS="12 17 22 27"

echo "SUBMITTING  $YR $MO FOR ${DAYS} ENSEMBLE ${ENSEMBLE_MEMBER}"

foreach DAY1 ( $DAYS )
 set FCSTDAT = ${YR}${MN}${DAY1}
 cd $GEOSS2S/util
 do_one_hindcast.sh $FCSTDAT ${ENSEMBLE_MEMBER} ${DOs2s}
 #EXAMPLE: fcst_start_Interim_s2s360.sh 20080814 1 0
 echo 'NOW SLEEP 15 SEC  ...'
 sleep 15
end

@ n = $n + 1
end 

date
echo " DONE $nfcsts MONTHS or YEARS"

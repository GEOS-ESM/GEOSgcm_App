#!/bin/csh

######################################################
#
# script to submit the series of seasonal forecasts
# uses subXdo_one_forecast.sh, which provides IC locations
#
######################################################

#########################################################
# PROVIDE FORECAST INITIALIZATION INFO 
# mon1, mon2 - start and end months [1 to 12];
# yr1, yr2   - start and end year   [1999 to 2015];
#########################################################
set DAYFCST = "17"

@ mon1 = 12
@ mon2 = 12

@ yr1 = 2017
@ yr2 = 2017

#########################################################
# PROVIDE FORECAST EXECUTION PREFERENCE                  
# ENSEMBLE MEMBER:
#  5   - unperturbed member for subseasonal forecast
#  2-4 - perturbed members for subseasonal forecast
# DOYEARS:
#  1   - execute forecasts initialized on mon1 for years yr1:yr2
#  0   - execute forecasts initialized on yr1  for months mon1:mon2
# DOs2s:
#  2   - jobs submitted to gmaodev queue
#  1   - jobs submitted to s2s queue
#  0   - jobs submitted to gmaofcst queue
######################################################### 

set ENSMEMBERS = "5 2 3 4"
set DOYEARS = 1
set DOs2s = 0

if (${DOYEARS} == 0 ) then
@ yr2 = $yr1
@ nfcsts = ( $mon2 - $mon1 ) + 1
echo "SUBMITTING $nfcsts FROM MONTH $mon1 TO $mon2"
else
@ mon2 = $mon1
@ nfcsts = ( $yr2 - $yr1 ) + 1
echo "SUBMITTING $nfcsts FROM YEAR $yr1 TO $yr2"
endif

@ n = 0
while( $n < $nfcsts )

@ MO = $n + $mon1
@ YR = $n + $yr1
if (${DOYEARS} == 0 ) @ YR = $yr1
if (${DOYEARS} == 1 ) @ MO = $mon1
if (${MO} < 10 ) set MN = 0${MO}
if (${MO} > 9 ) set MN = ${MO}

if ( $MO == 1 )  set DAYS="01 06 11 16 21 26 31"
if ( $MO == 2 )  set DAYS="05 10 15 20 25"
if ( $MO == 3 )  set DAYS="02 07 12 17 22 27"
if ( $MO == 4 )  set DAYS="01 06 11 16 21 26"
if ( $MO == 5 )  set DAYS="01 06 11 16 21 26 31"
if ( $MO == 6 )  set DAYS="05 10 15 20 25 30"
if ( $MO == 7 )  set DAYS="05 10 15 20 25 30"
if ( $MO == 8 )  set DAYS="04 09 14 19 24 29"
if ( $MO == 9 )  set DAYS="03 08 13 18 23 28"
if ( $MO == 10 ) set DAYS="03 08 13 18 23 28"
if ( $MO == 11 ) set DAYS="02 07 12 17 22 27"
if ( $MO == 12 ) set DAYS="02 07 12 17 22 27"

foreach DAY1 ( $DAYS )
if ( ${DAY1} == ${DAYFCST} ) then
 foreach ENSEMBLE_MEMBER ( $ENSMEMBERS )
 set FCSTDAT = ${YR}${MN}${DAY1}
 cd $GEOSS2S/util
 echo "SUBMITTING  $YR - $MO - ${DAY1} ENSEMBLE ${ENSEMBLE_MEMBER}"
 subXdo_one_forecast.sh $FCSTDAT ${ENSEMBLE_MEMBER} ${DOs2s}
 #EXAMPLE: fcst_start_Interim_s2s360.sh 20080814 1 0
 echo 'SLEEP 15 SEC  ...'
 sleep 15
 end
endif
end

@ n = $n + 1
end 

echo " DONE $nfcsts MONTHS or YEARS"

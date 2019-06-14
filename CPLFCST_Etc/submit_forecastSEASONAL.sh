#!/bin/csh

######################################################
#
# script to submit the series of seasonal forecasts
# uses do_one_forecast.sh, which provides IC locations
#
######################################################

#########################################################
# PROVIDE FORECAST INITIALIZATION INFO 
# mon1, mon2 - start and end months [1 to 12];
# yr1, yr2   - start and end year   [1999 to 2015];
#########################################################

@ mon1 = 12
@ mon2 = 12

@ yr1 = 2017
@ yr2 = 2017

#########################################################
# PROVIDE FORECAST EXECUTION PREFERENCE                  
# ENSEMBLE MEMBER:
#  1   - unperturbed member for seasonal forecast
#  6 and up - perturbed member for seasonal forecast
# DOYEARS:
#  1   - execute forecasts initialized on mon1 for years yr1:yr2
#  0   - execute forecasts initialized on yr1  for months mon1:mon2
# DOs2s:
#  2   - jobs submitted to gmaodev queue
#  1   - jobs submitted to s2s queue
#  0   - jobs submitted to gmaofcst queue
######################################################### 

set ENSEMBLE_MEMBER = 1
set DOYEARS = 1
set DOs2s = 0

if ( (${ENSEMBLE_MEMBER} > 1) & ( ${ENSEMBLE_MEMBER} < 6 ) ) then
   echo "ENSEMBLE NUMBERS 2-5 ARE RESERVED FOR SUB SEASONAL, EXIT"
   exit
endif
if ( $yr1 < 2016 ) then
   echo "YEAR FOR RUNNING FORECAST IS 2016 AND UP, EXIT"
   exit
endif

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
if ( ( $MO == 1 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="31"
if ( ( $MO == 2 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="25"
if ( ( $MO == 3 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="27"
if ( ( $MO == 4 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="26"
if ( ( $MO == 5 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="31"
if ( ( $MO == 6 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="30"
if ( ( $MO == 7 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="30"
if ( ( $MO == 8 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="29"
if ( ( $MO == 9 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="28"
if ( ( $MO == 10 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="28"
if ( ( $MO == 11 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="27"
if ( ( $MO == 12 ) & ( ${ENSEMBLE_MEMBER} > 5 ) )  set DAYS="27"

echo "SUBMITTING  $YR $MO FOR ${DAYS} ENSEMBLE ${ENSEMBLE_MEMBER}"

foreach DAY1 ( $DAYS )
 set FCSTDAT = ${YR}${MN}${DAY1}
 cd $GEOSS2S/util
 do_one_forecast.sh $FCSTDAT ${ENSEMBLE_MEMBER} ${DOs2s}
 #EXAMPLE: fcst_start_Interim_s2s360.sh 20080814 1 0
 echo 'SLEEP 15 SEC  ...'
 sleep 15

 @ multiens = 0
 if ( ${DAY1} > 26 ) @ multiens = 1 
 if ( ( $MO == 2 ) & ( ${DAY1} == 25 ) ) @ multiens = 1
 if ( ( $MO == 4 ) & ( ${DAY1} == 26) ) @ multiens = 1
 if ( ! -e $GEOSS2S/util/submitted/ens1gcm_setup$FCSTDAT ) @ multiens = 0
end

if ( $multiens == 1 ) then
 cd $GEOSS2S/util
 echo "TIME TO SUBMIT $FCSTDAT ens6 ${DOs2s} " 
 do_one_forecast.sh $FCSTDAT 6 ${DOs2s}
 echo 'SLEEP 15 MIN  ...'
 sleep 15m
 set PTRBM = "7 8 9 10 11"
 foreach PERTURBE_MEMBER ( $PTRBM )
    cd $GEOSS2S/util
    echo "TIME TO SUBMIT $FCSTDAT ens${PERTURBE_MEMBER} ${DOs2s} " 
    do_one_forecast.sh $FCSTDAT ${PERTURBE_MEMBER} ${DOs2s}
    echo 'SLEEP 1 MIN  ...'
    sleep 1m
 end
endif

@ n = $n + 1
end 

echo " DONE $nfcsts MONTHS or YEARS"

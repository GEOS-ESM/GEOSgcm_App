#!/bin/csh

######################################################
#
# script to submit the series of jobs 
# to clean the seasonal forecasts work directories
#
######################################################

### EDIT THIS SECTION ################################
### mon1, mon2 are start and end months [1 to 12];
### yr1, yr2   are start and end year   [1999 to 2015];

@ mon1 = 9
@ mon2 = 9

@ yr1 = 1999
@ yr2 = 1999

######################################################

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

echo "SUBMITTING  $YR $MO FOR ${DAYS} ENSEMBLE ${ENSEMBLE_MEMBER}"

foreach DAY1 ( $DAYS )
 set FCSTDAT = ${YR}${MN}${DAY1}
 cd $GEOSS2S/util
  fcst_clean_Interim_360.sh $FCSTDAT ${ENSEMBLE_MEMBER}
  #fcst_clean_Interim_360.sh 20080814 1
 sleep 15
end

@ n = $n + 1
end 

echo " DONE $nfcsts MONTHS or YEARS"

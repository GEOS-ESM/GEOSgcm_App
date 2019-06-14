#!/bin/csh 
#
#home/mathomp4/S2SScript/
set SLEEP_AMOUNT=3600

set CURRDATE=`(date +%Y-%m-%d)`

set CURR_YEAR=`(date +%Y)`
set CURR_MONTH=`(date +%m)`
set CURR_DAY=`(date +%d)`

set FORECAST_DATE_M1=`(date -d 'yesterday' +%Y-%m-%d)`
set FORECAST_DATE_MONWORD_M1=`(date -d 'yesterday' +%d-%b)`
set FORECAST_YM1=`(date -d 'yesterday' +%Y)`
set FORECAST_MM1=`(date -d 'yesterday' +%m)`
set FORECAST_DM1=`(date -d 'yesterday' +%d)`

set FORECAST_DATE_M2=`(date -d '2 days ago' +%Y-%m-%d)`
set FORECAST_DATE_MONWORD_M2=`(date -d '2 days ago' +%d-%b)`
set FORECAST_YM2=`(date -d '2 days ago' +%Y)`
set FORECAST_MM2=`(date -d '2 days ago' +%m)`
set FORECAST_DM2=`(date -d '2 days ago' +%d)`

set FORECAST_DATE_M3=`(date -d '3 days ago' +%Y-%m-%d)`
set FORECAST_DATE_MONWORD_M3=`(date -d '3 days ago' +%d-%b)`
set FORECAST_YM3=`(date -d '3 days ago' +%Y)`
set FORECAST_MM3=`(date -d '3 days ago' +%m)`
set FORECAST_DM3=`(date -d '3 days ago' +%d)`

echo "Execution Date: $CURRDATE"
echo "1 day  before : $FORECAST_DATE_M1"
echo "2 days before : $FORECAST_DATE_M2"

set FORECAST_DATE_FILE=/discover/nobackup/projects/gmao/m2oasf/aogcm/g5fcst/forecast/production/geos-s2s/util/subXs2sfcsts_dates.txt

# Check yesterday and day before yesterday
# ----------------------------------
@ day = 1
while ( $day <= 2 )
   echo -n "Checking if forecast should run... "
   if ($day == 1) then
   set FORECAST_DATE_MONWORD = ${FORECAST_DATE_MONWORD_M1}
   set FORECAST_Y = ${FORECAST_YM2}
   set FORECAST_M = ${FORECAST_MM2}
   set FORECAST_D = ${FORECAST_DM2}
   else
   set FORECAST_DATE_MONWORD = ${FORECAST_DATE_MONWORD_M2}
   set FORECAST_Y = ${FORECAST_YM3}
   set FORECAST_M = ${FORECAST_MM3}
   set FORECAST_D = ${FORECAST_DM3}
   endif

   # If the file exists, look for the date in it...
   # ----------------------------------------------
   grep -Fxq $FORECAST_DATE_MONWORD $FORECAST_DATE_FILE
   set GREPstatus = $status
   if  ( ${GREPstatus} == 0 ) then

      # The date was found
      # ------------------
      echo "$CURRDATE : Forecast date $FORECAST_DATE_MONWORD found."

      # Here is our command to check if files are ready from analysis
      # --------------------------------------------------------
      set S2S_CHECK_COMMAND="/discover/nobackup/projects/gmao/m2oasf/aogcm/g5fcst/forecast/production/geos-s2s/util/s2sfcsts_ic_check.py --year $FORECAST_Y --month $FORECAST_M --day $FORECAST_D"
      # Run a sleep loop until the command succeeds
      # -------------------------------------------
      @ counter    = 1
      @ counter_times = 20
      while ( $counter <= ${counter_times})
        ${S2S_CHECK_COMMAND}
        set ICstatus = $status
        if ( $ICstatus == 1 ) sleep $SLEEP_AMOUNT
        if ( $ICstatus == 0 ) then
           echo "IC available to submit subX forecast "
           /discover/nobackup/projects/gmao/m2oasf/aogcm/g5fcst/forecast/production/geos-s2s/util/subXsubmit_forecast.sh
           exit
        endif
        @ counter = $counter    + 1
      end
      echo "IC not available"
   else
      # The date was not found
      # ----------------------
      echo "$CURRDATE : Forecast date $FORECAST_DATE_MONWORD not found."
   endif
   @ day = $day + 1
end

exit

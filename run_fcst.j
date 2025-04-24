#!/bin/csh -f

setenv GEOSBIN /discover/nobackup/projects/gmao/osse2/GIT/GEOS-LM_v12-rc8/GEOSgcm/install-Aggressive-SLES15/bin

set FSEGMENT  = `grep '^\s*FCST_SEGMENT:' CAP.rc | cut -d: -f2`
if( $FSEGMENT != 00000000 ) then
     set REPLAY_BEG_DATE = `grep BEG_REPDATE: CAP.rc | cut -d':' -f2`
     set REPLAY_END_DATE = `grep END_REPDATE: CAP.rc | cut -d':' -f2`
     set nday  = `echo $FSEGMENT | cut -c8-9`
     set fcsts = `ls -r1 forecasts/gcm_CYCLED_REPLAY_forecast_P10800_C21600_T21600.j*`
     if ($fcsts[1] != '') then
     set nymdr = `echo $fcsts | cut -c60-67`
     set nhmsr = `echo $fcsts | cut -c69-74`
     set dater = `$GEOSBIN/tick $nymdr $nhmsr 43200`
     else
     set nymdr = `cat cap_restart | cut -c1-8`
     set nhmsr = `cat cap_restart | cut -c10-15`
     set dater = `$GEOSBIN/tick $nymdr $nhmsr 10800`
     endif
     set nymdz =  $dater[1]
     set nhmsz =  $dater[2]
     set nymdc = `cat cap_restart | cut -c1-8`
     set nhmsc = `cat cap_restart | cut -c10-15`
     set datec = `$GEOSBIN/tick $nymdc $nhmsc 10800`

     echo $dater $datec
     if ("$dater" == "$datec") then
     set yr4 = `echo $nymdz | cut -c1-4`
     set mo2 = `echo $nymdz | cut -c5-6`
     set hr4 = `echo $nhmsz | cut -c1-4`
     set ANA_EXPID  = `grep '^\s*REPLAY_ANA_EXPID:' AGCM.rc | cut -d: -f2`
     set ANA_LOCATION = "/discover/nobackup/projects/gmao/osse2/$ANA_EXPID"
     set ana_file = "$ANA_LOCATION/ana/Y${yr4}/M${mo2}/$ANA_EXPID.ana.eta.${nymdz}_${hr4}z.nc4"
     echo $ana_file
     if (-e $ana_file) then
     if( $nymdz >= ${REPLAY_BEG_DATE} & \
         $nymdz <= ${REPLAY_END_DATE} ) then
       cd forecasts
       if ( ($nhmsz == 000000) || ($nhmsz == 120000) ) then
                               set RUN = TRUE
##       if ($nhmsz == 120000) set RUN = FALSE
         if (! -e gcm_CYCLED_REPLAY_forecast_P10800_C21600_T21600.o${nymdz}_${nhmsz}z) then
     echo submit ./gcm_forecast.setup $nymdz $nhmsz $FSEGMENT $RUN
                 ./gcm_forecast.setup $nymdz $nhmsz $FSEGMENT $RUN
         endif
       else
               ##./gcm_forecast.setup $nymdz $nhmsz 00000001 TRUE
       endif
       cd ..
     endif
     endif
     endif
endif


#!/bin/csh -f

set SUBCASES = ('GFDL00_FP')

set FSEGMENT  = `grep '^\s*FCST_SEGMENT:' ../CAP.rc | cut -d: -f2`
#set fcsts = (`ls -1 BACM/gcm_CYCLED_REPLAY_forecast_P10800_C21600_T21600.j2023090[1-2,4]*`)
set fcsts = (`ls -1 ../holding/geosgcm_prog/*/*_00* | head -n -5`)
@ nf = 0
foreach fcst ( $fcsts )
   @ nf++
  #echo $fcst
   set nymdz = `echo $fcst | cut -d '.' -f 5 | cut -c1-8`
   set nhh   = `echo $fcst | cut -d '.' -f 5 | cut -c10-11`
   set nhmsz = ${nhh}0000
   foreach SUBCASE ($SUBCASES)   
   if ($nf >= 5) then
     echo $nymdz $nhmsz $nf
     set isRun = `grep COMM_TOTAL ${SUBCASE}/gcm_CYCLED_REPLAY_forecast_P10800_C21600_T21600.o${nymdz}_${nhmsz}z`
     if ($#isRun == 0) then
        /bin/rm -rf ${SUBCASE}/CYCLED_REPLAY_P10800_C21600_T21600_${nymdz}_${nhh}z
        /bin/rm -rf ${SUBCASE}/gcm_CYCLED_REPLAY_forecast_P10800_C21600_T21600.*${nymdz}_${nhmsz}z
        ./gcm_forecast.setup $nymdz $nhmsz $FSEGMENT $SUBCASE 40722606
     endif
    #if ($#isRun) then
    #   set hasStats = `grep '120 hr' ${SUBCASE}/CYCLED_REPLAY_P10800_C21600_T21600_${nymdz}_${nhh}z_STATS_GEOS-FP.o* | grep 'NH H'`
    #   if ($#hasStats == 0) then
    #      sbatch ${SUBCASE}/CYCLED_REPLAY_P10800_C21600_T21600_${nymdz}_${nhh}z/stats_GEOS-FP-${nymdz}_${nhh}z.j
    #   endif
    #endif
   endif
   end
   if ($nf == 9) exit 0
end

if (0) then

set jobID = 38820649
@ nf = 0
foreach SUBCASE ($SUBCASES)
set fcsts = (`ls -1 $SUBCASE/gcm_CYCLED_REPLAY_forecast_P10800_C21600_T21600.j*`)
foreach fcst ( $fcsts )
   setenv jobID0 `sbatch --parsable --dependency=afterany:${jobID} ${fcst}`
   echo jobIDs: $jobID $jobID0
   @ nf++
   if ($?jobID0 && ($nf == 4)) then
     set jobID = $jobID0
     @ nf = 0
   endif
end
end

endif

exit 0


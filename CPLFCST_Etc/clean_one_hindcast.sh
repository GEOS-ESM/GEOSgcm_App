######### Remove collections of YYYYYMMDD for ensemble ENS
######### assuming the data is archived if holding is removed during postprocessing
set ICDATE = $1
set ENS_MEMBER = $2 

echo "CLEANING FORECAST $ICDATE FOR ENSEMBLE MEMBER ${ENS_MEMBER}"
 
set YRI = `echo $ICDATE | cut -c1-4`
set MMI = `echo $ICDATE | cut -c5-6`
set DDI = `echo $ICDATE | cut -c7-8`

if ( $MMI == '01' ) set MMN='jan'
if ( $MMI == '02' ) set MMN='feb'
if ( $MMI == '03' ) set MMN='mar'
if ( $MMI == '04' ) set MMN='apr'
if ( $MMI == '05' ) set MMN='may'
if ( $MMI == '06' ) set MMN='jun'
if ( $MMI == '07' ) set MMN='jul'
if ( $MMI == '08' ) set MMN='aug'
if ( $MMI == '09' ) set MMN='sep'
if ( $MMI == '10' ) set MMN='oct'
if ( $MMI == '11' ) set MMN='nov'
if ( $MMI == '12' ) set MMN='dec'
set MD = $MMN$DDI

@ nmonths = 10
@ ncollec = 21
@ nfilesinhldg = 46
@ diracfilesmust = 2 * $ncollec + 3

set diracfiles1 = `ls -1 $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/geosgcm_*/*nc4 | wc -l`
set diracfiles2 = `ls -1 $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/geosgcm_*/*tar | wc -l`
@ diracfilesmon = ${diracfiles1} + ${diracfiles2}
@ diracfilesnum = ${diracfilesmust} * ${nmonths}
if ( ${diracfilesmon} == ${diracfilesnum} ) then
   echo "ARCHIVE HAS ALL DATA, CONTINUE"
else
   echo "CHECK DIRAC, NUMBER FILES DOES NOT EQUAL TO ${diracfilesnum}"
   exit
endif

chmod 755 $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}
chmod 755 $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}
chmod 755 $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/geosgcm_*
chmod 755 $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/restarts

set capicc8 = `cat $GEOSS2S/runh/${YRI}/${MD}/ens${ENS_MEMBER}/cap_restartIC | cut -c1-8`
set caprun8 = `cat $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/cap_restart | cut -c1-8`
set capend8 = `cat $GEOSS2S/runh/${YRI}/${MD}/ens${ENS_MEMBER}/CAP.rc | grep 'END_DATE' | cut -c15-22`

set capicc6 = `cat $GEOSS2S/runh/${YRI}/${MD}/ens${ENS_MEMBER}/cap_restartIC | cut -c1-6`
set caprun6 = `cat $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/cap_restart | cut -c1-6`
set capend6 = `cat $GEOSS2S/runh/${YRI}/${MD}/ens${ENS_MEMBER}/CAP.rc | grep 'END_DATE' | cut -c15-20`

#set COLLECTS = '6hravg 6hrins icecat ocn2d ocn3d seaice subxps surf vis2d'

if ( ${caprun8} < ${capicc8} ) exit
if ( ${caprun8} == ${capicc8} ) exit
if ( -e $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/DONE ) then
   echo FORECAST CLEANING COMPLETED, NO ACTION NEEDED, EXIT
   exit
endif

if ( ${caprun8} == ${capend8} ) then
   echo "RUN FINISHED for $ICDATE, PROCEED WITH CLEANING"
   touch $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/RUN_DONE
else
   echo "RUN NOT FINISHED YET for $ICDATE, EXIT"
   exit
endif

set ocmos = `ls -1 $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/MOM_Output/ocean_monthly*`
set nosmos = `ls -l $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/MOM_Output/ocean_monthly* | wc -l`
mkdir -p $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/MOM_Output
@ noc1 = 0
@ noc2 = 0
foreach ocmo ( $ocmos )
    echo "COPY $ocmo to archive..."
    scp -p $ocmo $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/MOM_Output
    set ocstatus = $status
    @ noc1 = $noc1 + $ocstatus
    @ noc2 = $noc2 + 1
end
if ( $noc1 > 0 ) then
   echo "MOM_output not archived, status = $noc1"
   exit
endif
if ( $noc2 < 9 ) then
   echo "MOM_output archived partially,files number = $noc2"
   exit
endif
sleep 10
ls -l $ARCHIVE/GEOS_S2S/seasonal/Y$YRI/${MD}/ens${ENS_MEMBER}/MOM_Output
echo MOM_Output archived, proceed

set nhld1 = `ls -1 $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/holding/geos*/*/*nc4 | wc -l`
set nhld2 = `ls -1 $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/holding/geos*/${capend6}/*nc4 | wc -l`
# 25= 4x(6hravg) + 4x(6hrins) + 1x(19-2 collections)
# for seasonal, daily is in ensN/geosgcm_C collection
# for seasonal we keep on disk all monthly, diurnal and 3 daily (3 months) + 2 daily (9 months) collections
echo "HOLDING $nhld1 $nhld2"

if ($nhld1 == $nhld2) then
        if ( $nhld1 != ${nfilesinhldg} ) exit
        if ( $nhld1 == ${nfilesinhldg} ) echo "HOLDING OK, PROCEED WITH CLEANING"
        /bin/rm -f $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/holding/geosgcm_*/${capend6}/${MD}.geosgcm_*.${capend6}01_???0z.nc4
        /bin/rmdir $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/holding/geosgcm_*/${capend6}
        /bin/rmdir $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/holding/geosgcm_*
        /bin/rmdir $GEOSS2S/runx/${YRI}/${MD}/ens${ENS_MEMBER}/holding

        echo REMOVE WORK FILES NOW...
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/archive
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/convert
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/forecasts
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/GEOSgcm.x
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/plot
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/post
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/RC
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/regress
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/RESTART
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/restarts
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/scratch
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/*_rst
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/vegdyn.data
        /bin/rm -rf $GEOSS2S/runx/$YRI/${MD}/ens${ENS_MEMBER}/MOM_Output
        mkdir -p $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/stdout
        /bin/mv $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/slurm* $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/stdout
        /bin/mv $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/RUN_DONE $GEOSS2S/runh/$YRI/${MD}/ens${ENS_MEMBER}/DONE
else
   echo "ERROR: HOLDING HAS DATA: $nhld1 $nhld2 , EXIT"
   exit
endif

echo "SUCCESS: DISK CLEANED FOR $ICDATE"

exit

#!/bin/csh -fx

#######################################################################
#                     Batch Parameters for Archive Job
#######################################################################

#SBATCH -t @ARCHIVE_T
#@ARCHIVE_P
#SBATCH --job-name=@ARCHIVE_N
#@ARCHIVE_Q
#@BATCH_GROUP
#SBATCH -o @ARCHIVE_N.o

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

@SETENVS

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE        @SITE
setenv GEOSBIN     @GEOSBIN

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib
module list

#######################################################################
#                         Archive Commands           
#######################################################################

setenv    EXPID   @EXPID
setenv    EXPDIR  @EXPDIR
setenv    HOMDIR  @HOMDIR

set capyr = `cat $HOMDIR/cap_restartIC | cut -c1-4`

#set collections = 'geosgcm_00zins geosgcm_6hravg geosgcm_6hrins geosgcm_aero geosgcm_gwd geosgcm_ices2s geosgcm_icethm geosgcm_int geosgcm_moist geosgcm_ocn2d geosgcm_ocn3d geosgcm_rad geosgcm_seaice geosgcm_subxps geosgcm_surf geosgcm_tend geosgcm_turb geosgcm_vis2 geosgcm_vis3d'

set collections = ''
foreach line ("`cat $HOMDIR/HISTORY.rc`")
   set firstword  = `echo $line | awk '{print $1}'`
   set firstchar  = `echo $firstword | cut -c1`
   set secondword = `echo $line | awk '{print $2}'`

   if ( $firstword == "::" ) goto done

   if ( $firstchar != "#" ) then
      set collection  = `echo $firstword | sed -e "s/'//g"`
      set collections = `echo $collections $collection`
      if ( $secondword == :: ) goto done
   endif

   if ( $firstword == COLLECTIONS: ) then
      set collections = `echo $secondword | sed -e "s/'//g"`
   endif
end
done: 
set ncollections = `echo $collections | wc -w`
@ npass = 0
@ nfail = 0

#check dirac availability
ssh dirac mkdir -p @ARCHIVE_D
set diracstat = $status
if ( $diracstat > 0 ) then
touch $HOMDIR/ARC_NOT${ncollections}
exit
endif

ssh dirac chmod -R 755 @ARCHIVE_D

foreach collection ( $collections )

set success = TRUE
@   npasses = 3
cd $EXPDIR/holding/$collection

@ n = 1
while( $n <= $npasses )
 tar cvf ${EXPID}.$collection.daily.$capyr.nc4.tar ${EXPID}.$collection.*nc4
 set tarstatus=$status
 if (${tarstatus} > 0) then
       echo "TAR Attempt "$n ... FAIL!
       echo /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
       if( $n == $npasses ) set success = FALSE
       @ n = $n + 1
 else
       @ n = 999
 endif
 wait
end
if( $success == FALSE ) goto ARCHSTATE

ssh dirac mkdir -p @ARCHIVE_D
echo " "
echo 'Archiving:  '${EXPID}.$collection.daily.$capyr.nc4.tar

@ n = 1
while( $n <= $npasses )
bbscp -z ${EXPID}.$collection.daily.$capyr.nc4.tar dirac:@ARCHIVE_D | set archive_status = `sed -e "s/ / /g"`
if ( .$archive_status[4] == .OK ) then
echo "SCP Attempt "$n ... PASS 
@ n = 999
else
echo "SCP Attempt "$n ... FAIL!
if( $n == $npasses ) set success = FALSE
@ n = $n + 1
endif
end
unset  archive_status
wait

ARCHSTATE:
if( $success == TRUE ) then
    if ($collection == 'geosgcm_00zins') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_6hravg') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    #if ($collection == 'geosgcm_6hrins') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_aero') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_gwd') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    #if ($collection == 'geosgcm_ices2s') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_icethm') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_int') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_moist') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_ocn2d') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_ocn3d') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_rad') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_seaice') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    #if ($collection == 'geosgcm_subxps') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_surf') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_tend') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    if ($collection == 'geosgcm_turb') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
    #if ($collection == 'geosgcm_vis2d') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar 
    if ($collection == 'geosgcm_vis3d') /bin/rm -f ${EXPID}.$collection.daily.$capyr.nc4.tar
echo "   Successfully Archived:  "$EXPDIR/holding/$collection 
echo "   To:                     "@ARCHIVE_D
     touch $EXPDIR/holding/ARC_YES_${EXPID}_$collection
     @ npass = $npass + 1
else
echo "   FAILED to Archive:  "$EXPDIR/holding/$collection
     touch $EXPDIR/holding/ARC_NOT_${EXPID}_$collection
     @ nfail = $nfail + 1
endif

end

cd $EXPDIR/restarts
bbscp -z restarts.e*tar dirac:@ARCHIVE_D | set archive_status = `sed -e "s/ / /g"`
if ( .$archive_status[4] == .OK ) then
   echo "SCP restarts PASS"
   touch $EXPDIR/holding/ARC_YES_${EXPID}_rst
else
   @ nfail = $nfail + 1
   touch $EXPDIR/holding/ARC_NOT_${EXPID}_rst
endif
unset  archive_status
wait

ARCHCLEAN:
# Clean all but holding and cap restart if all data is archived
if ($nfail   == 0 && $npass == $ncollections) then
   /bin/rm -f $EXPDIR/*rst
   /bin/rm -f $EXPDIR/GEOSgcm.x
   /bin/rm -f $EXPDIR/vegdyn.data
   /bin/rm -f $EXPDIR/linkbcs
   /bin/rm -rf $EXPDIR/convert
   /bin/rm -rf $EXPDIR/[f-gm-s]*
   /bin/rm -rf $EXPDIR/[MR]*

   /bin/rm -f $EXPDIR/holding/geosgcm_00zins/${EXPID}.geosgcm_00zins.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_6hravg/${EXPID}.geosgcm_6hravg.*nc4
   if ( -e $EXPDIR/holding/geosgcm_6hrins/${EXPID}.geosgcm_6hrins.daily.$capyr.nc4.tar ) /bin/rm -f $EXPDIR/holding/geosgcm_6hrins/${EXPID}.geosgcm_6hrins.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_aero/${EXPID}.geosgcm_aero.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_gwd/${EXPID}.geosgcm_gwd.*nc4
   if ( -e $EXPDIR/holding/geosgcm_ices2s/${EXPID}.geosgcm_ices2s.daily.$capyr.nc4.tar ) /bin/rm -f $EXPDIR/holding/geosgcm_ices2s/${EXPID}.geosgcm_ices2s.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_icethm/${EXPID}.geosgcm_icethm.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_int/${EXPID}.geosgcm_int.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_moist/${EXPID}.geosgcm_moist.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_ocn2d/${EXPID}.geosgcm_ocn2d.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_ocn3d/${EXPID}.geosgcm_ocn3d.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_rad/${EXPID}.geosgcm_rad.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_seaice/${EXPID}.geosgcm_seaice.*nc4
   if ( -e $EXPDIR/holding/geosgcm_subxps/${EXPID}.geosgcm_subxps.daily.$capyr.nc4.tar ) /bin/rm -f $EXPDIR/holding/geosgcm_subxps//${EXPID}.geosgcm_subxps.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_surf/${EXPID}.geosgcm_surf.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_tend/${EXPID}.geosgcm_tend.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_turb/${EXPID}.geosgcm_turb.*nc4
   if ( -e $EXPDIR/holding/geosgcm_vis2d/${EXPID}.geosgcm_vis2d.daily.$capyr.nc4.tar ) /bin/rm -f $EXPDIR/holding/geosgcm_vis2d/${EXPID}.geosgcm_vis2d.*nc4
   /bin/rm -f $EXPDIR/holding/geosgcm_vis3d/${EXPID}.geosgcm_vis3d.*nc4

   /bin/rmdir $EXPDIR/holding/geosgcm_*

   touch $HOMDIR/ARC_YES${ncollections}
else
   touch $HOMDIR/ARC_NOT${ncollections}
endif

exit

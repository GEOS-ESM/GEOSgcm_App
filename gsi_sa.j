#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#SBATCH --time=02:00:00
#SBATCH --ntasks=192 --ntasks-per-node=32
#SBATCH --job-name=gsi_sa
#SBATCH --output=gsi_sa_%j
#SBATCH --error=gsi_sa_%j
#SBATCH --constraint=sky
#SBATCH --qos=chmdev
#SBATCH --account=s1866
#@BATCH_NAME -o gcm_run.o@RSTDATE

#######################################################################
#                         System Settings 
#######################################################################

umask 022

limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

# Do NO2 assimilation 
set ASSIM_NO2 = 0

setenv ARCH `uname`

setenv EXPID    @EXPID 
setenv FVROOT   @FVROOT 
setenv TOOLSDIR @TOOLSDIR

setenv FVBIN           $FVROOT/bin
setenv RUN_CMDGSI      "$FVROOT/bin/esma_mpirun -np "

setenv  EXPDIR `pwd`
setenv  SCRDIR  $EXPDIR/scratch_gsi
setenv  GEOSDIR $EXPDIR/scratch
setenv  ANADIR  $EXPDIR/analyze
setenv  SPLDIR  $EXPDIR/analyze/spool
setenv  BKGDIR  $EXPDIR/holding_bkg

if (! -e $SCRDIR) mkdir -p $SCRDIR
if (! -e $SPLDIR) mkdir -p $SPLDIR
if (! -e $BKGDIR) mkdir -p $BKGDIR

# manually add inc.eta collection for data assimilation
if (! -e $EXPDIR/holding/ana2inc ) mkdir -p $EXPDIR/holding/ana2inc
if (! -e $EXPDIR/ods ) mkdir -p $EXPDIR/ods

# gsi checkpoint file names
set gsi_restart_file = "${GEOSDIR}/gsi_restart.runme"
set geos_done = "${GEOSDIR}/geos_run.done"

source $FVROOT/bin/g5_modules
module load comp/intel/19.1.3.304
module load mpi/impi/19.1.3.304
module load mpi/impi-prov/19.1.3.304

cd $SCRDIR
/bin/rm -rf *

# ---------------
# main while loop 
# ---------------
while ( ! -f $geos_done ) 

 # -----------------------------
 # run GSI if it's time to do so
 # -----------------------------
 if ( -e $gsi_restart_file ) then

  # move into GSI scratch directory
  cd $SCRDIR
  /bin/rm -rf *
  /bin/ln -sf . Obsloc

  /bin/cp -f  $EXPDIR/GSIsa.x   .
  /bin/cp -f  $EXPDIR/analyze/ana2inc.x .
  /bin/cp -f  $EXPDIR/analyze/RC/* .
  /bin/cp -f  $EXPDIR/analyze/*.rc .

  # python script to do omi postprocessing
  if ( $ASSIM_NO2 == 1 ) then
     /bin/cp -f $TOOLSDIR/omno2_post.py .
  endif

  # get time stamps from gsi_restart checkpoint file 
  set edate = e`cat $gsi_restart_file | cut -c1-8`_`cat $gsi_restart_file | cut -c10-13`z
  echo "gsi restart file found - running GSI for $edate"

  set nymdc = `cat $gsi_restart_file | cut -c1-8`
  set nhmsc = `cat $gsi_restart_file | cut -c10-15`
  set nymde = `cat $GEOSDIR/CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c2-9`
  set nhmse = `cat $GEOSDIR/CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c11-16`

  # Compute Time Variables at the Finish_(f) of current segment
  @ dt = 3600 * 6
  set nymdf = $nymdc
  set nhmsf = $nhmsc
  set date  = `$FVBIN/tick $nymdf $nhmsf $dt`
  set nymdf =  $date[1]
  set nhmsf =  $date[2]
  
  echo "finish of segment"
  echo $nymdf
  echo $nhmsf

  # Compute Time Variables at the Middle_(m) of current segment
  @ dt = 3600 * 3
  set nymdm = $nymdc
  set nhmsm = $nhmsc
  set date  = `$FVBIN/tick $nymdm $nhmsm $dt`
  set nymdm =  $date[1]
  set nhmsm =  $date[2]

  echo "middle of segment"
  echo $nymdm
  echo $nhmsm

  set hourc  = `echo $nhmsc | cut -c1-2`
  set hourm  = `echo $nhmsm | cut -c1-2`
  set hourf  = `echo $nhmsf | cut -c1-2`

  # Check for initial background files. These might be missing if 
  # doing a cold-start run.
  # eventually get left shoulder background files from restart file
  if ( ! -e ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4 ) then
    if ( -e ${GEOSDIR}/codas_background_rst ) then
       tar xf ${GEOSDIR}/codas_background_rst
       /bin/mv  bkg.eta.nc4 ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4
       /bin/mv  bkg.sfc.nc4 ${GEOSDIR}/$EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4
       /bin/mv cbkg.eta.nc4 ${GEOSDIR}/$EXPID.cbkg.eta.${nymdc}_${hourc}00z.nc4
    endif 
  endif

  # if background restart file is still missing, skip GSI analysis
  if ( -e ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4 ) then
   set do_gsi = 1
  else
   set do_gsi = 0 
  endif

  # run GSI
  if ( $do_gsi == 1 ) then

   # Create RC files for DAS from codas.rc and templates
   set fname = "codas_setup1.ctmp"
   sed -e "s/@EXPID/$EXPID/" $EXPDIR/analyze/codas_setup1.csh >! $fname
   chmod 755 $fname
   source ./$fname
 
   # Update GSI_GridComp.rc to have correct time
   set fname = "codas_setup2.ctmp"
   sed -e "s/@BEG_DATE/$nymdc $nhmsc/; s/@END_DATE/$nymdf $nhmsf/; \
           s/@RECORD_REF_TIME/$nhmsm/; s/@RECORD_REF_DATE/$nymdm/" \
          $EXPDIR/analyze/codas_setup2.csh >! $fname
   chmod 755 $fname
   ./$fname
 
   # Acquire observation files
   set fname = "codas_acqobs1.jtmp"
   sed -e "s/@ACQOBS_O/codas_acqobs1.log.o%j/" $EXPDIR/analyze/codas_acqobs.j >! $fname
   echo "acquire_obsys -v -d $SCRDIR -s $SPLDIR -ssh -e 999 ${nymdm} ${nhmsm} 060000 4 $OBSCLASS" >> $fname
   chmod 755 $fname
   sbatch --wait ./$fname
 
   set tmrw  = `$FVBIN/tick $nymdm $nhmsm 86400`
   set nymdt =  $tmrw[1]
   set nhmst =  $tmrw[2]
 
   set fname = "codas_acqobs2.jtmp"
   sed -e "s/@ACQOBS_O/codas_acqobs2.log.o%j/" $EXPDIR/analyze/codas_acqobs.j >! $fname
   echo "acquire_obsys -v -d $SPLDIR -s $SPLDIR -ssh -e 999 ${nymdt} ${nhmst} 060000 4 $OBSCLASS" >> $fname
   chmod 755 $fname
   sbatch ./$fname
 
   set NX_GSI = `grep NX: GSI_GridComp.rc | cut -d':' -f2`
   set NY_GSI = `grep NY: GSI_GridComp.rc | cut -d':' -f2`
   @ NPES_GSI = $NX_GSI * $NY_GSI

   # Get background files for CoDAS
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4  . 
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4  .
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdc}_${hourc}00z.nc4 .
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdm}_${hourm}00z.nc4  . 
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdm}_${hourm}00z.nc4  .
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdm}_${hourm}00z.nc4 .
   /bin/cp ${GEOSDIR}/$EXPID.ana.eta.${nymdm}_${hourm}00z.nc4  . 
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4  . 
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4  .
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdf}_${hourf}00z.nc4 .

   # For testing, also write to temporary 'background' directory
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4  ${BKGDIR} 
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4  ${BKGDIR} 
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdc}_${hourc}00z.nc4 ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdm}_${hourm}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdm}_${hourm}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdm}_${hourm}00z.nc4 ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.ana.eta.${nymdm}_${hourm}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdf}_${hourf}00z.nc4 ${BKGDIR}
 
   # Run GSIsa.x
   /bin/rm -f  GSI_EGRESS
   $RUN_CMDGSI $NPES_GSI ./GSIsa.x
   if( -e GSI_EGRESS ) then
    set rc = 0
   else
    set rc = -1
   endif
   echo GSIsa Run Status: $rc
   if( $rc == -1 ) exit

   # Compute analysis increment
   /bin/cp $EXPID.cbkg.eta.${nymdm}_${hourm}00z.nc4 cbkg.eta.nc4
   /bin/cp $EXPID.bkg.eta.${nymdm}_${hourm}00z.nc4   bkg.eta.nc4
   /bin/cp $EXPID.ana.eta.${nymdm}_${hourm}00z.nc4   ana.eta.nc4
   /bin/cp ana.eta.nc4 inc.eta.nc4
   ./ana2inc.x

   # Copy increment file to holding directory and rename to full date stamp for proper reference
   #/bin/cp ana.eta.nc4 $EXPDIR/holding/ana2inc/$EXPID.ana.eta.${nymdm}_${hourm}00z.nc4
   #/bin/cp inc.eta.nc4 $EXPDIR/holding/ana2inc/$EXPID.inc.eta.${nymdm}_${hourm}00z.nc4
   /usr/local/other/nco/5.0.1/bin/ncks -4 -L 1 ana.eta.nc4 $EXPDIR/holding/ana2inc/$EXPID.ana.eta.${nymdm}_${hourm}00z.nc4
   /usr/local/other/nco/5.0.1/bin/ncks -4 -L 1 inc.eta.nc4 $EXPDIR/holding/ana2inc/$EXPID.inc.eta.${nymdm}_${hourm}00z.nc4
   /bin/mv inc.eta.nc4 $GEOSDIR/$EXPID.inc.eta.${nymdm}_${hourm}00z.nc4

   # Accumulate statistics and diagnostics 
   cat fort.206 fort.216 > $EXPID.ana_stats.log.${nymdm}_${hourm}z.txt
   $FVROOT/bin/gsidiags ${nymdm} ${nhmsm} $EXPID set -rc gsidiags.rc
   $FVROOT/bin/diag2ods ${nymdm} ${nhmsm} $EXPID
   # copy ods files to ods directory
   set odsfiles = `/bin/ls -1 *.ods`
   foreach odsfile ($odsfiles)
    /bin/cp $odsfile $EXPDIR/ods
   end
   if ( -e ods.log ) then
    /bin/cp ods.log $EXPDIR/ods/ods.log.${nymdm}_${hourm}z
   endif

   # Clean up pe files
   /bin/rm -f pe*.obs_setup
   foreach collection ( diag_conv $OBSDIAGS )
    set coltag = `echo $collection | cut -d'_' -f2-`
    /bin/rm -f pe*.${coltag}_??
   end

   # Create analysis NO2 file for use by GEOS: this includes the nearest observation hour for each location with an increment
   if ( $ASSIM_NO2 == 1 ) then
    set datestring = "${nymdm}_${hourm}00z"
    set satfile = "omno2.${nymdm}.t${hourm}z.nc"
    set outfile = "ana_no2.after_gsi.${nymdm}_${hourm}00z.nc4"
    /usr/local/other/python/GEOSpyD/2019.03_py3.7/2019-04-22/bin/python omno2_post.py -d $datestring -a ana.eta.nc4 -b cbkg.eta.nc4 -s $satfile -o $outfile
    /bin/cp $outfile $GEOSDIR/$outfile
   endif

  else
   echo "skip GSI because first background restart file is missing"
   set rc = 0
  endif # do GSI

  # Save background files at final step for next cycle
  # --------------------------------------------------
  if ( $rc == 0 ) then
    /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4   bkg.eta.nc4
    /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4   bkg.sfc.nc4
    /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdf}_${hourf}00z.nc4 cbkg.eta.nc4

    tar cf ${GEOSDIR}/codas_background_checkpoint \
           bkg.eta.nc4 bkg.sfc.nc4 cbkg.eta.nc4

    # Move a copy of the updated ana.eta file to a specified run directory
    /bin/cp $EXPID.ana.eta.${nymdm}_${hourm}00z.nc4 ${GEOSDIR}/$EXPID.ana.eta.after_gsi.${nymdm}_${hourm}00z.nc4

  endif

  # write output file, copy to GEOS run directory
  #sleep 20
  set ofile = "gsi_done.${edate}"
  echo $nymdc $nhmsc > $ofile
  /bin/mv $ofile $GEOSDIR/ 
  /bin/mv $gsi_restart_file ${GEOSDIR}/gsi_restart.completed.$edate
  echo "GSI done - file written: $ofile"

 # ------------------------------------------------------------
 # if not doing GSI, just pause for 30 seconds then check again
 # ------------------------------------------------------------
 else
  echo 'go to sleep for 30 seconds...'
  sleep 30
 endif

 # the following if else statement is not strictly necessary, add it here mostly for testing
 if ( -e ${GEOSDIR}/${geos_done} ) then
  echo 'found file indicating that GEOS is done - leaving now'
  break
 else
  echo 'GEOS does not seem to be done yet, go back to beginning of while loop...'
 endif
end

echo 'All done'
exit


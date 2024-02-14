#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#SBATCH --time=06:00:00
#SBATCH --ntasks=320 --ntasks-per-node=80
#SBATCH --job-name=CFv2_gsi
#SBATCH --output=CFv2_gsi_%j
#SBATCH --constraint=mil
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

setenv ARCH `uname`

setenv EXPID @EXPID

setenv  EXPDIR `pwd`
setenv  FVROOT  $EXPDIR/analyze
setenv  SCRDIR  $EXPDIR/scratch_gsi
setenv  GEOSDIR $EXPDIR/scratch
setenv  ANADIR  $EXPDIR/analyze
setenv  SPLDIR  $EXPDIR/analyze/spool
setenv  BKGDIR  $EXPDIR/holding_bkg
setenv  RSTDIR  $EXPDIR/cdas_restarts
setenv  FVBUILD @FVBUILD 
#setenv  TOOLSDIR /discover/nobackup/cakelle2/CDAS/analyze/omno2/omno2

setenv FVBIN          $FVROOT/bin
setenv RUN_CMDGSI      "$FVROOT/bin/esma_mpirun -np "

if (! -e $SCRDIR) mkdir -p $SCRDIR
if (! -e $SPLDIR) mkdir -p $SPLDIR
if (! -e $BKGDIR) mkdir -p $BKGDIR
if (! -e $RSTDIR) mkdir -p $RSTDIR

# manually add inc.eta collection for data assimilation
if (! -e $EXPDIR/holding/ana2inc ) mkdir -p $EXPDIR/holding/ana2inc
if (! -e $EXPDIR/ods ) mkdir -p $EXPDIR/ods
if (! -e $EXPDIR/diags_nc ) mkdir -p $EXPDIR/diags_nc

# also add collection to hold the analysis files
#if (! -e $EXPDIR/holding/ana_no2_after_gsi ) /bin/mkdir -p $EXPDIR/holding/ana_no2_after_gsi

# gsi checkpoint file names
set gsi_restart_file = "${GEOSDIR}/gsi_restart.runme"
set geos_done = "${GEOSDIR}/geos_run.done"

#### Experiment environment
unsetenv LD_LIBRARY_PATH
source $FVROOT/bin/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${FVBUILD}/lib

# milan stuff
# Turn off warning about TMPDIR on NFS
setenv OMPI_MCA_shmem_mmap_enable_nfs_warning 0
# pre-connect MPI procs on mpi_init
setenv OMPI_MCA_mpi_preconnect_all 1
setenv OMPI_MCA_coll_tuned_bcast_algorithm 7
setenv OMPI_MCA_coll_tuned_scatter_algorithm 2
setenv OMPI_MCA_coll_tuned_reduce_scatter_algorithm 3
setenv OMPI_MCA_coll_tuned_allreduce_algorithm 3
setenv OMPI_MCA_coll_tuned_allgather_algorithm 4
setenv OMPI_MCA_coll_tuned_allgatherv_algorithm 3
setenv OMPI_MCA_coll_tuned_gather_algorithm 1
setenv OMPI_MCA_coll_tuned_barrier_algorithm 0
# required for a tuned flag to be effective
setenv OMPI_MCA_coll_tuned_use_dynamic_rules 1
# disable file locks
setenv OMPI_MCA_sharedfp "^lockedfile,individual"

# Move into GSI scratch directory
# -------------------------------
cd $SCRDIR
/bin/rm -rf *

# Initialize variables
set nymdf = "99999999"
set hourf = "99"

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
#  /bin/cp -f  $EXPDIR/analyze/ana2inc.x .
  /bin/cp -f  $EXPDIR/analyze/ana2inc.py .
#  /bin/cp -f  $EXPDIR/analyze/RC/* .
#  /bin/cp -f  $EXPDIR/analyze/*.rc .
  /bin/cp -f  $EXPDIR/analyze/codas.rc .
  /bin/cp -f  $EXPDIR/analyze/etc/* .

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
  if ( ! -e ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4 ) then
    if ( -e ${RSTDIR}/codas_background_rst.${nymdc}_${hourc}00z.tar ) then
       echo "extracting codas background files from ${RSTDIR}/codas_background_rst.${nymdc}_${hourc}00z.tar..."
       tar xvf ${RSTDIR}/codas_background_rst.${nymdc}_${hourc}00z.tar
       if ( -e $EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4 ) then
        /bin/mv  $EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4 ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4
       else
        echo "Warning: file not found: $EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4"
       endif
       if ( -e $EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4 ) then
        /bin/mv $EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4 ${GEOSDIR}/$EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4
       else
        echo "Warning: file not found: $EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4"
       endif
       if ( -e $EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4 ) then
        /bin/mv $EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4 ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4
       else
        echo "Warning: file not found: $EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4"
       endif
    endif 
  endif

  # if background restart file is still missing, skip GSI analysis
  if ( -e ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4 ) then
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
#   set fname = "codas_acqobs1.jtmp"
#   sed -e "s/@ACQOBS_O/codas_acqobs1.log.o%j/" $EXPDIR/analyze/codas_acqobs.j >! $fname
#   echo "acquire_obsys -v -d $SCRDIR -s $SPLDIR -ssh -e 999 ${nymdm} ${nhmsm} 060000 4 $OBSCLASS" >> $fname
#   chmod 755 $fname
#   sbatch --wait ./$fname
   set fname = "codas_acqobs1.jtmp"
   sed -e "s?@ACQOBS_O?codas_acqobs1.log.o%j?; s?@FVROOT?$FVROOT?" $EXPDIR/analyze/codas_acqobs.j >! $fname
   echo "acquire_obsys -v -d $SCRDIR -s $SPLDIR -ssh -e 999 ${nymdm} ${nhmsm} 060000 4 $OBSCLASS" >> $fname
   chmod 755 $fname
   sbatch --wait ./$fname
 
   set tmrw  = `$FVBIN/tick $nymdm $nhmsm 86400`
   set nymdt =  $tmrw[1]
   set nhmst =  $tmrw[2]
 
#   set fname = "codas_acqobs2.jtmp"
#   sed -e "s/@ACQOBS_O/codas_acqobs2.log.o%j/" $EXPDIR/analyze/codas_acqobs.j >! $fname
#   echo "acquire_obsys -v -d $SPLDIR -s $SPLDIR -ssh -e 999 ${nymdt} ${nhmst} 060000 4 $OBSCLASS" >> $fname
#   chmod 755 $fname
#   sbatch ./$fname

   set fname = "codas_acqobs2.jtmp"
   sed -e "s?@ACQOBS_O?codas_acqobs2.log.o%j?; s?@FVROOT?$FVROOT?" $EXPDIR/analyze/codas_acqobs.j >! $fname
   echo "acquire_obsys -v -d $SPLDIR -s $SPLDIR -ssh -e 999 ${nymdt} ${nhmst} 060000 4 $OBSCLASS" >> $fname
   chmod 755 $fname
   sbatch ./$fname
 
   # Get background files for CoDAS
   # Note: for the cbkg field at the beginning of the time window, use the 'corrected' fields generated during 
   # the previous predictor step
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4  . 
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4  .
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4 $EXPID.cbkg.eta.${nymdc}_${hourc}00z.nc4 
#   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdc}_${hourc}00z.nc4 .
#   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.corrector.${nymdc}_${hourc}00z.nc4  $EXPID.bkg.eta.${nymdc}_${hourc}00z.nc4 
#   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.corrector.${nymdc}_${hourc}00z.nc4  $EXPID.bkg.sfc.${nymdc}_${hourc}00z.nc4 
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
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdc}_${hourc}00z.nc4 ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdm}_${hourm}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdm}_${hourm}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdm}_${hourm}00z.nc4 ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.ana.eta.${nymdm}_${hourm}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4  ${BKGDIR}
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdf}_${hourf}00z.nc4 ${BKGDIR}

#   # Get bias coefficients: Copy pre-fetched files in exp dir if not yet locally available
#   if (! -e satbias_ang.in ) /bin/cp $EXPDIR/satbias/satbang satbias_ang.in
#   if (! -e satbias_angle  ) /bin/cp $EXPDIR/satbias/satbang satbias_angle
#   if (! -e satbias_in     ) /bin/cp $EXPDIR/satbias/satbias satbias_in
#   if (! -e satbias_pc     ) /bin/cp $EXPDIR/satbias/satbiaspc satbias_pc

#   # Datasets needed for radiances assimilation
#   /bin/ln -s gmao_global_convinfo.rc convinfo
#   /bin/ln -sf /discover/nobackup/projects/gmao/obsdev/bkarpowi/x46a_tst//fvInput/gsi/etc/fix_ncep20210525/REL-2.2.3-r60152_local-rev_5/CRTM_Coeffs/Little_Endian CRTM_Coeffs
#
#   # for radiance assimilation
#   setenv MKSI_SIDB $EXPDIR/gmao_satinfo.db/
 
   # the observation files now need to be linked outside of GSI (see /discover/nobackup/cakelle2/CDAS/code/GEOSadas-5_29_3/GEOSadas/install/bin/GEOSdas.csm @L941)
   $FVROOT/bin/match_obcls_obsys.pl ${nymdm} ${nhmsm} GSI_GridComp.rc gsiparm.anl
 
   # Run GSIsa.x
   /bin/rm -f  GSI_EGRESS
   #$RUN_CMDGSI $NPES_GSI ./GSIsa.x
   #$RUN_CMDGSI 192 ./GSIsa.x
   #esma_mpirun -perhost 8 -np 240 ./GSIsa.x 
   mpirun -np 320 ./GSIsa.x 
   if( -e GSI_EGRESS ) then
    set rc = 0
   else
    set rc = -1
   endif
   echo GSIsa Run Status: $rc
   if( $rc == -1 ) exit

   # deal with satbias files
#   if (! -e $EXPDIR/satbias/${nymdm}_${hourm}z ) mkdir -p $EXPDIR/satbias/${nymdm}_${hourm}z
#   /bin/cp satbias_angle  $EXPDIR/satbias/${nymdm}_${hourm}z/satbang_${nymdm}_${hourm}z 
#   /bin/cp satbias_out    $EXPDIR/satbias/${nymdm}_${hourm}z/satbias_out_${nymdm}_${hourm}z 
#   /bin/cp satbias_pc.out $EXPDIR/satbias/${nymdm}_${hourm}z/satbiaspc.out_${nymdm}_${hourm}z 
#   /bin/mv satbias_out satbias_in
#   /bin/mv satbias_pc.out satbias_pc

   # Compute analysis increment
   /bin/cp $EXPID.cbkg.eta.${nymdm}_${hourm}00z.nc4 cbkg.eta.nc4
   /bin/cp $EXPID.bkg.eta.${nymdm}_${hourm}00z.nc4   bkg.eta.nc4
   /bin/cp $EXPID.ana.eta.${nymdm}_${hourm}00z.nc4   ana.eta.nc4
   #/bin/cp ana.eta.nc4 inc.eta.nc4
   #./ana2inc.x
   /usr/local/other/GEOSpyD/23.5.2-0_py3.11/2023-11-02/bin/python ana2inc.py -a ana.eta.nc4 -b bkg.eta.nc4 -c cbkg.eta.nc4 -o inc.eta.nc4

   # Copy increment file to holding directory and rename to full date stamp for proper reference
   /bin/cp ana.eta.nc4 $EXPDIR/holding/ana2inc/$EXPID.ana.eta.${nymdm}_${hourm}00z.nc4
   /bin/cp inc.eta.nc4 $EXPDIR/holding/ana2inc/$EXPID.inc.eta.${nymdm}_${hourm}00z.nc4
   /bin/mv inc.eta.nc4 $GEOSDIR/$EXPID.inc.eta.${nymdm}_${hourm}00z.nc4

   # Accumulate statistics and diagnostics 
   # TODO: something doesn't work yet here...
   if( 0 ) then
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
   else
    echo "skip ods statistics for now..."
   endif

   # move netcdf diagnostics files to output directory
   if ( 1 ) then
    if ( -e $EXPDIR/diags_nc/${nymdm}_${hourm}z ) then
     /bin/rm -r $EXPDIR/diags_nc/${nymdm}_${hourm}z
    endif
    mkdir -p $EXPDIR/diags_nc/${nymdm}_${hourm}z
    set pefiles = `/bin/ls -1 pe*.nc4`
    foreach pefile ($pefiles)
     /bin/mv $pefile $EXPDIR/diags_nc/${nymdm}_${hourm}z
    end
   endif

   # Clean up pe files
   /bin/rm -f pe*.obs_setup
   foreach collection ( diag_conv $OBSDIAGS )
    set coltag = `echo $collection | cut -d'_' -f2-`
    /bin/rm -f pe*.${coltag}_??
   end

   # this is for NO2/SO2
   # Create analysis NO2 file for use by GEOS: this includes the nearest observation hour for each location with an increment
#   set datestring = "${nymdm}_${hourm}00z"
#   #set satfile = "omi_minds_no2.${nymdm}.t${hourm}z.nc"
#   set satfile = "omno2.${nymdm}.t${hourm}z.nc"
#   set outfile = "ana_no2.after_gsi.${nymdm}_${hourm}00z.nc4"
#   /usr/local/other/python/GEOSpyD/2019.03_py3.7/2019-04-22/bin/python ${TOOLSDIR}/omno2_post.py -d $datestring -a ana.eta.nc4 -b cbkg.eta.nc4 -s $satfile -v 'NO2' -on 'ColumnAmountNO2' -o $outfile
#   /bin/cp $outfile $GEOSDIR/$outfile
#   /bin/cp $outfile $EXPDIR/holding/ana_no2_after_gsi

   # this is for O3
   # Move a copy of the updated ana.eta file to a specified run directory
   /bin/cp $EXPID.ana.eta.${nymdm}_${hourm}00z.nc4 ${GEOSDIR}/$EXPID.ana.eta.after_gsi.${nymdm}_${hourm}00z.nc4

  else
   echo "skip GSI because first background restart file is missing"
   set rc = 0
  endif # do GSI

  # Save background files at final step for next cycle
  # keep for legacy reason, not used anymore
  # --------------------------------------------------
  if ( -e ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4 ) then
    /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4   bkg.eta.nc4
    /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4   bkg.sfc.nc4
    /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.${nymdf}_${hourf}00z.nc4 cbkg.eta.nc4
    tar cf ${GEOSDIR}/codas_background_checkpoint \
           bkg.eta.nc4 bkg.sfc.nc4 cbkg.eta.nc4
  endif


  # write output file, copy to GEOS run directory
  sleep 10
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

 # Save background files at final step for next cycle
 # --------------------------------------------------
 if ( -e ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdf}_${hourf}00z.nc4 ) then
 if ( -e ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4 ) then
 if ( -e ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4 ) then
   /bin/cp ${GEOSDIR}/$EXPID.cbkg.eta.corrector.${nymdf}_${hourf}00z.nc4 .
   /bin/cp ${GEOSDIR}/$EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4 .
   /bin/cp ${GEOSDIR}/$EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4 .
   echo "write codas_background_restart file: ${RSTDIR}/codas_background_rst.${nymdf}_${hourf}00z.tar"
   tar cf ${RSTDIR}/codas_background_rst.${nymdf}_${hourf}00z.tar \
          $EXPID.bkg.eta.${nymdf}_${hourf}00z.nc4 $EXPID.bkg.sfc.${nymdf}_${hourf}00z.nc4 $EXPID.cbkg.eta.corrector.${nymdf}_${hourf}00z.nc4
 endif
 endif
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


#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#SBATCH --time=04:00:00
#SBATCH --ntasks=240 --ntasks-per-node=30
#SBATCH --job-name=gsi_cdas
#SBATCH --output=gsi_cdas_%j
#SBATCH --error=gsi_cdas_%j
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

setenv ARCH `uname`

setenv EXPID <<<EXPID>>>

setenv FVROOT        /discover/nobackup/cakelle2/CDAS/code/GEOSadas-5_29_3/GEOSadas/install 
setenv FVBIN          $FVROOT/bin
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
if (! -e $EXPDIR/diags_nc ) mkdir -p $EXPDIR/diags_nc

# gsi checkpoint file names
set gsi_restart_file = "${GEOSDIR}/gsi_restart.runme"
set geos_done = "${GEOSDIR}/geos_run.done"

# Experiment environment
# Not sure how much of the following is really needed...
# ----------------------
  setenv GID s1866
  setenv group_list "SBATCH --account=$GID"
  setenv ARCH `uname -s`
  setenv HOST `uname -n`
  setenv NCPUS       384       # Number of CPUs to run GCM
  setenv NCPUS_IDF   96   # Number of CPUs to run IDF
  setenv NCPUS_IAU   96   # Number of CPUs to run IAU
  setenv NCPUS_GSI   240   # Number of CPUs to run GSI
  setenv NCPUS_GPERT 216 # Number of CPUs to run gcmPERT
  setenv NCPUS_AOD   1   # Number of CPUs to run PSAS-AOD
  setenv GAAS_RUN_SLURM 1 # launch AOD analysis as separate batch job
  setenv AODBLOCKJOB 1
  setenv NCPUS_PER_NODE 30
  setenv NODES      sky
  setenv N_CPU   $NCPUS
  setenv CASE    $EXPID  # experiment ID (for LSM's sake)

  unsetenv LD_LIBRARY_PATH
  source $FVROOT/bin/g5_modules
  setenv LD_LIBRARY_PATH ${BASEDIR}/${ARCH}/lib:${FVROOT}/lib:${LD_LIBRARY_PATH}

# MPI/OMP specific environment variables
# --------------------------------------
  setenv NUMBER_CPUS_IN_MACHINE      $NCPUS    # Total No CPU on this queue
  setenv OMP_NUM_THREADS             $NCPUS    # Number OMP Threads (generic)
  setenv OMP_SET_NUMTHREADS          1         # allows dynamic change of #threads
  setenv PSAS_NUM_MPI                1 # 24      # No. MPI processes for PSAS
  setenv PSAS_NUM_THREADS            1 # 16      # No. OMP threads   for PSAS
  setenv AGCM_N_PROCESSES            24      # N_PROC X N_THREADS = NCPUS
  setenv AGCM_N_THREADS_PER_PROCESS  16
  setenv AGCM_NUM_MPI                $AGCM_N_PROCESSES           # No. MPI processes for GCM
  setenv AGCM_NUM_THREADS            $AGCM_N_THREADS_PER_PROCESS # No. OMP threads   for GCM
  setenv OMP_NUM_THREADS             $AGCM_N_THREADS_PER_PROCESS # Number OMP Threads (generic)

  setenv MKL_CBWR SSE4_2

# MVAPICH variables
# -----------------
  if ($?MVAPICH) then
     setenv MV2_DEFAULT_TIME_OUT 23
     setenv MV2_USE_SHMEM_ALLREDUCE 0
     setenv MV2_ON_DEMAND_THRESHOLD 8192
     setenv MV2_USE_UD_HYBRID 0
     setenv MV2_USE_SHMEM_COLL      0
     setenv MV2_USE_UD_HYBRID       0
     setenv MV2_HYBRID_MAX_RC_CONN 16
     setenv MV2_SHOW_ENV_INFO 2
  endif

# The following are from OPS parallel

  if ($?I_MPI_ROOT) then
#    The following as recommended by Scott and SI Team
#    setenv I_MPI_USE_DYNAMIC_CONNECTIONS 0
     setenv I_MPI_SHM_HEAP_VSIZE   512
     setenv PSM2_MEMORY            large
     setenv I_MPI_ADJUST_GATHERV   3
     setenv I_MPI_ADJUST_ALLREDUCE 12
#    setenv I_MPI_EXTRA_FILESYSTEM 1
#    setenv I_MPI_EXTRA_FILESYSTEM_LIST gpfs
#    setenv ROMIO_FSTYPE_FORCE         "gpfs:"
#    setenv I_MPI_FABRICS shm:dapl
#    setenv I_MPI_FABRICS_LIST "dapl,ofa"
#    setenv I_MPI_FALLBACK "enable"
#    setenv I_MPI_MPD_RSH sshmpi
#    setenv I_MPI_DAPL_CHECK_MAX_RDMA_SIZE 1
#    setenv I_MPI_DAPL_UD_SEND_BUFFER_NUM 4096
#    setenv I_MPI_DAPL_UD_RECV_BUFFER_NUM 4096
#    setenv I_MPI_DAPL_UD_ACK_SEND_POOL_SIZE 4096
#    setenv I_MPI_DAPL_UD_ACK_RECV_POOL_SIZE 4096
#    setenv I_MPI_DAPL_UD_RNDV_EP_NUM 2
#    setenv I_MPI_DAPL_UD_REQ_EVD_SIZE 2000
  endif
  if ($?MPT_VERSION) then
     setenv DAPL_UCM_CQ_SIZE 4096
     setenv DAPL_UCM_QP_SIZE 4096
     setenv DAPL_UCM_REP_TIME 2000
     setenv DAPL_UCM_RTU_TIME 2000
     setenv DAPL_UCM_RETRY 7
     setenv DAPL_ACK_RETRY 7
     setenv DAPL_ACK_TIMER 20
     setenv DAPL_UCM_RETRY 10
     setenv DAPL_ACK_RETRY 10
# MPT env variables
# -----------------
     setenv MPI_COLL_REPRODUCIBLE
     setenv MPI_DISPLAY_SETTINGS
     setenv MPI_COMM_MAX  1024
     setenv MPI_GROUP_MAX 1024
     setenv MPI_BUFS_PER_PROC 1024
     setenv MPI_IB_TIMEOUT 23
     setenv MPI_XPMEM_ENABLE no
     setenv MPI_NUM_MEMORY_REGIONS 0
     setenv SUPRESS_XPMEM_TRIM_THRESH 1
  endif
  setenv SLURM_DISTRIBUTION block

# For some reason, PMI_RANK is randomly set and interferes
# with binarytile.x and other executables.
  unsetenv PMI_RANK

# Needed for TLM/ADM
# ------------------
  setenv N_SMP                        $NCPUS   # number of CPUS
  setenv N_MPI                        $NCPUS   # number of MPI processes
  setenv NUMBER_MLP_PROCESSES         ${N_MPI} # still used by GCM
  setenv NUMBER_CPUS_PER_MLP_PROCESS  ${N_SMP} # still used by GCM

# Add FVROOT/bin to front of path so fvDAS binaries are found first
# -----------------------------------------------------------------
  if ( `uname -s` == "Linux" ) then
    set path = ( . $FVROOT/bin $SHARE/dasilva/opengrads/Contents $BASEDIR/$ARCH/bin $path )
  else
    set path = ( . $FVROOT/bin $SHARE/dasilva/opengrads/Contents $path )
  endif

  setenv OMP_NUM_THREADS 1

# Discover specific configuration
# ----------------------------
  limit stacksize unlimited
  limit coredumpsize 0
  setenv KMP_STACKSIZE    450m
  unsetenv F_UFMTENDIAN
  setenv KMP_LIBRARY turnaround

# Node list
# ------------------------------
  if (`uname -n` =~ borg*) then
   cat $PBS_NODEFILE > $SCRDIR/ANA_list
  endif
  set PBS_NODEFILE=SCRDIR/ANA_list

# Move into GSI scratch directory
# -------------------------------
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
 
#   set NX_GSI = `grep NX: GSI_GridComp.rc | cut -d':' -f2`
#   set NY_GSI = `grep NY: GSI_GridComp.rc | cut -d':' -f2`
#   @ NPES_GSI = $NX_GSI * $NY_GSI

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

   # Get bias coefficients: Copy pre-fetched files in exp dir if not yet locally available
   if (! -e satbias_ang.in ) /bin/cp $EXPDIR/satbias/satbang satbias_ang.in
   if (! -e satbias_angle  ) /bin/cp $EXPDIR/satbias/satbang satbias_angle
   if (! -e satbias_in     ) /bin/cp $EXPDIR/satbias/satbias satbias_in
   if (! -e satbias_pc     ) /bin/cp $EXPDIR/satbias/satbiaspc satbias_pc

   # Datasets needed for radiances assimilation
   /bin/ln -s gmao_global_convinfo.rc convinfo
   /bin/ln -sf /discover/nobackup/projects/gmao/obsdev/bkarpowi/x46a_tst//fvInput/gsi/etc/fix_ncep20210525/REL-2.2.3-r60152_local-rev_5/CRTM_Coeffs/Little_Endian CRTM_Coeffs

   # for radiance assimilation
   setenv MKSI_SIDB $EXPDIR/gmao_satinfo.db/

   # the observation files now need to be linked outside of GSI (see /discover/nobackup/cakelle2/CDAS/code/GEOSadas-5_29_3/GEOSadas/install/bin/GEOSdas.csm @L941)
   $FVROOT/bin/match_obcls_obsys.pl ${nymdm} ${nhmsm} GSI_GridComp.rc gsiparm.anl
 
   # Run GSIsa.x
   /bin/rm -f  GSI_EGRESS
   #$RUN_CMDGSI $NPES_GSI ./GSIsa.x
   #$RUN_CMDGSI 192 ./GSIsa.x
   #esma_mpirun -perhost 8 -np 240 ./GSIsa.x 
   esma_mpirun -np 240 ./GSIsa.x 
   if( -e GSI_EGRESS ) then
    set rc = 0
   else
    set rc = -1
   endif
   echo GSIsa Run Status: $rc
   if( $rc == -1 ) exit

   # deal with satbias files
   if (! -e $EXPDIR/satbias/${nymdm}_${hourm}z ) mkdir -p $EXPDIR/satbias/${nymdm}_${hourm}z
   /bin/cp satbias_angle  $EXPDIR/satbias/${nymdm}_${hourm}z/satbang_${nymdm}_${hourm}z 
   /bin/cp satbias_out    $EXPDIR/satbias/${nymdm}_${hourm}z/satbias_out_${nymdm}_${hourm}z 
   /bin/cp satbias_pc.out $EXPDIR/satbias/${nymdm}_${hourm}z/satbiaspc.out_${nymdm}_${hourm}z 
   /bin/mv satbias_out satbias_in
   /bin/mv satbias_pc.out satbias_pc

   # Compute analysis increment
   /bin/cp $EXPID.cbkg.eta.${nymdm}_${hourm}00z.nc4 cbkg.eta.nc4
   /bin/cp $EXPID.bkg.eta.${nymdm}_${hourm}00z.nc4   bkg.eta.nc4
   /bin/cp $EXPID.ana.eta.${nymdm}_${hourm}00z.nc4   ana.eta.nc4
   /bin/cp ana.eta.nc4 inc.eta.nc4
   ./ana2inc.x

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
    /bin/mv pe*.nc4 $EXPDIR/diags_nc/${nymdm}_${hourm}z
   endif

   # Clean up pe files
   /bin/rm -f pe*.obs_setup
   foreach collection ( diag_conv $OBSDIAGS )
    set coltag = `echo $collection | cut -d'_' -f2-`
    /bin/rm -f pe*.${coltag}_??
   end

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


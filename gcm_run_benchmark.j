#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#SBATCH --time=06:00:00
#SBATCH --nodes=@NODES
#SBATCH --tasks-per-node=@TASKS_PER_NODE
#SBATCH --job-name=@EXPID
#SBATCH --output=@BENCH_DIR/%x.o%j
#SBATCH --nice --no-requeue
#SBATCH @SYSTEM

#######################################################################
#                         System Settings
#######################################################################

umask 022

limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSDIR          @GEOSDIR
setenv GEOSBIN          @GEOSBIN
setenv GEOSETC          @GEOSETC
setenv GEOSUTIL         @GEOSSRC

source $GEOSBIN/g5_modules
setenv @LD_LIBRARY_PATH_CMD ${LD_LIBRARY_PATH}:${GEOSDIR}/lib
# We only add BASEDIR to the @LD_LIBRARY_PATH_CMD if BASEDIR is defined (i.e., not running with Spack)
if ( $?BASEDIR ) then
    setenv @LD_LIBRARY_PATH_CMD ${@LD_LIBRARY_PATH_CMD}:${BASEDIR}/${ARCH}/lib
endif

setenv RUN_CMD "@RUN_CMD"

setenv GCMVER `cat $GEOSETC/.AGCM_VERSION`
echo   VERSION: $GCMVER

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################


setenv  EXPID   @EXPID
setenv  EXPDIR  @BENCH_DIR/@EXPID
setenv  HOMDIR  @BENCH_DIR/@EXPID

setenv  RSTDATE @RSTDATE
setenv  GCMEMIP @GCMEMIP

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################

if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot

if( $GCMEMIP == TRUE ) then
    if (! -e $EXPDIR/restarts/$RSTDATE ) mkdir -p $EXPDIR/restarts/$RSTDATE
    setenv  SCRDIR  $EXPDIR/scratch.$RSTDATE
else
    setenv  SCRDIR  $EXPDIR/scratch
endif

set STATS_ONLY = @STATS_ONLY
if ($STATS_ONLY == 0) then

if (! -e $SCRDIR ) mkdir -p $SCRDIR

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep '^\s*NX:'             $HOMDIR/AGCM.rc | cut -d: -f2`
set       NY  = `grep '^\s*NY:'             $HOMDIR/AGCM.rc | cut -d: -f2`
set  AGCM_IM  = `grep '^\s*AGCM_IM:'        $HOMDIR/AGCM.rc | cut -d: -f2`
set  AGCM_JM  = `grep '^\s*AGCM_JM:'        $HOMDIR/AGCM.rc | cut -d: -f2`
set  AGCM_LM  = `grep '^\s*AGCM_LM:'        $HOMDIR/AGCM.rc | cut -d: -f2`
set  OGCM_IM  = `grep '^\s*OGCM\.IM_WORLD:' $HOMDIR/AGCM.rc | cut -d: -f2`
set  OGCM_JM  = `grep '^\s*OGCM\.JM_WORLD:' $HOMDIR/AGCM.rc | cut -d: -f2`

@COUPLED set  OGCM_LM  = `grep '^\s*OGCM\.LM:'       $HOMDIR/AGCM.rc | cut -d: -f2`
@COUPLED set       NX  = `grep '^\s*OGCM\.NX:'       $HOMDIR/AGCM.rc | cut -d: -f2`
@COUPLED set       NY  = `grep '^\s*OGCM\.NY:'       $HOMDIR/AGCM.rc | cut -d: -f2`

# Calculate number of cores/nodes for IOSERVER
# --------------------------------------------

set USE_IOSERVER   = @USE_IOSERVER
set AGCM_IOS_NODES = `grep '^\s*IOSERVER_NODES:' $HOMDIR/AGCM.rc | cut -d: -f2`

if ($USE_IOSERVER == 0) then
   set IOS_NODES = 0
else
   set IOS_NODES = $AGCM_IOS_NODES
endif

# Check for Over-Specification of CPU Resources
# ---------------------------------------------
if ($?SLURM_NTASKS) then
   set  NCPUS = $SLURM_NTASKS
else if ($?PBS_NODEFILE) then
   set  NCPUS = `cat $PBS_NODEFILE | wc -l`
else
   set  NCPUS = NULL
endif

@ MODEL_NPES = $NX * $NY
@ NCPUS_PER_NODE = @TASKS_PER_NODE

if ( $NCPUS != NULL ) then

   if ( $USE_IOSERVER == 1 ) then

      @ NODES  = `echo "( ($MODEL_NPES + $NCPUS_PER_NODE) + ($AGCM_IOS_NODES * $NCPUS_PER_NODE) - 1)/$NCPUS_PER_NODE" | bc`
      @ NPES   = $NODES * $NCPUS_PER_NODE

      if( $NPES > $NCPUS ) then
         echo "CPU Resources are Over-Specified"
         echo "--------------------------------"
         echo "Allotted  NCPUs: $NCPUS"
         echo "Requested NCPUs: $NPES"
         echo ""
         echo "Specified NX: $NX"
         echo "Specified NY: $NY"
         echo ""
         echo "Specified IOSERVER_NODES: $AGCM_IOS_NODES"
         echo "Specified cores per node: $NCPUS_PER_NODE"
         exit
      endif

   else

      @ NPES = $MODEL_NPES

      if( $NPES > $NCPUS ) then
         echo "CPU Resources are Over-Specified"
         echo "--------------------------------"
         echo "Allotted  NCPUs: $NCPUS"
         echo "Requested NCPUs: $NPES"
         echo ""
         echo "Specified NX: $NX"
         echo "Specified NY: $NY"
         exit
      endif

   endif

else
   # This is for the desktop path

   @ NPES = $MODEL_NPES

endif

#######################################################################
#                       GCMEMIP Setup
#######################################################################

if( $GCMEMIP == TRUE & ! -e $EXPDIR/restarts/$RSTDATE/cap_restart ) then

cd $EXPDIR/restarts/$RSTDATE

cp $HOMDIR/CAP.rc CAP.rc.orig
awk '{$1=$1};1' < CAP.rc.orig > CAP.rc

set year  = `echo $RSTDATE | cut -d_ -f1 | cut -b1-4`
set month = `echo $RSTDATE | cut -d_ -f1 | cut -b5-6`

@EMIP_OLDLAND# Copy MERRA-2 Restarts
@EMIP_OLDLAND# ---------------------
@EMIP_NEWLAND# Copy Jason-3_4 REPLAY MERRA-2 NewLand Restarts
@EMIP_NEWLAND# ----------------------------------------------
cp /discover/nobackup/projects/gmao/g6dev/ltakacs/@EMIP_MERRA2/restarts/AMIP/M${month}/restarts.${year}${month}.tar .
tar xf  restarts.${year}${month}.tar
/bin/rm restarts.${year}${month}.tar
@EMIP_OLDLAND/bin/rm MERRA2*bin


@EMIP_OLDLAND# Regrid MERRA-2 Restarts
@EMIP_OLDLAND# -----------------------
@EMIP_NEWLAND# Regrid Jason-3_4 REPLAY MERRA-2 NewLand Restarts
@EMIP_NEWLAND# ------------------------------------------------
set RSTID = `/bin/ls *catch* | cut -d. -f1`
set day   = `/bin/ls *catch* | cut -d. -f3 | awk 'match($0,/[0-9]{8}/) {print substr($0,RSTART+6,2)}'`
$GEOSBIN/regrid.pl -np -ymd ${year}${month}${day} -hr 21 -grout C${AGCM_IM} -levsout ${AGCM_LM} -outdir . -d . -expid $RSTID -tagin @EMIP_BCS_IN -oceanin e -i -nobkg -lbl -nolcv -tagout @LSMBCS -rs 3 -oceanout @OCEANOUT
@EMIP_OLDLAND/bin/rm $RSTID.*.bin

     set IMC = $AGCM_IM
if(     $IMC < 10 ) then
     set IMC = 000$IMC
else if($IMC < 100) then
     set IMC = 00$IMC
else if($IMC < 1000) then
     set IMC = 0$IMC
endif

set  chk_type = `/usr/bin/file -Lb --mime-type C${AGCM_IM}[cef]_${RSTID}.*catch*`
if( "$chk_type" =~ "application/octet-stream" ) set ext = bin
if( "$chk_type" =~ "application/x-hdf"        ) set ext = nc4

$GEOSBIN/stripname C${AGCM_IM}@OCEANOUT_${RSTID}.
$GEOSBIN/stripname .${year}${month}${day}_21z.$ext.@LSMBCS_@BCSTAG.@ATMOStag_@OCEANtag
@EMIP_OLDLAND/bin/mv gocart_internal_rst gocart_internal_rst.merra2
@EMIP_OLDLAND$GEOSBIN/gogo.x -s $RSTID.Chem_Registry.rc.${year}${month}${day}_21z -t $EXPDIR/RC/Chem_Registry.rc -i gocart_internal_rst.merra2 -o gocart_internal_rst -r C${AGCM_IM} -l ${AGCM_LM}


# Create CAP.rc and cap_restart
# -----------------------------
set   nymd = ${year}${month}${day}
set   nhms = 210000
echo $nymd $nhms > cap_restart

set curmonth = $month
      @ count = 0
while( $count < 4 )
       set date  = `$GEOSBIN/tick $nymd $nhms 86400`
       set nymd  =  $date[1]
       set nhms  =  $date[2]
       set year  = `echo $nymd | cut -c1-4`
       set month = `echo $nymd | cut -c5-6`
       if( $curmonth != $month ) then
        set curmonth  = $month
             @ count  = $count + 1
       endif
end
set oldstring =  `grep '^\s*END_DATE:' CAP.rc`
set newstring =  "END_DATE: ${year}${month}01 210000"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
/bin/rm CAP.tmp

endif

#######################################################################
#   Move to Scratch Directory and Copy RC Files from Home Directory
#######################################################################

cd $SCRDIR
/bin/rm -rf *
                             cp -f  $EXPDIR/RC/* .
                             cp     $EXPDIR/cap_restart .
                             cp -f  $HOMDIR/*.rc .
                             cp -f  $HOMDIR/*.nml .
                             cp -f  $HOMDIR/*.yaml .
                             cp     $GEOSBIN/bundleParser.py .

                             cat fvcore_layout.rc >> input.nml

                             @MOM6cp -f  $HOMDIR/MOM_input .
                             @MOM6cp -f  $HOMDIR/MOM_override .

if( $GCMEMIP == TRUE ) then
    cp -f  $EXPDIR/restarts/$RSTDATE/cap_restart .
    cp -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
endif

set END_DATE  = `grep '^\s*END_DATE:'     CAP.rc | cut -d: -f2`
set NUM_SGMT  = `grep '^\s*NUM_SGMT:'     CAP.rc | cut -d: -f2`
set FSEGMENT  = `grep '^\s*FCST_SEGMENT:' CAP.rc | cut -d: -f2`
set USE_SHMEM = `grep '^\s*USE_SHMEM:'    CAP.rc | cut -d: -f2`


#######################################################################
#              Create HISTORY Collection Directories
#######################################################################

set collections = ''
foreach line ("`cat HISTORY.rc`")
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
   foreach collection ( $collections )
      if (! -e $EXPDIR/$collection )         mkdir $EXPDIR/$collection
      if (! -e $EXPDIR/holding/$collection ) mkdir $EXPDIR/holding/$collection
   end

#######################################################################
#                        Link Boundary Datasets
#######################################################################

setenv BCSDIR    @BCSDIR
@DATAOCEAN setenv SSTDIR    @SSTDIR
setenv CHMDIR    @CHMDIR
@DATAOCEAN setenv BCRSLV    @ATMOStag_@OCEANtag
@COUPLED setenv BCRSLV    @ATMOStag_DE0360xPE0180
setenv DATELINE  DC
setenv EMISSIONS @EMISSIONS

@MOM5setenv ABCSDIR  @COUPLEDIR/atmosphere_bcs/@LSMBCS/MOM5/@ATMOStag_@OCEANtag
@MOM5setenv OBCSDIR  @shared_COUPLED/ocean/MOM5/${OGCM_IM}x${OGCM_JM}
@MOM6setenv ABCSDIR  @COUPLEDIR/atmosphere_bcs/@LSMBCS/MOM6/@ATMOStag_@OCEANtag
@MOM6setenv OBCSDIR  @shared_COUPLED/ocean/MOM6/${OGCM_IM}x${OGCM_JM}
@COUPLEDsetenv SSTDIR  @COUPLEDIR/SST/MERRA2/${OGCM_IM}x${OGCM_JM}
@COUPLEDsetenv BCTAG `basename $ABCSDIR`
@DATAOCEANsetenv BCTAG `basename $BCSDIR`

set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -f

@COUPLED /bin/mkdir -p RESTART
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

@COUPLED/bin/ln -sf $OBCSDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
@COUPLED/bin/ln -sf $ABCSDIR/@ATMOStag_@OCEANtag-Pfafstetter.til   tile.data
@COUPLED/bin/ln -sf $ABCSDIR/@ATMOStag_@OCEANtag-Pfafstetter.TRN   runoff.bin
@COUPLED/bin/ln -sf $OBCSDIR/MAPL_Tripolar.nc .
@COUPLED/bin/ln -sf $OBCSDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
@MOM5#/bin/ln -s @COUPLEDIR/a@HIST_IMx@HIST_JM_o${OGCM_IM}x${OGCM_JM}/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data
@MOM6#/bin/ln -s @COUPLEDIR/MOM6/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data

# Precip correction
#/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP

@DATAOCEAN /bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.til  tile.data
@DATAOCEAN if(     -e  $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL) then
@DATAOCEAN /bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL  tile.bin
@DATAOCEAN endif

# DAS or REPLAY Mode (AGCM.rc:  pchem_clim_years = 1-Year Climatology)
# --------------------------------------------------------------------
@OPS_SPECIES/bin/ln -sf $BCSDIR/Shared/pchem.species.Clim_Prod_Loss.z_721x72.nc4 species.data

# CMIP-5 Ozone Data (AGCM.rc:  pchem_clim_years = 228-Years)
# ----------------------------------------------------------
@CMIP_SPECIES/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data

# S2S pre-industrial with prod/loss of stratospheric water vapor
# (AGCM.rc:  pchem_clim_years = 3-Years,  and  H2O_ProdLoss: 1 )
# --------------------------------------------------------------
#/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-6.wH2OandPL.1850s.z_91x72.nc4 species.data

# MERRA-2 Ozone Data (AGCM.rc:  pchem_clim_years = 39-Years)
# ----------------------------------------------------------
@MERRA2OX_SPECIES/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.MERRA2OX.197902-201706.z_91x72.nc4 species.data

/bin/ln -sf $BCSDIR/Shared/*bin .
/bin/ln -sf $BCSDIR/Shared/*c2l*.nc4 .

@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/visdf_@RES_DATELINE.dat visdf.dat
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/nirdf_@RES_DATELINE.dat nirdf.dat
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/vegdyn_@RES_DATELINE.dat vegdyn.data
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/lai_clim_@RES_DATELINE.data lai.data
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/green_clim_@RES_DATELINE.data green.data
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/ndvi_clim_@RES_DATELINE.data ndvi.data

@COUPLED/bin/ln -sf $ABCSDIR/visdf_@RES_DATELINE.dat visdf.dat
@COUPLED/bin/ln -sf $ABCSDIR/nirdf_@RES_DATELINE.dat nirdf.dat
@COUPLED/bin/ln -sf $ABCSDIR/vegdyn_@RES_DATELINE.dat vegdyn.data
@COUPLED/bin/ln -sf $ABCSDIR/lai_clim_@RES_DATELINE.data lai.data
@COUPLED/bin/ln -sf $ABCSDIR/green_clim_@RES_DATELINE.data green.data
@COUPLED/bin/ln -sf $ABCSDIR/ndvi_clim_@RES_DATELINE.data ndvi.data

@GCMRUN_CATCHCNif ( -f $BCSDIR/$BCRSLV/lnfm_clim_@RES_DATELINE.data  ) /bin/ln -sf $BCSDIR/$BCRSLV/lnfm_clim_@RES_DATELINE.data lnfm.data
@GCMRUN_CATCHCN/bin/ln -s $BCSDIR/land/shared/CO2_MonthlyMean_DiurnalCycle.nc4

#@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
#@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
#@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data
@DATAOCEAN/bin/ln -sf /discover/nobackup/bmauer/gmted_topo/NCAR_TOPO_GMTED_UFS_SMOOTHING/c${AGCM_IM}/smoothed/gmted_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data
@DATAOCEAN/bin/ln -sf /discover/nobackup/bmauer/gmted_topo/NCAR_TOPO_GMTED_UFS_SMOOTHING/c${AGCM_IM}/smoothed/gmted_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data
@DATAOCEAN/bin/ln -sf /discover/nobackup/bmauer/gmted_topo/NCAR_TOPO_GMTED_UFS_SMOOTHING/c${AGCM_IM}/smoothed/gmted_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data

@COUPLED/bin/ln -sf $ABCSDIR/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
@COUPLED/bin/ln -sf $ABCSDIR/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
@COUPLED/bin/ln -sf $ABCSDIR/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data

@FVCUBEDif(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
@FVCUBED/bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
@FVCUBEDendif

@COUPLED cp $HOMDIR/*_table .
@COUPLED cp $OBCSDIR/INPUT/* INPUT
@COUPLED /bin/ln -sf $OBCSDIR/cice/kmt_cice.bin .
@COUPLED /bin/ln -sf $OBCSDIR/cice/grid_cice.bin .

_EOF_

@GCMRUN_CATCHCNset LSM_CHOICE = `grep LSM_CHOICE:  AGCM.rc | cut -d':' -f2`
@GCMRUN_CATCHCNif ($LSM_CHOICE == 2) then
@GCMRUN_CATCHCN  grep -v "'CNFROOTC'" HISTORY.rc > Hist_tmp.rc && mv Hist_tmp.rc HISTORY.rc
@GCMRUN_CATCHCNendif

@DATAOCEAN echo "/bin/ln -sf $SSTDIR"'/@SSTFILE   sst.data' >> $FILE
@DATAOCEAN echo "/bin/ln -sf $SSTDIR"'/@ICEFILE fraci.data' >> $FILE
@DATAOCEAN echo "/bin/ln -sf $SSTDIR"'/@KPARFILE SEAWIFS_KPAR_mon_clim.data' >> $FILE

chmod +x linkbcs
cp  linkbcs $EXPDIR

#######################################################################
#                    Get Executable and RESTARTS
#######################################################################

cp $EXPDIR/GEOSgcm.x .

set rst_files      = `grep "RESTART_FILE"    AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_file_names = `grep "RESTART_FILE"    AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2`

set chk_files      = `grep "CHECKPOINT_FILE" AGCM.rc | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_file_names = `grep "CHECKPOINT_FILE" AGCM.rc | grep -v "#" | cut -d ":" -f2`

set monthly_chk_names = `cat $EXPDIR/HISTORY.rc | grep -v '^[\t ]*#' | sed -n 's/\([^\t ]\+\).monthly:[\t ]*1.*/\1/p' | sed 's/$/_rst/' `

# Remove possible bootstrap parameters (+/-)
# ------------------------------------------
set dummy = `echo $rst_file_names`
set rst_file_names = ''
foreach rst ( $dummy )
  set length  = `echo $rst | awk '{print length($0)}'`
  set    bit  = `echo $rst | cut -c1`
  if(  "$bit" == "+" | \
       "$bit" == "-" ) set rst = `echo $rst | cut -c2-$length`
  set rst_file_names = `echo $rst_file_names $rst`
end

# Copy Restarts to Scratch Directory
# ----------------------------------
if( $GCMEMIP == TRUE ) then
    foreach rst ( $rst_file_names $monthly_chk_names )
      if(-e $EXPDIR/restarts/$RSTDATE/$rst ) cp $EXPDIR/restarts/$RSTDATE/$rst . &
    end
else
    foreach rst ( $rst_file_names $monthly_chk_names )
      if(-e $EXPDIR/$rst ) cp $EXPDIR/$rst . &
    end
endif
wait

# Get proper ridge scheme GWD internal restart
# --------------------------------------------
/bin/rm gwd_internal_rst
/bin/cp @GWDRSDIR/gwd_internal_c${AGCM_IM} gwd_internal_rst

@COUPLED /bin/mkdir INPUT
@COUPLED cp $EXPDIR/RESTART/* INPUT

# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_file_names )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            cp $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
@COUPLED    cp -r $EXPDIR/RESTART ${EXPDIR}/restarts/RESTART.${edate}
   cd $EXPDIR/restarts
      @DATAOCEAN tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
      @COUPLED tar cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} RESTART.${edate}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
     @COUPLED /bin/rm -rf RESTART.${edate}
   cd $SCRDIR
endif

# If any restart is binary, set NUM_READERS to 1 so that
# +-style bootstrapping of missing files can occur in
# MAPL. pbinary cannot do this, but pnc4 can.
# ------------------------------------------------------
set found_binary = 0

foreach rst ( $rst_file_names )
   if (-e $rst) then
      set rst_type = `/usr/bin/file -Lb --mime-type $rst`
      if ( $rst_type =~ "application/octet-stream" ) then
         set found_binary = 1
      endif
   endif
end

if ($found_binary == 1) then
   /bin/mv AGCM.rc AGCM.tmp
   cat AGCM.tmp | sed -e "/^NUM_READERS/ s/\([0-9]\+\)/1/g" > AGCM.rc
   /bin/rm AGCM.tmp
endif


##################################################################
######
######         Perform multiple iterations of Model Run
######
##################################################################

@ counter    = 1
while ( $counter <= ${NUM_SGMT} )

/bin/rm -f  EGRESS

if( $GCMEMIP == TRUE ) then
    cp -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
else
    cp -f $HOMDIR/CAP.rc .
endif

/bin/mv CAP.rc CAP.rc.orig
awk '{$1=$1};1' < CAP.rc.orig > CAP.rc

# Set Time Variables for Current_(c), Ending_(e), and Segment_(s) dates
# ---------------------------------------------------------------------
set nymdc = `awk '{print $1}' cap_restart`
set nhmsc = `awk '{print $2}' cap_restart`
set nymde = `grep '^\s*END_DATE:' CAP.rc | cut -d: -f2 | awk '{print $1}'`
set nhmse = `grep '^\s*END_DATE:' CAP.rc | cut -d: -f2 | awk '{print $2}'`
set nymds = `grep '^\s*JOB_SGMT:' CAP.rc | cut -d: -f2 | awk '{print $1}'`
set nhmss = `grep '^\s*JOB_SGMT:' CAP.rc | cut -d: -f2 | awk '{print $2}'`

# Compute Time Variables at the Finish_(f) of current segment
# -----------------------------------------------------------
set nyear   = `echo $nymds | cut -c1-4`
set nmonth  = `echo $nymds | cut -c5-6`
set nday    = `echo $nymds | cut -c7-8`
set nhour   = `echo $nhmss | cut -c1-2`
set nminute = `echo $nhmss | cut -c3-4`
set nsec    = `echo $nhmss | cut -c5-6`
       @ dt = $nsec + 60 * $nminute + 3600 * $nhour + 86400 * $nday

set nymdf = $nymdc
set nhmsf = $nhmsc
set date  = `$GEOSBIN/tick $nymdf $nhmsf $dt`
set nymdf =  $date[1]
set nhmsf =  $date[2]
set year  = `echo $nymdf | cut -c1-4`
set month = `echo $nymdf | cut -c5-6`
set day   = `echo $nymdf | cut -c7-8`

     @  month = $month + $nmonth
while( $month > 12 )
     @  month = $month - 12
     @  year  = $year  + 1
end
     @  year  = $year  + $nyear
     @ nymdf  = $year * 10000 + $month * 100 + $day

if( $nymdf >  $nymde )    set nymdf = $nymde
if( $nymdf == $nymde )    then
    if( $nhmsf > $nhmse ) set nhmsf = $nhmse
endif

set yearc = `echo $nymdc | cut -c1-4`
set yearf = `echo $nymdf | cut -c1-4`

# For Non-Reynolds SST, Modify local CAP.rc Ending date if Finish time exceeds Current year boundary
# --------------------------------------------------------------------------------------------------
if( @OCEANtag != DE0360xPE0180 ) then
    if( $yearf > $yearc ) then
      #@ yearf = $yearc + 1
      #@ nymdf = $yearf * 10000 + 0101
      # set oldstring = `grep '^\s*END_DATE:' CAP.rc`
      # set newstring = "END_DATE: $nymdf $nhmsf"
      # /bin/mv CAP.rc CAP.tmp
      # cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
    endif
endif

# Which ExtData are we using
set  EXTDATA2G_TRUE = `grep -i '^\s*USE_EXTDATA2G:\s*\.TRUE\.'    CAP.rc | wc -l`

# Select proper AMIP GOCART Emission RC Files
# -------------------------------------------
if( ${EMISSIONS} == AMIP_EMISSIONS ) then
    if( $EXTDATA2G_TRUE == 0 ) then
       set AMIP_Transition_Date = 20000301

       # Before 2000-03-01, we need to use AMIP.20C which has different
       # emissions (HFED instead of QFED) valid before 2000-03-01. Note
       # that if you make a change to anything in $EXPDIR/RC/AMIP or
       # $EXPDIR/RC/AMIP.20C, you might need to make a change in the other
       # directory to be consistent. Some files in AMIP.20C are symlinks to
       # that in AMIP but others are not.

       if( $nymdc < ${AMIP_Transition_Date} ) then
            set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP.20C
            if( $nymdf > ${AMIP_Transition_Date} ) then
             set nymdf = ${AMIP_Transition_Date}
             set oldstring = `grep '^\s*END_DATE:' CAP.rc`
             set newstring = "END_DATE: $nymdf $nhmsf"
             /bin/mv CAP.rc CAP.tmp
                        cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
            endif
       else
            set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
       endif
    else
       set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
    endif

    if( $AGCM_LM == 72 ) then
        cp ${AMIP_EMISSIONS_DIRECTORY}/*.rc .
        cp ${AMIP_EMISSIONS_DIRECTORY}/*.yaml .
    else
        set files = `/bin/ls -1 ${AMIP_EMISSIONS_DIRECTORY}/*.rc ${AMIP_EMISSIONS_DIRECTORY}/*.yaml`
        foreach file ($files)
          /bin/rm -f `basename $file`
          /bin/rm -f dummy
          cp $file dummy
          cat dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > `basename $file`
        end
    endif

endif

# Rename big ExtData files that are not needed
# --------------------------------------------
set            SC_TRUE = `grep -i '^\s*ENABLE_STRATCHEM:\s*\.TRUE\.'     GEOS_ChemGridComp.rc | wc -l`
if (          $SC_TRUE == 0 && -e StratChem_ExtData.rc          ) /bin/mv          StratChem_ExtData.rc          StratChem_ExtData.rc.NOT_USED
set           GMI_TRUE = `grep -i '^\s*ENABLE_GMICHEM:\s*\.TRUE\.'       GEOS_ChemGridComp.rc | wc -l`
if (         $GMI_TRUE == 0 && -e GMI_ExtData.rc                ) /bin/mv                GMI_ExtData.rc                GMI_ExtData.rc.NOT_USED
set           GCC_TRUE = `grep -i '^\s*ENABLE_GEOSCHEM:\s*\.TRUE\.'      GEOS_ChemGridComp.rc | wc -l`
if (         $GCC_TRUE == 0 && -e GEOSCHEMchem_ExtData.rc       ) /bin/mv       GEOSCHEMchem_ExtData.rc       GEOSCHEMchem_ExtData.rc.NOT_USED
set         CARMA_TRUE = `grep -i '^\s*ENABLE_CARMA:\s*\.TRUE\.'         GEOS_ChemGridComp.rc | wc -l`
if (       $CARMA_TRUE == 0 && -e CARMAchem_GridComp_ExtData.rc ) /bin/mv CARMAchem_GridComp_ExtData.rc CARMAchem_GridComp_ExtData.rc.NOT_USED
set           DNA_TRUE = `grep -i '^\s*ENABLE_DNA:\s*\.TRUE\.'           GEOS_ChemGridComp.rc | wc -l`
if (         $DNA_TRUE == 0 && -e DNA_ExtData.rc                ) /bin/mv                DNA_ExtData.rc                DNA_ExtData.rc.NOT_USED
set         ACHEM_TRUE = `grep -i '^\s*ENABLE_ACHEM:\s*\.TRUE\.'         GEOS_ChemGridComp.rc | wc -l`
if (       $ACHEM_TRUE == 0 && -e ACHEM_ExtData.rc              ) /bin/mv              ACHEM_ExtData.rc              ACHEM_ExtData.rc.NOT_USED
set   GOCART_DATA_TRUE = `grep -i '^\s*ENABLE_GOCART_DATA:\s*\.TRUE\.'   GEOS_ChemGridComp.rc | wc -l`
if ( $GOCART_DATA_TRUE == 0 && -e GOCARTdata_ExtData.rc         ) /bin/mv         GOCARTdata_ExtData.rc         GOCARTdata_ExtData.rc.NOT_USED

@MP_TURN_OFF_WSUB_EXTDATA# 1MOM and GFDL microphysics do not use WSUB_CLIM
@MP_TURN_OFF_WSUB_EXTDATA# -------------------------------------------------
if ($EXTDATA2G_TRUE == 0 ) then
   @MP_TURN_OFF_WSUB_EXTDATA/bin/mv WSUB_ExtData.rc WSUB_ExtData.tmp
   @MP_TURN_OFF_WSUB_EXTDATAcat WSUB_ExtData.tmp | sed -e '/^WSUB_CLIM/ s#ExtData.*#/dev/null#' > WSUB_ExtData.rc
else
   @MP_TURN_OFF_WSUB_EXTDATA/bin/mv WSUB_ExtData.yaml WSUB_ExtData.tmp
   @MP_TURN_OFF_WSUB_EXTDATAcat WSUB_ExtData.tmp | sed -e '/collection:/ s#WSUB_SWclim.*#/dev/null#' > WSUB_ExtData.yaml
endif
@MP_TURN_OFF_WSUB_EXTDATA/bin/rm WSUB_ExtData.tmp

# Generate the complete ExtData.rc
# --------------------------------
if(-e ExtData.rc )    /bin/rm -f   ExtData.rc
set  extdata_files = `/bin/ls -1 *_ExtData.rc`

# Switch to MODIS v6.1 data after Nov 2021
if( $EXTDATA2G_TRUE == 0 ) then
   set MODIS_Transition_Date = 20211101
   if ( ${EMISSIONS} == OPS_EMISSIONS && ${MODIS_Transition_Date} <= $nymdc ) then
       cat $extdata_files | sed 's|\(qfed2.emis_.*\).006.|\1.061.|g' > ExtData.rc
   else
   cat $extdata_files > ExtData.rc
   endif
endif

if( $EXTDATA2G_TRUE == 1 ) then

  $GEOSBIN/construct_extdata_yaml_list.py GEOS_ChemGridComp.rc
  touch ExtData.rc

endif

# Move GOCART to use RRTMGP Bands
# -------------------------------
# UNCOMMENT THE LINES BELOW IF RUNNING RRTMGP
#
#set instance_files = `/bin/ls -1 *_instance*.rc`
#foreach instance ($instance_files)
#   /bin/mv $instance $instance.tmp
#   cat $instance.tmp | sed -e '/RRTMG/ s#RRTMG#RRTMGP#' > $instance
#   /bin/rm $instance.tmp
#end

# Link Boundary Conditions for Appropriate Date
# ---------------------------------------------
setenv YEAR $yearc
./linkbcs

if (! -e tile.bin) then
$GEOSBIN/binarytile.x tile.data tile.bin
@MOM5 $GEOSBIN/binarytile.x tile_hist.data tile_hist.bin
endif

# If running in dual ocean mode, link sst and fraci data here
#set yy  = `cat cap_restart | cut -c1-4`
#echo $yy
#ln -sf $SSTDIR/dataoceanfile_MERRA2_SST.${OGCM_IM}x${OGCM_JM}.${yy}.data sst.data
#ln -sf $SSTDIR/dataoceanfile_MERRA2_ICE.${OGCM_IM}x${OGCM_JM}.${yy}.data fraci.data

#######################################################################
#                Split Saltwater Restart if detected
#######################################################################

if ( (-e $SCRDIR/openwater_internal_rst) && (-e $SCRDIR/seaicethermo_internal_rst)) then
  echo "Saltwater internal state is already split, good to go!"
else
 if ( ( ( -e $SCRDIR/saltwater_internal_rst ) || ( -e $EXPDIR/saltwater_internal_rst) ) && ( $counter == 1 ) ) then

   echo "Found Saltwater internal state. Splitting..."

   # If saltwater_internal_rst is in EXPDIR move to SCRDIR
   # -----------------------------------------------------
   if ( -e $EXPDIR/saltwater_internal_rst ) /bin/mv $EXPDIR/saltwater_internal_rst $SCRDIR

   # The splitter script requires an OutData directory
   # -------------------------------------------------
   if (! -d OutData ) mkdir -p OutData

   # Run the script
   # --------------
   $RUN_CMD 1 $GEOSBIN/SaltIntSplitter tile.data $SCRDIR/saltwater_internal_rst

   # Move restarts
   # -------------
   /bin/mv OutData/openwater_internal_rst OutData/seaicethermo_internal_rst .

   # Remove OutData
   # --------------
   /bin/rmdir OutData

   # Make decorated copies for restarts tarball
   # ------------------------------------------
   cp openwater_internal_rst    $EXPID.openwater_internal_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
   cp seaicethermo_internal_rst $EXPID.seaicethermo_internal_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}

   # Inject decorated copies into restarts tarball
   # ---------------------------------------------
   tar rf $EXPDIR/restarts/restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}

   # Remove the decorated restarts
   # -----------------------------
   /bin/rm $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}

   # Remove the saltwater internal restart
   # -------------------------------------
   /bin/rm $SCRDIR/saltwater_internal_rst
 else
   echo "Neither saltwater_internal_rst, nor openwater_internal_rst and seaicethermo_internal_rst were found. Abort!"
   exit 6
 endif
endif

# Test Openwater Restart for Number of tiles correctness
# ------------------------------------------------------

if ( -x $GEOSBIN/rs_numtiles.x ) then

   set N_OPENW_TILES_EXPECTED = `grep '^\s*0' tile.data | wc -l`
   set N_OPENW_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x openwater_internal_rst | grep Total | awk '{print $NF}'`

   if ( $N_OPENW_TILES_EXPECTED != $N_OPENW_TILES_FOUND ) then
      echo "Error! Found $N_OPENW_TILES_FOUND tiles in openwater. Expect to find $N_OPENW_TILES_EXPECTED tiles."
      echo "Your restarts are probably for a different ocean."
      exit 7
   endif

endif

# Check for MERRA2OX Consistency
# ------------------------------

# The MERRA2OX pchem file is only valid until 201706, so this is a first
# attempt at a check to make sure you aren't using it and are past the date

# Check for MERRA2OX by looking at AGCM.rc
set PCHEM_CLIM_YEARS = `awk '/pchem_clim_years/ {print $2}' AGCM.rc`

# If it is 39, we are using MERRA2OX
if ( $PCHEM_CLIM_YEARS == 39 ) then

   # Grab the date from cap_restart
   set YEARMON = `cat cap_restart | cut -c1-6`

   # Set a magic date
   set MERRA2OX_END_DATE = "201706"

   # String comparison seems to work here...
   if ( $YEARMON > $MERRA2OX_END_DATE ) then
      echo "You seem to be using MERRA2OX pchem species file, but your simulation date [${YEARMON}] is after 201706. This file is only valid until this time."
      exit 2
   endif
endif

# Environment variables for MPI, etc
# ----------------------------------

@SETENVS

@GPUSTART

# Run bundleParser.py
#---------------------
python bundleParser.py

# If REPLAY, link necessary forcing files
# ---------------------------------------
set  REPLAY_MODE = `grep '^\s*REPLAY_MODE:' AGCM.rc | cut -d: -f2`
if( $REPLAY_MODE == 'Exact' | $REPLAY_MODE == 'Regular' ) then

     set ANA_EXPID    = `grep '^\s*REPLAY_ANA_EXPID:'    AGCM.rc | cut -d: -f2`
     set ANA_LOCATION = `grep '^\s*REPLAY_ANA_LOCATION:' AGCM.rc | cut -d: -f2`

     set REPLAY_FILE        = `grep '^\s*REPLAY_FILE:'   AGCM.rc | cut -d: -f2`
     set REPLAY_FILE09      = `grep '^\s*REPLAY_FILE09:' AGCM.rc | cut -d: -f2`
     set REPLAY_FILE_TYPE   = `echo $REPLAY_FILE           | cut -d"/" -f1 | grep -v %`
     set REPLAY_FILE09_TYPE = `echo $REPLAY_FILE09         | cut -d"/" -f1 | grep -v %`

     # Modify GAAS_GridComp.rc and Link REPLAY files
     # ---------------------------------------------
     /bin/mv -f GAAS_GridComp.rc GAAS_GridComp.tmp
     cat GAAS_GridComp.tmp | sed -e "s?aod/Y%y4/M%m2/${ANA_EXPID}.?aod/Y%y4/M%m2/${ANA_EXPID}.?g" > GAAS_GridComp.rc

     /bin/ln -sf ${ANA_LOCATION}/aod .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE_TYPE} .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE09_TYPE} .

endif

# Establish safe default number of OpenMP threads
# -----------------------------------------------
setenv OMP_NUM_THREADS 1

# Run GEOSgcm.x
# -------------
if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

if( $USE_IOSERVER == 1 ) then
   set IOSERVER_OPTIONS = "--npes_model $MODEL_NPES --nodes_output_server $IOS_NODES"

   # Per SI Team, the multigroup server should always be used
   # The ideal number of backend PEs is based on the number of HISTORY
   # collections and number of IO nodes

   # First we figure out the number of collections in the HISTORY.rc (this is not perfect, but is close to right)
   set NUM_HIST_COLS = `cat HISTORY.rc | sed -n '/^COLLECTIONS:/,/^ *::$/{p;/^ *::$/q}' | grep -v '^ *#' | wc -l`

   # Protect against divide by zero
   if ($IOS_NODES == 0) then
      echo "Something is wrong. IOSERVER asked for, but zero IO nodes provided"
      exit 3
   endif

   # Now we divide that number of collections by the ioserver nodes
   set NUM_BACKEND_PES = `echo "scale=6;(($NUM_HIST_COLS - 1) / $IOS_NODES)" | bc | awk '{print int($1 + 0.5)}'`

   # Finally multigroup requires at least two backend pes
   if ($NUM_BACKEND_PES < 2) set NUM_BACKEND_PES = 2

   set IOSERVER_EXTRA = "--oserver_type multigroup --npes_backend_pernode $NUM_BACKEND_PES"
else
   set IOSERVER_OPTIONS = ""
   set IOSERVER_EXTRA = ""
endif

$RUN_CMD $NPES ./GEOSgcm.x $IOSERVER_OPTIONS $IOSERVER_EXTRA --logging_config 'logging.yaml'

if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

@GPUEND

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc
if ( $rc == -1 ) exit -1
endif # STATS_ONLY

if ($EXPID =~ '*RFCST*') then

echo Running Stats

foreach CLIM_SRC ('MERRA-2' 'ERA5')

cd $SCRDIR

set nDAYS = `cat CAP.rc | grep JOB_SGMT: | cut -d: -f2 | cut -c7-9`
@ nMAX = $nDAYS + 1
@ fHOURS = 24 * $nDAYS
@ nSECS = -60 * 60 * $fHOURS

set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set dateV  = `$GEOSBIN/tick $nymdc $nhmsc $nSECS`
set nymd =  $dateV[1]
set yyyymm   = `echo $nymd | cut -b1-6`
set FDAYS  = `printf "%02d" $nDAYS`

set statsdir = $SCRDIR/GEOS-${CLIM_SRC}.stats.$yyyymm.f${FDAYS}day
mkdir -p $statsdir
cd       $statsdir

set year0      = `echo $nymd | cut -b1-4`
set month0     = `echo $nymd | cut -b5-6`
set fcst_files = `/bin/ls -1 ../*geosgcm_prog*nc4`

# Set Climatology
# ------------
if ( "$CLIM_SRC" == 'ERA5' ) then
   set clim_files = `/bin/ls -1 /discover/nobackup/projects/gmao/g6dev/sdrabenh/valdat/era5/clim_data/era5_mnth.mrg1_avg.1440x0721.Y1985-2014.T[01][0268]z.nc4`
   set levs_clim  = `echo 1000 975 950 925 900 875 850 825 800 775 750 700 650 600 550 500 450 400 350 300 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1`
else
   set clim_files = `/bin/ls -1 $SHARE/gmao_ops/verification/stats/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_[01][0268]z.576x361.data.nc4`
   set levs_clim  = `echo 1000 975 950 925 900 850 800 750 700 600 500 400 300 250 200 150 100 70 50 30 10 7 5 3 1`
endif

set ana_files = ''
@ n = 1
while ($n <= $nMAX)
set year  = `echo $nymd | cut -b1-4`
set month = `echo $nymd | cut -b5-6`
if ( "$CLIM_SRC" == 'ERA5' ) then
 set next = `/bin/ls -1 /discover/nobackup/projects/gmao/g6dev/sdrabenh/valdat/era5/pres_levels_daily/Y$year/M$month/era5_pl-allvar.${nymd}*z.nc4`
else
 set next = `/bin/ls -1 /discover/nobackup/projects/gmao/dadev/dao_it/archive/@X_EXPID/diag/Y${year}/M${month}/@X_EXPID.inst3_3d_asm_Np.${nymd}*.nc4`
endif
set ana_files = `echo $ana_files $next`
set date = `$GEOSUTIL/post/tick $nymd 0 86400`
set nymd = $date[1]
echo $n $date $nMAX
@ n = $n + 1
end

cat > stats_$CLIM_SRC.j << EOF
#!/bin/csh -f
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --job-name=@EXPID
#SBATCH --output=${EXPDIR}_STATS_${CLIM_SRC}.o%j
#SBATCH --no-requeue
#SBATCH @SYSTEM
setenv GEOSDIR   $GEOSDIR
setenv GEOSBIN   $GEOSBIN
setenv GEOSETC   $GEOSETC
setenv GEOSUTIL  $GEOSUTIL
source $GEOSBIN/g5_modules
setenv @LD_LIBRARY_PATH_CMD ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${GEOSDIR}/lib
echo $@LD_LIBRARY_PATH_CMD
cd $statsdir
$RUN_CMD 1 $GEOSUTIL/bin/stats.x -fcst $fcst_files -ana $ana_files -cli $clim_files -rc $GEOSUTIL/post/stats.rc \
                                 -levs $levs_clim -tag $EXPID -nfreq 060000 -fhour $fHOURS
exit 0
EOF
sbatch stats_$CLIM_SRC.j

cd $SCRDIR

end
endif
exit 0

#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`awk '{print $1}' cap_restart`_`awk '{print $2}' cap_restart | cut -c1-2`z

@COUPLED cp -r RESTART ${EXPDIR}/restarts/RESTART.${edate}
@COUPLED cp RESTART/* INPUT

# Move Intermediate Checkpoints to RESTARTS directory
# ---------------------------------------------------
set   checkpoints  =    `/bin/ls -1 *_checkpoint.*`
if( $#checkpoints != 0 ) /bin/mv -f *_checkpoint.* ${EXPDIR}/restarts


# Rename Final Checkpoints for Archive
# ------------------------------------
    set checkpoints = `/bin/ls -1 *_checkpoint`
foreach checkpoint ($checkpoints)
        set   chk_type = `/usr/bin/file -Lb --mime-type $checkpoint`
            if ( $chk_type =~ "application/octet-stream" ) then
                  set ext  = bin
            else
                  set ext  = nc4
            endif
       /bin/mv            $checkpoint      $EXPID.${checkpoint}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.$ext
       $GEOSBIN/stripname _checkpoint _rst $EXPID.${checkpoint}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.$ext
end


# Remove Initial RESTARTS
# -----------------------
set restarts = `/bin/ls -1 *_rst`
/bin/rm  $restarts


# Copy Renamed Final Checkpoints to RESTARTS directory
# ----------------------------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
cp $restart ${EXPDIR}/restarts
end

# Remove EXPID from RESTART name
# ------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
$GEOSBIN/stripname $EXPID. '' $restart
end

# Remove DATE and VERSION Stamps from RESTART name
# ------------------------------------------------
    set  restarts = `/bin/ls -1 *_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
$GEOSBIN/stripname .${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.\* '' $restart
end


# TAR ARCHIVED RESTARTS
# ---------------------
cd $EXPDIR/restarts
if( $FSEGMENT == 00000000 ) then
        @DATAOCEAN tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
        @COUPLED tar cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.* RESTART.${edate}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
        @COUPLED /bin/rm -rf RESTART.${edate}
endif


#######################################################################
#               Move HISTORY Files to Holding Directory
#######################################################################

# Move current files to /holding
# ------------------------------
cd $SCRDIR
foreach collection ( $collections )
   /bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection
end

@COUPLED # MOM-Specific Output Files
@COUPLED # -------------------------
@MOM5 set dsets="ocean_month"
@MOM6 set dsets="ocean_state prog_z sfc_ave forcing"
@COUPLED  foreach dset ( $dsets )
@COUPLED  set num = `/bin/ls -1 $dset.nc | wc -l`
@COUPLED  if($num != 0) then
@COUPLED     if(! -e $EXPDIR/MOM_Output) mkdir -p $EXPDIR/MOM_Output
@COUPLED     /bin/mv $SCRDIR/$dset.nc $EXPDIR/MOM_Output/$dset.${edate}.nc
@COUPLED  endif
@COUPLED  end
@COUPLED
#######################################################################
#                 Run Post-Processing and Forecasts
#######################################################################

$GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles

if( $FSEGMENT != 00000000 ) then
     set REPLAY_BEG_DATE = `grep '^\s*BEG_REPDATE:' $HOMDIR/CAP.rc | cut -d: -f2`
     set REPLAY_END_DATE = `grep '^\s*END_REPDATE:' $HOMDIR/CAP.rc | cut -d: -f2`
     set nday            = `echo $FSEGMENT | bc`
         @ dt  = 10800 - 86400 * $nday
     set date  = `$GEOSBIN/tick $nymdc $nhmsc $dt`
     set nymdz =  $date[1]
     set nhmsz =  $date[2]

     if( $nymdz >= ${REPLAY_BEG_DATE} & \
         $nymdz <= ${REPLAY_END_DATE} ) then
         setenv CYCLED .TRUE.
         $EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $nday TRUE
     endif
endif

#######################################################################
#                         Update Iteration Counter
#######################################################################

set enddate = `echo  $END_DATE | cut -c1-8`
set capdate = `cat cap_restart | cut -c1-8`

if ( $capdate < $enddate ) then
@ counter = $counter    + 1
else
@ counter = ${NUM_SGMT} + 1
endif

end   # end of segment loop; remain in $SCRDIR

#######################################################################
#                              Re-Submit Job
#######################################################################

if( $GCMEMIP == TRUE ) then
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/$rst
     end
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       cp $rst $EXPDIR/restarts/$RSTDATE/$rst &
     end
     wait
     cp cap_restart $EXPDIR/restarts/$RSTDATE/cap_restart
else
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/$rst
     end
        /bin/rm -f $EXPDIR/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       cp $rst $EXPDIR/$rst &
     end
     wait
     cp cap_restart $EXPDIR/cap_restart
endif

@COUPLED cp -rf RESTART $EXPDIR

if ( $rc == 0 ) then
      cd  $HOMDIR
      if ( $GCMEMIP == TRUE ) then
          if( $capdate < $enddate ) @BATCH_CMD $HOMDIR/gcm_run.j$RSTDATE
          else
          if( $capdate < $enddate ) @BATCH_CMD $HOMDIR/gcm_run.j
      endif
endif

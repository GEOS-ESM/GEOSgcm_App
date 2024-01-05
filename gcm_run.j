#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#@BATCH_TIME@RUN_T
#@RUN_P
#@BATCH_JOBNAME@RUN_N
#@RUN_Q
#@BATCH_GROUP
#@BATCH_JOINOUTERR
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

setenv SITE             @SITE
setenv GEOSDIR          @GEOSDIR
setenv GEOSBIN          @GEOSBIN
setenv GEOSETC          @GEOSETC
setenv GEOSUTIL         @GEOSSRC

source $GEOSBIN/g5_modules
setenv @LD_LIBRARY_PATH_CMD ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${GEOSDIR}/lib

setenv RUN_CMD "@RUN_CMD"

setenv GCMVER `cat $GEOSETC/.AGCM_VERSION`
echo   VERSION: $GCMVER

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################


setenv  EXPID   @EXPID
setenv  EXPDIR  @EXPDIR
setenv  HOMDIR  @HOMDIR

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

setenv  SCRDIR  $EXPDIR/scratch

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

# Calculate number of cores/nodes for IOSERVER
# --------------------------------------------

set USE_IOSERVER      = @USE_IOSERVER
set NUM_OSERVER_NODES = `grep '^\s*IOSERVER_NODES:'  $HOMDIR/AGCM.rc | cut -d: -f2`
set NUM_BACKEND_PES   = `grep '^\s*NUM_BACKEND_PES:' $HOMDIR/AGCM.rc | cut -d: -f2`

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

set NCPUS_PER_NODE = @NCPUS_PER_NODE
set NUM_MODEL_NODES=`echo "scale=6;($MODEL_NPES / $NCPUS_PER_NODE)" | bc | awk 'function ceil(x, y){y=int(x); return(x>y?y+1:y)} {print ceil($1)}'`

if ( $NCPUS != NULL ) then

   if ( $USE_IOSERVER == 1 ) then

      @ TOTAL_NODES = $NUM_MODEL_NODES + $NUM_OSERVER_NODES
      @ TOTAL_PES = $TOTAL_NODES * $NCPUS_PER_NODE

      if( $TOTAL_PES > $NCPUS ) then
         echo "CPU Resources are Over-Specified"
         echo "--------------------------------"
         echo "Allotted  NCPUs: $NCPUS"
         echo "Requested NCPUs: $TOTAL_PES"
         echo ""
         echo "Specified NX: $NX"
         echo "Specified NY: $NY"
         echo ""
         echo "Specified model nodes: $NUM_MODEL_NODES"
         echo "Specified oserver nodes: $NUM_OSERVER_NODES"
         echo "Specified cores per node: $NCPUS_PER_NODE"
         exit
      endif

   else

      @ TOTAL_PES = $MODEL_NPES

      if( $TOTAL_PES > $NCPUS ) then
         echo "CPU Resources are Over-Specified"
         echo "--------------------------------"
         echo "Allotted  NCPUs: $NCPUS"
         echo "Requested NCPUs: $TOTAL_PES"
         echo ""
         echo "Specified NX: $NX"
         echo "Specified NY: $NY"
         echo ""
         echo "Specified model nodes: $NUM_MODEL_NODES"
         echo "Specified cores per node: $NCPUS_PER_NODE"
         exit
      endif

   endif

else
   # This is for the desktop path

   @ TOTAL_PES = $MODEL_NPES

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
if (-z input.nml) then
   echo "try cat for input.nml again"
   cat fvcore_layout.rc >> input.nml
endif
if (-z input.nml) then
   echo "input.nml is zero-length"
   exit 0
endif

@MOM6cp -f  $HOMDIR/MOM_input .
@MOM6cp -f  $HOMDIR/MOM_override .
@CICE6cp -f  $HOMDIR/ice_in .

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
@MOM5setenv SSTDIR  @COUPLEDIR/SST/MERRA2/${OGCM_IM}x${OGCM_JM}
@MOM6setenv SSTDIR  @COUPLEDIR/SST/MERRA2/${OGCM_IM}x${OGCM_JM}
@MOM5setenv BCTAG `basename $ABCSDIR`
@MOM6setenv BCTAG `basename $ABCSDIR`
#this is hard-wired for NAS for now - should make it more general
@MITsetenv GRIDDIR /nobackupp18/afahad/GEOSMITgcmFiles/GRIDDIR/a${AGCM_IM}x${AGCM_JM}_o${OGCM_IM}x${OGCM_JM}
@MITsetenv BCTAG `basename $GRIDDIR`
@DATAOCEANsetenv BCTAG `basename $BCSDIR`

set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -f

@COUPLED /bin/mkdir -p RESTART
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

@MOM5/bin/ln -sf $OBCSDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
@MOM6/bin/ln -sf $OBCSDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
@MIT/bin/ln -sf /nobackupp18/afahad/GEOSMITgcmFiles/SEAWIFS_KPAR_mon_clim.data SEAWIFS_KPAR_mon_clim.data
@MOM5/bin/ln -sf $ABCSDIR/@ATMOStag_@OCEANtag-Pfafstetter.til   tile.data
@MOM6/bin/ln -sf $ABCSDIR/@ATMOStag_@OCEANtag-Pfafstetter.til   tile.data
## Should include this >>>MOM5<<</bin/ln -s @COUPLEDIR/a@HIST_IMx@HIST_JM_o${OGCM_IM}x${OGCM_JM}/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data
## Should include this >>>MOM6<<</bin/ln -s @COUPLEDIR/MOM6/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data
@MIT/bin/ln -sf /nobackupp18/afahad/GEOSMITgcmFiles/CF0090x6C_LL5400xLL0015-Pfafstetter.til   tile.data
@MOM5/bin/ln -sf $ABCSDIR/@ATMOStag_@OCEANtag-Pfafstetter.TRN   runoff.bin
@MOM6/bin/ln -sf $ABCSDIR/@ATMOStag_@OCEANtag-Pfafstetter.TRN   runoff.bin
@MIT/bin/ln -sf /nobackupp18/afahad/GEOSMITgcmFiles/CF0090x6C_LL5400xLL0015-Pfafstetter.TRN   runoff.bin
@MOM5/bin/ln -sf $OBCSDIR/MAPL_Tripolar.nc .
@MOM6/bin/ln -sf $OBCSDIR/MAPL_Tripolar.nc .
@MIT/bin/ln -sf $GRIDDIR/mit.ascii
@MOM5/bin/ln -sf $OBCSDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
@MOM6/bin/ln -sf $OBCSDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
@MIT/bin/ln -sf $GRIDDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
@MIT/bin/ln -sf /nobackupp18/afahad/GEOSMITgcmFiles/DC0360xPC0181_LL5400x15-LL.bin DC0360xPC0181_LL5400x15-LL.bin

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

>>>GCMRUN_CATCHCN<<<if ( -f $BCSDIR/$BCRSLV/lnfm_clim_@RES_DATELINE.data  ) /bin/ln -sf $BCSDIR/$BCRSLV/lnfm_clim_@RES_DATELINE.data lnfm.data
>>>GCMRUN_CATCHCN<<</bin/ln -s $BCSDIR/land/shared/CO2_MonthlyMean_DiurnalCycle.nc4

@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
@DATAOCEAN/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data
#@DATAOCEAN/bin/ln -sf /discover/nobackup/bmauer/gmted_topo/NCAR_TOPO_GMTED_UFS_SMOOTHING/c${AGCM_IM}/smoothed/gmted_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data
#@DATAOCEAN/bin/ln -sf /discover/nobackup/bmauer/gmted_topo/NCAR_TOPO_GMTED_UFS_SMOOTHING/c${AGCM_IM}/smoothed/gmted_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data
#@DATAOCEAN/bin/ln -sf /discover/nobackup/bmauer/gmted_topo/NCAR_TOPO_GMTED_UFS_SMOOTHING/c${AGCM_IM}/smoothed/gmted_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data

@COUPLED/bin/ln -sf $ABCSDIR/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
@COUPLED/bin/ln -sf $ABCSDIR/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
@COUPLED/bin/ln -sf $ABCSDIR/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data

>>>FVCUBED<<<if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
>>>FVCUBED<<</bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
>>>FVCUBED<<<endif

@COUPLED cp $HOMDIR/*_table .
@COUPLED cp $OBCSDIR/INPUT/* INPUT
@CICE4 /bin/ln -sf $OBCSDIR/cice/kmt_cice.bin .
@CICE4 /bin/ln -sf $OBCSDIR/cice/grid_cice.bin .
@CICE6 /bin/ln -sf $OBCSDIR/cice6/cice6_grid.nc .
@CICE6 /bin/ln -sf $OBCSDIR/cice6/cice6_kmt.nc .
@CICE6 /bin/ln -sf $OBCSDIR/cice6/cice6_global.bathy.nc .

_EOF_

>>>GCMRUN_CATCHCN<<<set LSM_CHOICE = `grep LSM_CHOICE:  AGCM.rc | cut -d':' -f2`
>>>GCMRUN_CATCHCN<<<if ($LSM_CHOICE == 2) then
>>>GCMRUN_CATCHCN<<<  grep -v "'CNFROOTC'" HISTORY.rc > Hist_tmp.rc && mv Hist_tmp.rc HISTORY.rc
>>>GCMRUN_CATCHCN<<<endif

@DATAOCEAN echo "/bin/ln -sf $SSTDIR"'/@SSTFILE   sst.data' >> $FILE
@DATAOCEAN echo "/bin/ln -sf $SSTDIR"'/@ICEFILE fraci.data' >> $FILE
@DATAOCEAN echo "/bin/ln -sf $SSTDIR"'/@KPARFILE SEAWIFS_KPAR_mon_clim.data' >> $FILE

chmod +x linkbcs
cp  linkbcs $EXPDIR

#######################################################################
#                  Setup executable
#######################################################################

@SINGULARITY_BUILD #######################################################################
@SINGULARITY_BUILD #             Settings for Singularity - EXPERIMENTAL
@SINGULARITY_BUILD #######################################################################
@SINGULARITY_BUILD
@SINGULARITY_BUILD # Based on work on discover, to run you need to load the same compiler
@SINGULARITY_BUILD # and MPI to match those in the container. For example, if your container was
@SINGULARITY_BUILD # built with:
@SINGULARITY_BUILD #   GNU 11.2.0
@SINGULARITY_BUILD #   Intel Fortran 2021.6.0 (aka Intel oneAPI 2022.1.0)
@SINGULARITY_BUILD #   Intel MPI 2021.6.0 (aka Intel oneAPI 2022.1.0)
@SINGULARITY_BUILD # then you would need to load:
@SINGULARITY_BUILD #   source /usr/share/modules/init/csh
@SINGULARITY_BUILD #   module purge
@SINGULARITY_BUILD #   module load comp/gcc/11.2.0
@SINGULARITY_BUILD #   module load comp/intel/2021.6.0
@SINGULARITY_BUILD #   module load mpi/impi/2021.6.0
@SINGULARITY_BUILD #
@SINGULARITY_BUILD # And then also append ${GEOSDIR}/lib to LD_LIBRARY_PATH
@SINGULARITY_BUILD #   setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${GEOSDIR}/lib
@SINGULARITY_BUILD #
@SINGULARITY_BUILD # For now we use the fact that on discover we can source g5_modules
@SINGULARITY_BUILD #
@SINGULARITY_BUILD # If you are using singularity, set the path to the singularity sandbox here
@SINGULARITY_BUILD setenv SINGULARITY_SANDBOX @SINGULARITY_SANDBOX
@SINGULARITY_BUILD
@SINGULARITY_BUILD # Error out if SINGULARITY_SANDBOX is not set
@SINGULARITY_BUILD if( $SINGULARITY_SANDBOX == "" ) then
@SINGULARITY_BUILD    echo "ERROR: You must set SINGULARITY_SANDBOX to the path to your Singularity sandbox"
@SINGULARITY_BUILD    exit 1
@SINGULARITY_BUILD endif
@SINGULARITY_BUILD
@SINGULARITY_BUILD # If SINGULARITY_SANDBOX is non-empty, then run executable in singularity sandbox
@SINGULARITY_BUILD echo "We are running under Singularity"
@SINGULARITY_BUILD echo ""
@SINGULARITY_BUILD
@SINGULARITY_BUILD # Load the Singularity module
@SINGULARITY_BUILD module load singularity
@SINGULARITY_BUILD
@SINGULARITY_BUILD # Set Singularity Bind Paths. Note: These are dependent on where you are running.
@SINGULARITY_BUILD # By default, we'll assume you are running this script from NOBACKUP
@SINGULARITY_BUILD
@SINGULARITY_BUILD setenv REAL_BIND_PATH @REAL_BIND_PATH
@SINGULARITY_BUILD setenv BASE_BIND_PATH @BASE_BIND_PATH
@SINGULARITY_BUILD setenv BOUNDARY_DIR @BOUNDARY_DIR
@SINGULARITY_BUILD setenv SINGULARITY_BIND_PATH "-B ${NOBACKUP}:${NOBACKUP}:rw,${REAL_BIND_PATH}:${REAL_BIND_PATH}:rw,${BASE_BIND_PATH}:${BASE_BIND_PATH}:ro,${BOUNDARY_DIR}:${BOUNDARY_DIR}:ro"
@SINGULARITY_BUILD
@SINGULARITY_BUILD # If you are running from a different location, you will need to change the bind path
@SINGULARITY_BUILD # Also, note that often $NOBACKUP is, say, /discover/nobackup/username, but gcm_setup
@SINGULARITY_BUILD # will set GEOSDIR, GEOSBIN, etc. above to something like /gpfsm/dnbXX/username which
@SINGULARITY_BUILD # is the "real" physical path that /discover/nobackup/username is a symlink to.
@SINGULARITY_BUILD # You might need to change all the gpfsm paths to nobackup paths.
@SINGULARITY_BUILD
@SINGULARITY_BUILD # Set a variable to encapsulate all Singularity details
@SINGULARITY_BUILD setenv SINGULARITY_RUN "singularity exec $SINGULARITY_BIND_PATH $SINGULARITY_SANDBOX"
@SINGULARITY_BUILD
@SINGULARITY_BUILD # Detect if GEOSgcm.x is in the experiment directory
@SINGULARITY_BUILD if (-e $EXPDIR/GEOSgcm.x) then
@SINGULARITY_BUILD    echo "Found GEOSgcm.x in $EXPDIR"
@SINGULARITY_BUILD
@SINGULARITY_BUILD    # If SINGULARITY_SANDBOX is non-empty and GEOSgcm.x is found in the experiment directory,
@SINGULARITY_BUILD    # force the use of GEOSgcm.x in the installation directory
@SINGULARITY_BUILD    if( $SINGULARITY_SANDBOX != "" ) then
@SINGULARITY_BUILD       echo "NOTE: Testing has shown Singularity only works when running with"
@SINGULARITY_BUILD       echo "      the GEOSgcm.x executable directly from the installation bin directory"
@SINGULARITY_BUILD       echo ""
@SINGULARITY_BUILD       echo "      So, we will *ignore* the local GEOSgcm.x and "
@SINGULARITY_BUILD       echo "      instead use $GEOSBIN/GEOSgcm.x"
@SINGULARITY_BUILD       echo ""
@SINGULARITY_BUILD    else
@SINGULARITY_BUILD       echo "Using GEOSgcm.x from $GEOSBIN"
@SINGULARITY_BUILD    endif
@SINGULARITY_BUILD    setenv GEOSEXE $GEOSBIN/GEOSgcm.x
@SINGULARITY_BUILD else
@SINGULARITY_BUILD    echo "Using GEOSgcm.x from $GEOSBIN"
@SINGULARITY_BUILD    setenv GEOSEXE $GEOSBIN/GEOSgcm.x
@SINGULARITY_BUILD endif

@NATIVE_BUILD echo "Copying $EXPDIR/GEOSgcm.x to $SCRDIR"
@NATIVE_BUILD echo ""
@NATIVE_BUILD /bin/cp $EXPDIR/GEOSgcm.x $SCRDIR/GEOSgcm.x
@NATIVE_BUILD setenv GEOSEXE $SCRDIR/GEOSgcm.x

#######################################################################
#                         Get RESTARTS
#######################################################################

set rst_files      = `grep "RESTART_FILE"    AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_file_names = `grep "RESTART_FILE"    AGCM.rc | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2`

set chk_files      = `grep "CHECKPOINT_FILE" AGCM.rc | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_file_names = `grep "CHECKPOINT_FILE" AGCM.rc | grep -v "#" | cut -d ":" -f2`

set monthly_chk_names = `cat $EXPDIR/HISTORY.rc | grep -v '^[\t ]*#' | sed -n 's/\([^\t ]\+\).monthly:[\t ]*1.*/\1/p' | sed 's/$/_rst/' `

set dummy = `echo $rst_file_names`
set rst_file_names = ''
set tile_rsts = (catch catchcn route lake landice openwater saltwater seaicethermo)

# check if it resarts by face
# ----------------------------------
set rst_by_face = NO
if(-e $EXPDIR/fvcore_internal_rst & -e $EXPDIR/fvcore_internal_face_1_rst) then
  echo "grid-based internal_rst and internal_face_x_rst should not co-exist"
  echo "please remove all *internal_rst except these tile-based restarts :"
  foreach rst ( $tile_rsts )
	  echo ${rst}_internal_rst
  end
  exit
endif
if(-e $EXPDIR/fvcore_internal_face_1_rst) then
  set rst_by_face = YES
endif

set Rbyface = `grep READ_RESTART_BY_FACE: AGCM.rc | grep -v "#" |  cut -d ":" -f2`
if ($rst_by_face == NO) then
  if ($Rbyface == YES)  then
     sed -i '/READ_RESTART_BY_FACE:/c\READ_RESTART_BY_FACE: NO' AGCM.rc
  endif
else
  # make sure num_readers is multiple of 6
  @ num_readers = `grep NUM_READERS: AGCM.rc | grep -v "#" |  cut -d ":" -f2`
  @ remainer = $num_readers % 6
  if ($remainer != 0) then
     sed -i '/NUM_READERS:/c\NUM_READERS: 6' AGCM.rc
  endif

  if ($Rbyface != YES)  then
     sed -i '/READ_RESTART_BY_FACE:/c\READ_RESTART_BY_FACE: YES' AGCM.rc
  endif
endif

set Wbyface = `grep WRITE_RESTART_BY_FACE: AGCM.rc | grep -v "#" |  cut -d ":" -f2`
if ($Wbyface == YES)  then
  # make sure num_readers is multiple of 6
  @ num_writers = `grep NUM_WRITERS: AGCM.rc | grep -v "#" |  cut -d ":" -f2`
  @ remainer = $num_writers % 6
  if ($remainer != 0) then
     sed -i '/NUM_WRITERS:/c\NUM_WRITERS: 6' AGCM.rc
  endif
endif
# Remove possible bootstrap parameters (+/-)
# ------------------------------------------
foreach rst ( $dummy )
  set length  = `echo $rst | awk '{print length($0)}'`
  set    bit  = `echo $rst | cut -c1`
  if(  "$bit" == "+" | \
       "$bit" == "-" ) set rst = `echo $rst | cut -c2-$length`
  set is_tile_rst = FALSE
  if ($rst_by_face == YES) then
     foreach tile_rst ($tile_rsts)
       if ( $rst =~ *$tile_rst* ) then
         set is_tile_rst = TRUE
         break
       endif
     end
  endif
  if ($is_tile_rst == FALSE & $rst_by_face == YES) then
    set part1 = `echo $rst:q | sed 's/_rst/ /g'`
      foreach n (1 2 3 4 5 6)
         set rst = ${part1}_face_${n}_rst
         set rst_file_names = `echo $rst_file_names $rst`
      end
  else
    set rst_file_names = `echo $rst_file_names $rst`
  endif
end

# Get proper ridge scheme GWD internal restart
# --------------------------------------------
if ( $rst_by_face == YES ) then
  echo "WARNING: The generated gwd_internal_face_x_rst are used"
  #foreach n (1 2 3 4 5 6)
    #/bin/rm gwd_internal_face_${n}_rst
    #/bin/cp @GWDRSDIR/gwd_internal_c${AGCM_IM}_face_${n} gwd_internal_face_${n}_rst
  #end
else
  /bin/rm gwd_internal_rst
  /bin/cp @GWDRSDIR/gwd_internal_c${AGCM_IM} gwd_internal_rst
endif
@COUPLED /bin/mkdir INPUT
@COUPLED cp $EXPDIR/RESTART/* INPUT

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

# Link restarts to scratch
set sdate  = `awk '{print $1}' cap_restart`_`awk '{print $2}' cap_restart | cut -c1-4`z
foreach rst ( $rst_file_names $monthly_chk_names )
   if(-e $EXPDIR/restarts/${sdate}/${rst}.${sdate}.nc4 ) ln -s $EXPDIR/restarts/${sdate}/${rst}.${sdate}.nc4 $rst &
end
wait

/bin/rm -f  EGRESS

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
       @ yearf = $yearc + 1
       @ nymdf = $yearf * 10000 + 0101
        set oldstring = `grep '^\s*END_DATE:' CAP.rc`
        set newstring = "END_DATE: $nymdf $nhmsf"
        /bin/mv CAP.rc CAP.tmp
        cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
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

if( $AGCM_LM  != 72 ) then
    set files = `/bin/ls  *.yaml`
    foreach file ($files)
      cp $file dummy
      cat dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > $file
    end
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
if (       $ACHEM_TRUE == 0 && -e GEOSachem_ExtData.rc          ) /bin/mv          GEOSachem_ExtData.rc          GEOSachem_ExtData.rc.NOT_USED

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
   if ( -e $EXPDIR/saltwater_internal_rst ) /bin/cp $EXPDIR/saltwater_internal_rst $SCRDIR

   # The splitter script requires an OutData directory
   # -------------------------------------------------
   if (! -d OutData ) mkdir -p OutData

   # Run the script
   # --------------
   @SINGULARITY_BUILD $RUN_CMD 1 $SINGULARITY_RUN $GEOSBIN/SaltIntSplitter tile.data $SCRDIR/saltwater_internal_rst
   @NATIVE_BUILD $RUN_CMD 1 $GEOSBIN/SaltIntSplitter tile.data $SCRDIR/saltwater_internal_rst

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
   @SINGULARITY_BUILD set N_OPENW_TILES_FOUND = `$RUN_CMD 1 $SINGULARITY_RUN $GEOSBIN/rs_numtiles.x openwater_internal_rst | grep Total | awk '{print $NF}'`
   @NATIVE_BUILD set N_OPENW_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x openwater_internal_rst | grep Total | awk '{print $NF}'`

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

@MPT_SHEPHERD

# Run bundleParser.py
#---------------------
python3 bundleParser.py

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

     # Modify GAAS_GridComp_ExtData and Link REPLAY files
     # ---------------------------------------------
     /bin/mv -f GAAS_GridComp_ExtData.yaml GAAS_GridComp_ExtData.yaml.tmpl
     cat GAAS_GridComp_ExtData.yaml.tmpl | sed -e "s?das.aod_?chem/Y%y4/M%m2/${ANA_EXPID}.aod_?g" > GAAS_GridComp_ExtData.yaml

     /bin/mv -f GAAS_GridComp_ExtData.rc GAAS_GridComp_ExtData.rc.tmpl
     cat GAAS_GridComp_ExtData.rc.tmpl | sed -e "s?das.aod_?chem/Y%y4/M%m2/${ANA_EXPID}.aod_?g" > GAAS_GridComp_ExtData.rc

     /bin/ln -sf ${ANA_LOCATION}/chem .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE_TYPE} .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE09_TYPE} .

endif

# Establish safe default number of OpenMP threads
# -----------------------------------------------
@MIT # ---------------------------------------------------
@MIT # For MITgcm restarts - before running GEOSgcm.x
@MIT # ---------------------------------------------------
@MIT
@MIT # set time interval for segment in seconds
@MIT
@MIT set yearc  = `echo $nymdc | cut -c1-4`
@MIT set monthc = `echo $nymdc | cut -c5-6`
@MIT set dayc   = `echo $nymdc | cut -c7-8`
@MIT set hourc  = `echo $nhmsc | cut -c1-2`
@MIT set minutec = `echo $nhmsc | cut -c3-4`
@MIT set secondc = `echo $nhmsc | cut -c5-6`
@MIT
@MIT set yearf  = `echo $nymdf | cut -c1-4`
@MIT set monthf = `echo $nymdf | cut -c5-6`
@MIT set dayf   = `echo $nymdf | cut -c7-8`
@MIT set hourf  = `echo $nhmsf | cut -c1-2`
@MIT set minutef = `echo $nhmsf | cut -c3-4`
@MIT set secondf = `echo $nhmsf | cut -c5-6`
@MIT
@MIT set yearf = `echo $nymdf | cut -c1-4`
@MIT
@MIT set time1 = `date -u -d "${yearc}-${monthc}-${dayc}T${hourc}:${minutec}:${secondc}" "+%s"`
@MIT set time2 = `date -u -d "${yearf}-${monthf}-${dayf}T${hourf}:${minutef}:${secondf}" "+%s"`
@MIT
@MIT      @ mitdt = $time2 - $time1
@MIT echo "Segment time: $mitdt"
@MIT
@MIT
@MIT # Set-up MITgcm run directory
@MIT if (! -e mitocean_run) mkdir -p mitocean_run
@MIT cd mitocean_run
@MIT
@MIT # link mit configuration and initialization files
@MIT ln -sf $EXPDIR/mit_input/* .
@MIT # link mitgcm restarts if exist
@MIT /bin/ln -sf $EXPDIR/restarts/pic* .
@MIT # make an archive folder for mitgcm run
@MIT mkdir $EXPDIR/mit_output
@MIT
@MIT # Calculate segment time steps
@MIT set mit_nTimeSteps = `cat ${SCRDIR}/AGCM.rc | grep OGCM_RUN_DT: | cut -d: -f2 | tr -s " " | cut -d" " -f2`
@MIT @ mit_nTimeSteps = ${mitdt} / $mit_nTimeSteps
@MIT
@MIT #change namelist variables in data - nTimeSteps, chkptFreq and monitorFreq
@MIT sed -i "s/nTimeSteps.*/nTimeSteps       = ${mit_nTimeSteps},/" data
@MIT sed -i "s/chkptFreq.*/chkptFreq        = ${mitdt}.0,/" data
@MIT sed -i "s/pChkptFreq.*/pChkptFreq        = ${mitdt}.0,/" data
@MIT # get nIter0
@MIT
@MIT if (! -e ${EXPDIR}/restarts/MITgcm_restart_dates.txt ) then
@MIT   set nIter0 = `grep nIter0 data | tr -s " " | cut -d"=" -f2 | cut -d"," -f1 | awk '{$1=$1;print}'`
@MIT else
@MIT   set nIter0 = `grep "$nymdc $nhmsc" ${EXPDIR}/restarts/MITgcm_restart_dates.txt | cut -d" " -f5`
@MIT   if ( $nIter0 == "" ) then
@MIT     echo "No ocean restart file for $nymdc $nhmsc, exiting"
@MIT     echo "If this is a new initialized experiment, delete:"
@MIT     echo "${EXPDIR}/restarts/MITgcm_restart_dates.txt"
@MIT     echo "and restart"
@MIT     exit
@MIT   else
@MIT     sed -i "s/nIter0.*/ nIter0           = ${nIter0},/" data
@MIT   endif
@MIT endif
@MIT
@MIT cd ..
@MIT # ---------------------------------------------------
@MIT # End MITgcm restarts - before running GEOSgcm.x
@MIT # ---------------------------------------------------

# Set OMP_NUM_THREADS
# -------------------
setenv OMP_NUM_THREADS 1

# Run GEOSgcm.x
# -------------
if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

if( $USE_IOSERVER == 1 ) then
   set IOSERVER_OPTIONS = "--npes_model $MODEL_NPES --nodes_output_server $NUM_OSERVER_NODES"
   set IOSERVER_EXTRA   = "--oserver_type multigroup --npes_backend_pernode $NUM_BACKEND_PES"
else
   set IOSERVER_OPTIONS = ""
   set IOSERVER_EXTRA   = ""
endif

@SINGULARITY_BUILD @OCEAN_PRELOAD $RUN_CMD $TOTAL_PES $SINGULARITY_RUN $GEOSEXE $IOSERVER_OPTIONS $IOSERVER_EXTRA --logging_config 'logging.yaml'
@NATIVE_BUILD @OCEAN_PRELOAD $RUN_CMD $TOTAL_PES $GEOSEXE $IOSERVER_OPTIONS $IOSERVER_EXTRA --logging_config 'logging.yaml'

if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc

#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

# Move Intermediate Checkpoints to RESTARTS directory
# Use fvcore to find the dates
# ---------------------------------------------------
set dates = ( )
set files = `/bin/ls -1 *fvcore_internal_checkpoint*.nc4`
foreach file ($files)
  set date = `echo  $file | cut -d '.' -f2`
  set dates = ( ${dates} ${date} )
end

set checkpoints = `/bin/ls -1 *_checkpoint`
foreach  date ($dates)
   if (! -e ${EXPDIR}/restarts/${date} ) mkdir -p $EXPDIR/restarts/${date}
   foreach checkpoint ($checkpoints)
       mv  $checkpoint.$date.nc4 ${EXPDIR}/restarts/${date}/
   end
   set capdate = `echo $date | cut -d '_' -f1`
   set captime = `echo $date | cut -d '_' -f2 | cut -d 'z' -f1 `
   echo ${capdate}" "${captime}"00" > new_cap_restart
   cp new_cap_restart ${EXPDIR}/restarts/${date}/cap_restart
end


set edate  = `awk '{print $1}' cap_restart`_`awk '{print $2}' cap_restart | cut -c1-4`z
# Rename Final Checkpoints for moving to storage
# ------------------------------------
    set checkpoints = `/bin/ls -1 *_checkpoint`
foreach checkpoint ($checkpoints)
       /bin/mv            $checkpoint      ${checkpoint}.${edate}.nc4
       $GEOSBIN/stripname _checkpoint _rst ${checkpoint}.${edate}.nc4
end

# Remove Initial RESTARTS
# -----------------------
set restarts = `/bin/ls -1 *_rst`
/bin/rm -f $restarts

# mv Final Checkpoints to RESTARTS directory
# ----------------------------------------------------
if (! -e ${EXPDIR}/restarts/${edate} ) mkdir -p $EXPDIR/restarts/${edate}
    set  restarts = `/bin/ls -1 *_rst.${edate}.nc4`
foreach  restart ($restarts)
mv $restart ${EXPDIR}/restarts/${edate}/
end

cp cap_restart ${EXPDIR}/restarts/${edate}/

if (! -e ${EXPDIR}/restarts/iau_increments ) mkdir -p $EXPDIR/restarts/iau_increments
mv agcm_import* ${EXPDIR}/restarts/iau_increments/

#######################################################################
#               Move HISTORY Files to Holding Directory
#######################################################################

# Move current files to /holding
# ------------------------------
#cd $SCRDIR
#foreach collection ( $collections )
   #/bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection
#end

#######################################################################
#                 Run Post-Processing and Forecasts
#######################################################################

#$GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles

#if( $FSEGMENT != 00000000 ) then
     #set REPLAY_BEG_DATE = `grep '^\s*BEG_REPDATE:' $HOMDIR/CAP.rc | cut -d: -f2`
     #set REPLAY_END_DATE = `grep '^\s*END_REPDATE:' $HOMDIR/CAP.rc | cut -d: -f2`
     #set nday            = `echo $FSEGMENT | bc`
         #@ dt  = 10800 - 86400 * $nday
     #set date  = `$GEOSBIN/tick $nymdc $nhmsc $dt`
     #set nymdz =  $date[1]
     #set nhmsz =  $date[2]

     #if( $nymdz >= ${REPLAY_BEG_DATE} & \
         #$nymdz <= ${REPLAY_END_DATE} ) then
         #setenv CYCLED .TRUE.
         #$EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $nday TRUE
     #endif
#endif

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

cp cap_restart $EXPDIR/cap_restart

if ( $rc == 0 ) then
      cd  $HOMDIR
      if( $capdate < $enddate ) @BATCH_CMD $HOMDIR/gcm_run.j
endif

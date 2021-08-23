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
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${GEOSDIR}/lib

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

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

if( $GCMEMIP == TRUE ) then
    if (! -e $EXPDIR/restarts/$RSTDATE ) mkdir -p $EXPDIR/restarts/$RSTDATE
    setenv  SCRDIR  $EXPDIR/scratch.$RSTDATE
else
    setenv  SCRDIR  $EXPDIR/scratch
endif

if (! -e $SCRDIR ) mkdir -p $SCRDIR

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep      "^ *NX:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY  = `grep      "^ *NY:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM  = `grep      "AGCM_IM:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM  = `grep      "AGCM_JM:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM  = `grep      "AGCM_LM:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM  = `grep      "OGCM\.IM_WORLD:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM  = `grep      "OGCM\.JM_WORLD:" $HOMDIR/AGCM.rc | cut -d':' -f2`

>>>COUPLED<<<set  OGCM_LM  = `grep "OGCM\.LM:" $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set       NX = `grep  "OGCM\.NX:" $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set       NY = `grep  "OGCM\.NY:" $HOMDIR/AGCM.rc | cut -d':' -f2`

# Calculate number of cores/nodes for IOSERVER
# --------------------------------------------

set USE_IOSERVER   = @USE_IOSERVER
set AGCM_IOS_NODES = `grep IOSERVER_NODES: $HOMDIR/AGCM.rc | cut -d':' -f2`

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

if ( $NCPUS != NULL ) then

   if ( $USE_IOSERVER == 1 ) then

      set NCPUS_PER_NODE = @NCPUS_PER_NODE

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

set      FILE = strip
/bin/rm $FILE
cat << EOF > $FILE
#!/bin/ksh
/bin/mv \$1 \$1.tmp
touch   \$1
while read line
do
echo \$line >> \$1
done < \$1.tmp
exit
EOF
chmod +x $FILE

@CPEXEC $HOMDIR/CAP.rc .
./strip         CAP.rc

set year  = `echo $RSTDATE | cut -d_ -f1 | cut -b1-4`
set month = `echo $RSTDATE | cut -d_ -f1 | cut -b5-6`

>>>EMIP_OLDLAND<<<# Copy MERRA-2 Restarts
>>>EMIP_OLDLAND<<<# ---------------------
>>>EMIP_NEWLAND<<<# Copy Jason-3_4 REPLAY MERRA-2 NewLand Restarts
>>>EMIP_NEWLAND<<<# ----------------------------------------------
@CPEXEC /discover/nobackup/projects/gmao/g6dev/ltakacs/@EMIP_MERRA2/restarts/AMIP/M${month}/restarts.${year}${month}.tar .
@TAREXEC xf  restarts.${year}${month}.tar
/bin/rm restarts.${year}${month}.tar
>>>EMIP_OLDLAND<<</bin/rm MERRA2*bin


>>>EMIP_OLDLAND<<<# Regrid MERRA-2 Restarts
>>>EMIP_OLDLAND<<<# -----------------------
>>>EMIP_NEWLAND<<<# Regrid Jason-3_4 REPLAY MERRA-2 NewLand Restarts
>>>EMIP_NEWLAND<<<# ------------------------------------------------
set RSTID = `/bin/ls *catch* | cut -d. -f1`
set day   = `/bin/ls *catch* | cut -d. -f3 | cut -b 7-8`
$GEOSBIN/regrid.pl -np -ymd ${year}${month}${day} -hr 21 -grout C${AGCM_IM} -levsout ${AGCM_LM} -outdir . -d . -expid $RSTID -tagin @EMIP_BCS_IN -oceanin e -i -nobkg -lbl -nolcv -tagout @LSMBCS -rs 3 -oceanout @OCEANOUT
>>>EMIP_OLDLAND<<</bin/rm $RSTID.*.bin

     set IMC = $AGCM_IM
if(     $IMC < 10 ) then
     set IMC = 000$IMC
else if($IMC < 100) then
     set IMC = 00$IMC
else if($IMC < 1000) then
     set IMC = 0$IMC
endif

set  chk_type = `/usr/bin/file -Lb --mime-type C${AGCM_IM}e_${RSTID}.*catch*`
if( "$chk_type" =~ "application/octet-stream" ) set ext = bin
if( "$chk_type" =~ "application/x-hdf"        ) set ext = nc4

$GEOSBIN/stripname C${AGCM_IM}@OCEANOUT_${RSTID}.
$GEOSBIN/stripname .${year}${month}${day}_21z.$ext.@LSMBCS_@BCSTAG.@ATMOStag_@OCEANtag
>>>EMIP_OLDLAND<<</bin/mv gocart_internal_rst gocart_internal_rst.merra2
>>>EMIP_OLDLAND<<<$GEOSBIN/gogo.x -s $RSTID.Chem_Registry.rc.${year}${month}${day}_21z -t $EXPDIR/RC/Chem_Registry.rc -i gocart_internal_rst.merra2 -o gocart_internal_rst -r C${AGCM_IM} -l ${AGCM_LM}


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
set oldstring =  `cat CAP.rc | grep END_DATE:`
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
                             /bin/ln -sf $EXPDIR/RC/* .
                             @CPEXEC     $EXPDIR/cap_restart .
                             @CPEXEC -f  $HOMDIR/*.rc .
                             @CPEXEC -f  $HOMDIR/*.nml .
                             @CPEXEC -f  $HOMDIR/*.yaml .
                             @CPEXEC     $GEOSBIN/bundleParser.py .

                             cat fvcore_layout.rc >> input.nml

			    >>>MOM6<<<@CPEXEC -f  $HOMDIR/MOM_input .
			    >>>MOM6<<<@CPEXEC -f  $HOMDIR/MOM_override .

if( $GCMEMIP == TRUE ) then
    @CPEXEC -f  $EXPDIR/restarts/$RSTDATE/cap_restart .
    @CPEXEC -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
endif

set END_DATE  = `grep     END_DATE:  CAP.rc | cut -d':' -f2`
set NUM_SGMT  = `grep     NUM_SGMT:  CAP.rc | cut -d':' -f2`
set FSEGMENT  = `grep FCST_SEGMENT:  CAP.rc | cut -d':' -f2`
set USE_SHMEM = `grep    USE_SHMEM:  CAP.rc | cut -d':' -f2`

#######################################################################
#         Create Strip Utility to Remove Multiple Blank Spaces
#######################################################################

set      FILE = strip
/bin/rm $FILE
cat << EOF > $FILE
#!/bin/ksh
/bin/mv \$1 \$1.tmp
touch   \$1
while read line
do
echo \$line >> \$1
done < \$1.tmp
exit
EOF
chmod +x $FILE

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
>>>DATAOCEAN<<<setenv SSTDIR    @SSTDIR
setenv CHMDIR    @CHMDIR
>>>DATAOCEAN<<<setenv BCRSLV    @ATMOStag_@OCEANtag
>>>COUPLED<<<setenv BCRSLV    @ATMOStag_DE0360xPE0180
setenv DATELINE  DC
setenv EMISSIONS @EMISSIONS

>>>MOM5<<<setenv GRIDDIR  @COUPLEDIR/a${AGCM_IM}x${AGCM_JM}_o${OGCM_IM}x${OGCM_JM}
>>>MOM6<<<setenv GRIDDIR  @COUPLEDIR/MOM6/@ATMOStag_@OCEANtag
>>>COUPLED<<<setenv GRIDDIR2  @COUPLEDIR/SST/MERRA2/${OGCM_IM}x${OGCM_JM}
>>>COUPLED<<<setenv BCTAG `basename $GRIDDIR`
>>>DATAOCEAN<<<setenv BCTAG `basename $BCSDIR`

set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -f

>>>COUPLED<<</bin/mkdir -p RESTART
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

>>>COUPLED<<</bin/ln -sf $GRIDDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/@ATMOStag_@OCEANtag-Pfafstetter.til   tile.data
>>>MIT<<</bin/ln -sf $GRIDDIR/@ATMOStag_@OCEANtag-Runoff.bin   runoff.bin
>>>MIT<<</bin/ln -sf $GRIDDIR/mit.ascii .
>>>MOM5<<</bin/ln -sf $GRIDDIR/@ATMOStag_@OCEANtag-Pfafstetter.TRN   runoff.bin
>>>MOM5<<</bin/ln -sf $GRIDDIR/MAPL_Tripolar.nc .
>>>MOM6<<</bin/ln -sf $GRIDDIR/@ATMOStag_@OCEANtag-Pfafstetter.TRN   runoff.bin
>>>MOM6<<</bin/ln -sf $GRIDDIR/MAPL_Tripolar.nc .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
>>>MOM5<<</bin/ln -s @COUPLEDIR/a@HIST_IMx@HIST_JM_o${OGCM_IM}x${OGCM_JM}/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data
>>>MOM6<<</bin/ln -s @COUPLEDIR/MOM6/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data

# Precip correction
#/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP

>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.til  tile.data
>>>DATAOCEAN<<<if(     -e  $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL) then
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL  tile.bin
>>>DATAOCEAN<<<endif

# DAS or REPLAY Mode (AGCM.rc:  pchem_clim_years = 1-Year Climatology)
# --------------------------------------------------------------------
#/bin/ln -sf $BCSDIR/Shared/pchem.species.Clim_Prod_Loss.z_721x72.nc4 species.data

# CMIP-5 Ozone Data (AGCM.rc:  pchem_clim_years = 228-Years)
# ----------------------------------------------------------
#/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data

# S2S pre-industrial with prod/loss of stratospheric water vapor
# (AGCM.rc:  pchem_clim_years = 3-Years,  and  H2O_ProdLoss: 1 )
# --------------------------------------------------------------
#/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-6.wH2OandPL.1850s.z_91x72.nc4 species.data

# MERRA-2 Ozone Data (AGCM.rc:  pchem_clim_years = 39-Years)
# ----------------------------------------------------------
/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.MERRA2OX.197902-201706.z_91x72.nc4 species.data

/bin/ln -sf $BCSDIR/Shared/*bin .
/bin/ln -sf $BCSDIR/Shared/*c2l*.nc4 .

/bin/ln -sf $BCSDIR/$BCRSLV/visdf_@RES_DATELINE.dat visdf.dat
/bin/ln -sf $BCSDIR/$BCRSLV/nirdf_@RES_DATELINE.dat nirdf.dat
/bin/ln -sf $BCSDIR/$BCRSLV/vegdyn_@RES_DATELINE.dat vegdyn.data
/bin/ln -sf $BCSDIR/$BCRSLV/lai_clim_@RES_DATELINE.data lai.data
/bin/ln -sf $BCSDIR/$BCRSLV/green_clim_@RES_DATELINE.data green.data
/bin/ln -sf $BCSDIR/$BCRSLV/ndvi_clim_@RES_DATELINE.data ndvi.data
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODISVISmean_${AGCM_IM}x${AGCM_JM}.dat ) /bin/ln -s MODISVISmean.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODISVISstd_${AGCM_IM}x${AGCM_JM}.dat  ) /bin/ln -s MODISVISstd.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODISNIRmean_${AGCM_IM}x${AGCM_JM}.dat ) /bin/ln -s MODISNIRmean.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODISNIRstd_${AGCM_IM}x${AGCM_JM}.dat  ) /bin/ln -s MODISNIRstd.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODELFPARmean_${AGCM_IM}x${AGCM_JM}.dat) /bin/ln -s MODELFPARmean.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODELFPARstd_${AGCM_IM}x${AGCM_JM}.dat ) /bin/ln -s MODELFPARstd.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODISFPARmean_${AGCM_IM}x${AGCM_JM}.dat) /bin/ln -s MODISFPARmean.dat
>>>GCMRUN_CATCHCN<<<if (-f $BCSDIR/$BCRSLV/MODISFPARstd_${AGCM_IM}x${AGCM_JM}.dat ) /bin/ln -s MODISFPARstd.dat
>>>GCMRUN_CATCHCN<<</bin/ln -s /discover/nobackup/projects/gmao/ssd/land/l_data/LandBCs_files_for_mkCatchParam/V001/CO2_MonthlyMean_DiurnalCycle.nc4
>>>GCMRUN_CATCHCN<<</bin/ln -s /discover/nobackup/projects/gmao/ssd/land/l_data/LandBCs_files_for_mkCatchParam/V001/FPAR_CDF_Params-M09.nc4

/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data

>>>FVCUBED<<<if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
>>>FVCUBED<<</bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
>>>FVCUBED<<<endif

>>>COUPLED<<<@CPEXEC $HOMDIR/*_table .
>>>COUPLED<<<@CPEXEC $GRIDDIR/INPUT/* INPUT
>>>COUPLED<<</bin/ln -sf $GRIDDIR/cice/kmt_cice.bin .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/cice/grid_cice.bin .

_EOF_

>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@SSTFILE   sst.data' >> $FILE
>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@ICEFILE fraci.data' >> $FILE
>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@KPARFILE SEAWIFS_KPAR_mon_clim.data' >> $FILE

chmod +x linkbcs
@CPEXEC  linkbcs $EXPDIR

#######################################################################
#                    Get Executable and RESTARTS 
#######################################################################

@CPEXEC $EXPDIR/GEOSgcm.x .

set rst_files      = `cat AGCM.rc | grep "RESTART_FILE"    | grep -v VEGDYN | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_file_names = `cat AGCM.rc | grep "RESTART_FILE"    | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2`

set chk_files      = `cat AGCM.rc | grep "CHECKPOINT_FILE" | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_file_names = `cat AGCM.rc | grep "CHECKPOINT_FILE" | grep -v "#" | cut -d ":" -f2`

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
    foreach rst ( $rst_file_names )
      if(-e $EXPDIR/restarts/$RSTDATE/$rst ) @CPEXEC $EXPDIR/restarts/$RSTDATE/$rst . &
    end
else
    foreach rst ( $rst_file_names )
      if(-e $EXPDIR/$rst ) @CPEXEC $EXPDIR/$rst . &
    end
endif
wait

>>>COUPLED<<</bin/mkdir INPUT
>>>COUPLED<<<@CPEXEC $EXPDIR/RESTART/* INPUT

# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_file_names )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            @CPEXEC $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
>>>COUPLED<<<   @CPEXEC -r $EXPDIR/RESTART ${EXPDIR}/restarts/RESTART.${edate}
   cd $EXPDIR/restarts
      >>>DATAOCEAN<<<@TAREXEC cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
      >>>COUPLED<<<@TAREXEC cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} RESTART.${edate}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
     >>>COUPLED<<</bin/rm -rf RESTART.${edate}
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
    @CPEXEC -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
else
    @CPEXEC -f $HOMDIR/CAP.rc .
endif

./strip CAP.rc

# Set Time Variables for Current_(c), Ending_(e), and Segment_(s) dates 
# ---------------------------------------------------------------------
set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set nymde = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c2-9`
set nhmse = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c11-16`
set nymds = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c2-9`
set nhmss = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c11-16`

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
        set oldstring = `cat CAP.rc | grep END_DATE:`
        set newstring = "END_DATE: $nymdf $nhmsf"
        /bin/mv CAP.rc CAP.tmp
        cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
    endif
endif

# Select proper MERRA-2 GOCART Emission RC Files
# (NOTE: MERRA2-DD has same transition date)
# ----------------------------------------------
if( ${EMISSIONS} == MERRA2 | \
    ${EMISSIONS} == MERRA2-DD ) then
    set MERRA2_Transition_Date = 20000401

    if( $nymdc < ${MERRA2_Transition_Date} ) then
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/etc/$EMISSIONS/19600101-20000331
         if( $nymdf > ${MERRA2_Transition_Date} ) then
          set nymdf = ${MERRA2_Transition_Date}
          set oldstring = `cat CAP.rc | grep END_DATE:`
          set newstring = "END_DATE: $nymdf $nhmsf"
          /bin/mv CAP.rc CAP.tmp
                     cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
         endif
    else
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/etc/$EMISSIONS/20000401-present
    endif

    if( $AGCM_LM == 72 ) then
        @CPEXEC --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          @CPEXEC $file dummy
              cat       dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > `basename $file`
        end
    endif

endif

# Rename big ExtData files that are not needed
# --------------------------------------------
set            SC_TRUE = `grep -i "^ *ENABLE_STRATCHEM *: *\.TRUE\."     GEOS_ChemGridComp.rc | wc -l`
if (          $SC_TRUE == 0 && -e StratChem_ExtData.rc          ) /bin/mv          StratChem_ExtData.rc          StratChem_ExtData.rc.NOT_USED
set           GMI_TRUE = `grep -i "^ *ENABLE_GMICHEM *: *\.TRUE\."       GEOS_ChemGridComp.rc | wc -l`
if (         $GMI_TRUE == 0 && -e GMI_ExtData.rc                ) /bin/mv                GMI_ExtData.rc                GMI_ExtData.rc.NOT_USED
set           GCC_TRUE = `grep -i "^ *ENABLE_GEOSCHEM *: *\.TRUE\."      GEOS_ChemGridComp.rc | wc -l`
if (         $GCC_TRUE == 0 && -e GEOSCHEMchem_ExtData.rc       ) /bin/mv       GEOSCHEMchem_ExtData.rc       GEOSCHEMchem_ExtData.rc.NOT_USED
set         CARMA_TRUE = `grep -i "^ *ENABLE_CARMA *: *\.TRUE\."         GEOS_ChemGridComp.rc | wc -l`
if (       $CARMA_TRUE == 0 && -e CARMAchem_GridComp_ExtData.rc ) /bin/mv CARMAchem_GridComp_ExtData.rc CARMAchem_GridComp_ExtData.rc.NOT_USED
set           DNA_TRUE = `grep -i "^ *ENABLE_DNA *: *\.TRUE\."           GEOS_ChemGridComp.rc | wc -l`
if (         $DNA_TRUE == 0 && -e DNA_ExtData.rc                ) /bin/mv                DNA_ExtData.rc                DNA_ExtData.rc.NOT_USED
set         ACHEM_TRUE = `grep -i "^ *ENABLE_ACHEM *: *\.TRUE\."         GEOS_ChemGridComp.rc | wc -l`
if (       $ACHEM_TRUE == 0 && -e GEOSachem_ExtData.rc          ) /bin/mv          GEOSachem_ExtData.rc          GEOSachem_ExtData.rc.NOT_USED
set   GOCART_DATA_TRUE = `grep -i "^ *ENABLE_GOCART_DATA *: *\.TRUE\."   GEOS_ChemGridComp.rc | wc -l`
if ( $GOCART_DATA_TRUE == 0 && -e GOCARTdata_ExtData.rc         ) /bin/mv         GOCARTdata_ExtData.rc         GOCARTdata_ExtData.rc.NOT_USED

# Generate the complete ExtData.rc
# --------------------------------
if(-e ExtData.rc )    /bin/rm -f   ExtData.rc
set  extdata_files = `/bin/ls -1 *_ExtData.rc`
cat $extdata_files > ExtData.rc 

# Link Boundary Conditions for Appropriate Date
# ---------------------------------------------
setenv YEAR $yearc
./linkbcs

if (! -e tile.bin) then
$GEOSBIN/binarytile.x tile.data tile.bin
>>>COUPLED<<<$GEOSBIN/binarytile.x tile_hist.data tile_hist.bin
endif

# If running in dual ocean mode, link sst and fraci data here
#set yy  = `cat cap_restart | cut -c1-4`
#echo $yy
#ln -sf $GRIDDIR2/dataoceanfile_MERRA2_SST.${OGCM_IM}x${OGCM_JM}.${yy}.data sst.data
#ln -sf $GRIDDIR2/dataoceanfile_MERRA2_ICE.${OGCM_IM}x${OGCM_JM}.${yy}.data fraci.data

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
   @CPEXEC openwater_internal_rst    $EXPID.openwater_internal_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
   @CPEXEC seaicethermo_internal_rst $EXPID.seaicethermo_internal_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}

   # Inject decorated copies into restarts tarball
   # ---------------------------------------------
   @TAREXEC rf $EXPDIR/restarts/restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}

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

   set N_OPENW_TILES_EXPECTED = `grep '^ *0' tile.data | wc -l`
   set N_OPENW_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x openwater_internal_rst | grep Total | awk '{print $NF}'`
         
   if ( $N_OPENW_TILES_EXPECTED != $N_OPENW_TILES_FOUND ) then
      echo "Error! Found $N_OPENW_TILES_FOUND tiles in openwater. Expect to find $N_OPENW_TILES_EXPECTED tiles."
      echo "Your restarts are probably for a different ocean."
      exit 7
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
set  REPLAY_MODE = `grep REPLAY_MODE: AGCM.rc | grep -v '#' | cut -d: -f2`
if( $REPLAY_MODE == 'Exact' | $REPLAY_MODE == 'Regular' ) then

     set ANA_EXPID    = `grep REPLAY_ANA_EXPID:    AGCM.rc | grep -v '#'   | cut -d: -f2`
     set ANA_LOCATION = `grep REPLAY_ANA_LOCATION: AGCM.rc | grep -v '#'   | cut -d: -f2`

     set REPLAY_FILE        = `grep REPLAY_FILE:   AGCM.rc | grep -v '#'   | cut -d: -f2`
     set REPLAY_FILE09      = `grep REPLAY_FILE09: AGCM.rc | grep -v '#'   | cut -d: -f2`
     set REPLAY_FILE_TYPE   = `echo $REPLAY_FILE           | cut -d"/" -f1 | grep -v %`
     set REPLAY_FILE09_TYPE = `echo $REPLAY_FILE09         | cut -d"/" -f1 | grep -v %`

     # Modify GAAS_GridComp.rc and Link REPLAY files
     # ---------------------------------------------
     /bin/mv -f GAAS_GridComp.rc GAAS_GridComp.tmp
     cat GAAS_GridComp.tmp | sed -e "s?aod/Y%y4/M%m2/@ANA_EXPID.?aod/Y%y4/M%m2/${ANA_EXPID}.?g" > GAAS_GridComp.rc

     /bin/ln -sf ${ANA_LOCATION}/aod .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE_TYPE} .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE09_TYPE} .

endif

# ---------------------------------------------------
# For MITgcm restarts - before running GEOSgcm.x
# ---------------------------------------------------

# set time interval for segment in seconds

set yearc  = `echo $nymdc | cut -c1-4`
set monthc = `echo $nymdc | cut -c5-6`
set dayc   = `echo $nymdc | cut -c7-8`
set hourc  = `echo $nhmsc | cut -c1-2`
set minutec = `echo $nhmsc | cut -c3-4`
set secondc = `echo $nhmsc | cut -c5-6`

set yearf  = `echo $nymdf | cut -c1-4`
set monthf = `echo $nymdf | cut -c5-6`
set dayf   = `echo $nymdf | cut -c7-8`
set hourf  = `echo $nhmsf | cut -c1-2`
set minutef = `echo $nhmsf | cut -c3-4`
set secondf = `echo $nhmsf | cut -c5-6`

set yearf = `echo $nymdf | cut -c1-4`

set time1 = `date -u -d "${yearc}-${monthc}-${dayc}T${hourc}:${minutec}:${secondc}" "+%s"`
set time2 = `date -u -d "${yearf}-${monthf}-${dayf}T${hourf}:${minutef}:${secondf}" "+%s"`

     @ mitdt = $time2 - $time1
echo "Segment time: $mitdt"


# Set-up MITgcm run directory
if (! -e mitocean_run) mkdir -p mitocean_run
cd mitocean_run

# link mit configuration and initialization files
ln -sf $EXPDIR/mit_input/* .
# link mitgcm restarts if exist
/bin/ln -sf $EXPDIR/restarts/pic* .
# make an archive folder for mitgcm run
mkdir $EXPDIR/mit_output

# Calculate segment time steps
set mit_nTimeSteps = `cat ${SCRDIR}/AGCM.rc | grep OGCM_RUN_DT: | cut -d: -f2 | tr -s " " | cut -d" " -f2`
@ mit_nTimeSteps = ${mitdt} / $mit_nTimeSteps

#change namelist variables in data - nTimeSteps, chkptFreq and monitorFreq
sed -i "s/nTimeSteps.*/nTimeSteps       = ${mit_nTimeSteps},/" data
sed -i "s/chkptFreq.*/chkptFreq        = ${mitdt}.0,/" data
sed -i "s/pChkptFreq.*/pChkptFreq        = ${mitdt}.0,/" data
# get nIter0

if (! -e ${EXPDIR}/restarts/MITgcm_restart_dates.txt ) then
  set nIter0 = `grep nIter0 data | tr -s " " | cut -d"=" -f2 | cut -d"," -f1 | awk '{$1=$1;print}'`
else
  set nIter0 = `grep "$nymdc $nhmsc" ${EXPDIR}/restarts/MITgcm_restart_dates.txt | cut -d" " -f5`
  if ( $nIter0 == "" ) then
    echo "No ocean restart file for $nymdc $nhmsc, exiting"
    echo "If this is a new initialized experiment, delete:"
    echo "${EXPDIR}/restarts/MITgcm_restart_dates.txt"
    echo "and restart"
    exit
  else
    sed -i "s/nIter0.*/ nIter0           = ${nIter0},/" data
  endif
endif

cd ..
# ---------------------------------------------------
# End MITgcm restarts - before running GEOSgcm.x
# ---------------------------------------------------

# Set OMP_NUM_THREADS
# -------------------
setenv OMP_NUM_THREADS 1

# Run GEOSgcm.x
# -------------
if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

if( $USE_IOSERVER == 1) then
   set IOSERVER_OPTIONS = "--npes_model $MODEL_NPES --nodes_output_server $IOS_NODES"
else
   set IOSERVER_OPTIONS = ""
endif

$RUN_CMD $NPES ./GEOSgcm.x $IOSERVER_OPTIONS --logging_config 'logging.yaml'

if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null

@GPUEND

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc

# ---------------------------------------------------
# For MITgcm restarts - after running GEOSgcm.x
# ---------------------------------------------------

set STEADY_STATE_OCEAN=`grep STEADY_STATE_OCEAN AGCM.rc | cut -d':' -f2 | tr -d " "`

# update ocean only if activated. Otherwize use the same pickups (passive ocean).
if ( ${STEADY_STATE_OCEAN} != 0 ) then

  if ( ${rc} == 0 ) then

    # Update nIter0 for next segment
    set znIter00 = `echo $nIter0 | awk '{printf("%010d",$1)}'`
    @ nIter0 = $nIter0 + $mit_nTimeSteps
    set znIter0 = `echo $nIter0 | awk '{printf("%010d",$1)}'`

    # to update MITgcm restart list file
    sed -i "/${nIter0}/d" ${EXPDIR}/restarts/MITgcm_restart_dates.txt
    echo "Date_GEOS5 $nymdf $nhmsf NITER0_MITgcm ${nIter0}" >> ${EXPDIR}/restarts/MITgcm_restart_dates.txt

    /bin/mv $SCRDIR/mitocean_run/STDOUT.0000 $EXPDIR/mit_output/STDOUT.${znIter00}

  endif

  cd $SCRDIR/mitocean_run

  # Check existance of roling pickups
  set nonomatch rp =  ( pickup*ckptA* )
  echo $rp
  # Rename and move them if exist
  if ( -e $rp[1] ) then
    set timeStepNumber=`cat pickup.ckptA.meta | grep timeStepNumber | tr -s " " | cut -d" " -f5 | awk '{printf("%010d",$1)}'`
    foreach fname ( pickup*ckptA* )
      set bname = `echo ${fname} | cut -d "." -f1 | cut -d "/" -f2`
      set aname = `echo ${fname} | cut -d "." -f3`
      echo $EXPDIR/restarts/${bname}.${timeStepNumber}.${aname}
      /bin/mv ${fname} $EXPDIR/restarts/${bname}.${timeStepNumber}.${aname}
    end
  endif

  # Check existance of permanent pickups
  set nonomatch pp =  ( pickup* )
  echo $pp
  # Move them if exist
  if ( -e $pp[1] ) then
    foreach fname ( pickup* )
      if ( ! -e $EXPDIR/restarts/${fname} ) /bin/mv ${fname} $EXPDIR/restarts/${fname}
    end
  endif

  /bin/mv T.* $EXPDIR/mit_output/
  /bin/mv S.* $EXPDIR/mit_output/
  /bin/mv U.* $EXPDIR/mit_output/
  /bin/mv V.* $EXPDIR/mit_output/
  /bin/mv W.* $EXPDIR/mit_output/
  /bin/mv PH* $EXPDIR/mit_output/
  /bin/mv Eta.* $EXPDIR/mit_output/

  /bin/mv AREA.* $EXPDIR/mit_output/
  /bin/mv HEFF.* $EXPDIR/mit_output/
  /bin/mv HSNOW.* $EXPDIR/mit_output/
  /bin/mv UICE.* $EXPDIR/mit_output/
  /bin/mv VICE.* $EXPDIR/mit_output/

  #copy mit output to mit_output
  foreach i (`grep -i filename data.diagnostics  | grep "^ " | cut -d"=" -f2 | cut -d"'" -f2 | awk '{$1=$1;print}'`)
   /bin/mv ${i}* $EXPDIR/mit_output/
  end

  foreach i (`grep -i stat_fName data.diagnostics | grep "^ " | cut -d"=" -f2 | cut -d"'" -f2 | awk '{$1=$1;print}'`)
   /bin/mv ${i}* $EXPDIR/mit_output/
  end

  cd $SCRDIR

endif

# ---------------------------------------------------
# End MITgcm restarts - after running GEOSgcm.x
# ---------------------------------------------------

 
#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

>>>COUPLED<<<@CPEXEC -r RESTART ${EXPDIR}/restarts/RESTART.${edate}
>>>COUPLED<<<@CPEXEC RESTART/* INPUT

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
@CPEXEC $restart ${EXPDIR}/restarts
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
	>>>DATAOCEAN<<<@TAREXEC cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
        >>>COUPLED<<<@TAREXEC cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.* RESTART.${edate}
        /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
	>>>COUPLED<<</bin/rm -rf RESTART.${edate}
    endif
cd $SCRDIR

# Move monthly collection checkpoints to restarts
#------------------------------------------------
set monthlies = `/bin/ls *chk` 
if ( $#monthlies > 0 ) then
    foreach ff (*chk)
	    /bin/mv $ff `basename $ff chk`rst
    end
endif

#######################################################################
#               Move HISTORY Files to Holding Directory
#######################################################################

# Check for files waiting in /holding
# -----------------------------------
set     waiting_files = `/bin/ls -1 $EXPDIR/holding/*/*nc4`
set num_waiting_files = $#waiting_files

# Move current files to /holding
# ------------------------------
foreach collection ( $collections )
   /bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection
end

>>>COUPLED<<<# MOM-Specific Output Files
>>>COUPLED<<<# -------------------------
>>>MOM5<<< set dsets="ocean_month"
>>>MOM6<<< set dsets="ocean_state prog_z sfc_ave forcing"
>>>COUPLED<<< foreach dset ( $dsets )
>>>COUPLED<<< set num = `/bin/ls -1 $dset.nc | wc -l`
>>>COUPLED<<< if($num != 0) then
>>>COUPLED<<<    if(! -e $EXPDIR/MOM_Output) mkdir -p $EXPDIR/MOM_Output
>>>COUPLED<<<    /bin/mv $SCRDIR/$dset.nc $EXPDIR/MOM_Output/$dset.${edate}.nc
>>>COUPLED<<< endif
>>>COUPLED<<< end
>>>COUPLED<<<
#######################################################################
#                 Run Post-Processing and Forecasts
#######################################################################

$GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles
 
if( $FSEGMENT != 00000000 ) then
     set REPLAY_BEG_DATE = `grep BEG_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set REPLAY_END_DATE = `grep END_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set nday            = `echo $FSEGMENT | /bin/grep -Po '\d+' | bc`
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

end

#######################################################################
#                              Re-Submit Job
#######################################################################

if( $GCMEMIP == TRUE ) then
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/$rst
     end
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       @CPEXEC $rst $EXPDIR/restarts/$RSTDATE/$rst &
     end
     wait
     @CPEXEC cap_restart $EXPDIR/restarts/$RSTDATE/cap_restart
else
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/$rst
     end
        /bin/rm -f $EXPDIR/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       @CPEXEC $rst $EXPDIR/$rst &
     end
     wait
     @CPEXEC cap_restart $EXPDIR/cap_restart
endif

>>>COUPLED<<<@CPEXEC -rf RESTART $EXPDIR

if ( $rc == 0 ) then
      cd  $HOMDIR
      if( $GCMEMIP == TRUE ) then
          if( $capdate < $enddate ) @BATCH_CMD $HOMDIR/gcm_run.j$RSTDATE
      else
          if( $capdate < $enddate ) @BATCH_CMD $HOMDIR/gcm_run.j
      endif
endif

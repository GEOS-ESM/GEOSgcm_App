#!/bin/csh -fx

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#SBATCH --time=@RUN_T
#@RUN_P
#SBATCH --job-name=@RUN_N
#@RUN_SP
#@RUN_SN
#SBATCH -A g0609

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
setenv RUN_CMD         "@RUN_CMD"
setenv GCMVER           @GCMVER

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    EXPID   @EXPID
setenv    EXPDIR  @EXPDIR
setenv    HOMDIR  @HOMDIR
setenv    SCRDIR  $EXPDIR/scratch

>>>FORCAST<<<set SUBSEASONAL = @FCSTMODE
>>>FORCAST<<<set QFEDCLIM = @FCSTQFED
#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################

if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot
if (! -e $SCRDIR            ) mkdir -p $SCRDIR

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep           NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY  = `grep           NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM  = `grep      AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM  = `grep      AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM  = `grep      AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM  = `grep      OGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM  = `grep      OGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set END_DATE  = `grep     END_DATE:  $HOMDIR/CAP.rc | cut -d':' -f2`
set NUM_SGMT  = `grep     NUM_SGMT:  $HOMDIR/CAP.rc | cut -d':' -f2`
set FSEGMENT  = `grep FCST_SEGMENT:  $HOMDIR/CAP.rc | cut -d':' -f2`
set USE_SHMEM = `grep    USE_SHMEM:  $HOMDIR/CAP.rc | cut -d':' -f2`

>>>COUPLED<<<set  OGCM_LM  = `grep OGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set       NX = `grep  OGCM_NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set       NY = `grep  OGCM_NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set  HIST_IM = @HIST_IM
>>>COUPLED<<<set  HIST_JM = @HIST_JM

# Check for Over-Specification of CPU Resources
# ---------------------------------------------
  if ($?PBS_NODEFILE) then
     set  NCPUS = `cat $PBS_NODEFILE | wc -l`
     @    NPES  = $NX * $NY
        if( $NPES > $NCPUS ) then
             echo "CPU Resources are Over-Specified"
             echo "--------------------------------"
             echo "Allotted NCPUs: $NCPUS"
             echo "Specified  NX : $NX"
             echo "Specified  NY : $NY"
             exit
        endif
     endif
  endif

# Set ATMOS and OCEAN Horizontal Resolution Tags
# ----------------------------------------------
set AGCM_IM_Tag = `echo $AGCM_IM | awk '{printf "%4.4i", $1}'`
set AGCM_JM_Tag = `echo $AGCM_JM | awk '{printf "%4.4i", $1}'`
set OGCM_IM_Tag = `echo $OGCM_IM | awk '{printf "%4.4i", $1}'`
set OGCM_JM_Tag = `echo $OGCM_JM | awk '{printf "%4.4i", $1}'`
>>>COUPLED<<<set HIST_IM_Tag = `echo $HIST_IM | awk '{printf "%4.4i", $1}'`
>>>COUPLED<<<set HIST_JM_Tag = `echo $HIST_JM | awk '{printf "%4.4i", $1}'`

>>>FVLATLON<<<set ATMOStag = DC${AGCM_IM_Tag}xPC${AGCM_JM_Tag}
>>>FVCUBED<<<set ATMOStag = CF${AGCM_IM_Tag}x6C
>>>DATAOCEAN<<<set OCEANtag = DE${OGCM_IM_Tag}xPE${OGCM_JM_Tag}
>>>COUPLED<<<set OCEANtag = TM${OGCM_IM_Tag}xTM${OGCM_JM_Tag}
>>>COUPLED<<<set HISTtag = DC${HIST_IM_Tag}xPC${HIST_JM_Tag}

#######################################################################
#   Move to Scratch Directory and Copy RC Files from Home Directory
#######################################################################

cd $SCRDIR
/bin/rm -rf *
                             /bin/ln -sf $EXPDIR/RC/* .
                             /bin/cp     $EXPDIR/cap_restart .
                             /bin/cp -f  $HOMDIR/*.rc .
                             /bin/cp -f  $HOMDIR/*.nml .

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
>>>DATAOCEAN<<<setenv BCRSLV    ${ATMOStag}_${OCEANtag}
>>>COUPLED<<<setenv BCRSLV    ${ATMOStag}_DE0360xPE0180
setenv DATELINE  DC
setenv EMISSIONS @EMISSIONS

>>>COUPLED<<<setenv GRIDDIR  @COUPLEDIR/a${AGCM_IM}x${AGCM_JM}_o${OGCM_IM}x${OGCM_JM}
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
>>>COUPLED<<</bin/ln -sf $GRIDDIR/${ATMOStag}_${OCEANtag}-Pfafstetter.til   tile.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/${ATMOStag}_${OCEANtag}-Pfafstetter.TRN   runoff.bin
>>>COUPLED<<</bin/ln -sf $GRIDDIR/tripolar_${OGCM_IM}x${OGCM_JM}.ascii .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
>>>COUPLED<<</bin/ln -s /discover/nobackup/yvikhlia/coupled/Forcings/a${HIST_IM}x${HIST_JM}_o${OGCM_IM}x${OGCM_JM}/${HISTtag}_${OCEANtag}-Pfafstetter.til tile_hist.data

>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.til  tile.data
>>>DATAOCEAN<<<if(     -e  $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL) then
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL  tile.bin
>>>DATAOCEAN<<<endif

/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data
/bin/ln -sf $BCSDIR/Shared/*bin .

>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/visdf_@RES_DATELINE.dat visdf.dat
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/nirdf_@RES_DATELINE.dat nirdf.dat
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/vegdyn_@RES_DATELINE.dat vegdyn.data
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/lai_clim_@RES_DATELINE.data lai.data
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/green_clim_@RES_DATELINE.data green.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/visdf.dat visdf.dat
>>>COUPLED<<</bin/ln -sf $GRIDDIR/nirdf.dat nirdf.dat
>>>VEGMERR<<</bin/ln -sf /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2/OutData/vegdyn_internal_rst vegdyn.data
>>>VEGCPLD<<</bin/ln -sf /discover/nobackup/yvikhlia/coupled/Forcings/a180x1080_o720x410/vegdyn.data vegdyn.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/lai.dat lai.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/green.dat green.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data

>>>FVCUBED<<<if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
>>>FVCUBED<<</bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
>>>FVCUBED<<<endif

>>>COUPLED<<</bin/cp $HOMDIR/input.nml .
>>>COUPLED<<</bin/cp $HOMDIR/*_table .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/cice/kmt_cice.bin .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/cice/grid_cice.bin .

_EOF_

>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@SSTFILE   sst.data' >> $FILE
>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@ICEFILE fraci.data' >> $FILE
>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@KPARFILE SEAWIFS_KPAR_mon_clim.data' >> $FILE

chmod +x linkbcs
/bin/cp  linkbcs $EXPDIR

#######################################################################
#          Get C2L History weights/index file for Cubed-Sphere
#######################################################################

set C_NPX = `echo $AGCM_IM | awk '{printf "%5.5i", $1}'`
set C_NPY = `echo $AGCM_JM | awk '{printf "%5.5i", $1}'`
set H_NPX = `echo @HIST_IM | awk '{printf "%5.5i", $1}'`
set H_NPY = `echo @HIST_JM | awk '{printf "%5.5i", $1}'`

set c2l_file = "${C_NPX}x${C_NPY}_c2l_${H_NPX}x${H_NPY}.bin"

if (-e $BCSDIR/$BCRSLV/${c2l_file}) /bin/ln -s $BCSDIR/$BCRSLV/${c2l_file} .

#######################################################################
#                    Get Executable and RESTARTS 
#######################################################################

/bin/cp $EXPDIR/GEOSgcm.x .

set rst_files      = `cat AGCM.rc | grep "RESTART_FILE"    | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_files      = `cat AGCM.rc | grep "CHECKPOINT_FILE" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_file_names = `cat AGCM.rc | grep "RESTART_FILE"    | cut -d ":" -f2`
set chk_file_names = `cat AGCM.rc | grep "CHECKPOINT_FILE" | cut -d ":" -f2`

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
foreach rst ( $rst_file_names )
  if(-e $EXPDIR/$rst ) /bin/cp $EXPDIR/$rst . &
end
wait

>>>COUPLED<<</bin/cp -R $GRIDDIR/INPUT .
>>>COUPLED<<</bin/cp $EXPDIR/RESTART/* INPUT

# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
>>>FORCAST<<<set capdateIC = `cat $HOMDIR/cap_restartIC | cut -c1-8`
>>>FORCAST<<<set capdateCR = `cat cap_restart | cut -c1-8`
>>>FORCAST<<<set monIC = `cat $HOMDIR/cap_restartIC | cut -c5-6`
>>>FORCAST<<<set monCR = `cat cap_restart | cut -c5-6`

set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_file_names )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            /bin/cp $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
>>>COUPLED<<<   /bin/cp -r $EXPDIR/RESTART ${EXPDIR}/restarts/RESTART.${edate}
   cd $EXPDIR/restarts
      >>>DATAOCEAN<<<tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
      >>>COUPLED<<<tar cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} RESTART.${edate}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
     >>>COUPLED<<</bin/rm -rf RESTART.${edate}
   cd $SCRDIR
endif

##################################################################
######
######         Perform multiple iterations of Model Run
######
##################################################################

@ counter    = 1
while ( $counter <= ${NUM_SGMT} )

/bin/rm -f  EGRESS
/bin/cp -f $HOMDIR/CAP.rc .
./strip            CAP.rc

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

# For MERRA-2 and OSTIA, Modify local CAP.rc Ending date if Finish time exceeds Current year boundary
# ---------------------------------------------------------------------------------------------------
if( ${OCEANtag} == DE1440xPE0720 | \
    ${OCEANtag} == DE2880xPE1440 ) then
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
if( ${QFEDCLIM} == FALSE ) then
if( ${EMISSIONS} == MERRA2 | \
    ${EMISSIONS} == MERRA2-DD ) then
    set MERRA2_Transition_Date = 20000401

    if( $nymdc < ${MERRA2_Transition_Date} ) then
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/19600101-20000331
         if( $nymdf > ${MERRA2_Transition_Date} ) then
          set nymdf = ${MERRA2_Transition_Date}
          set oldstring = `cat CAP.rc | grep END_DATE:`
          set newstring = "END_DATE: $nymdf $nhmsf"
          /bin/mv CAP.rc CAP.tmp
                     cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
         endif
    else
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/20000401-present
    endif

    if( $AGCM_LM == 72 ) then
        /bin/cp --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          /bin/cp $file dummy
              cat       dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > `basename $file`
        end
    endif

endif
endif

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

# Test Saltwater Restart for Number of tiles correctness
# ------------------------------------------------------

if ( -x $GEOSBIN/rs_numtiles.x ) then

   set N_SALT_TILES_EXPECTED = `grep '^ *0' tile.data | wc -l`
   set N_SALT_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x saltwater_internal_rst | grep Total | awk '{print $3}'`
         
   if ( $N_SALT_TILES_EXPECTED != $N_SALT_TILES_FOUND ) then
      echo "Error! Found $N_SALT_TILES_FOUND tiles in saltwater. Expect to find $N_SALT_TILES_EXPECTED tiles."
      echo "Your restarts are probably for a different ocean."
      exit 7
   endif    

endif

# Environment variables for MPI, etc
# ----------------------------------

@SETENVS

@GPUSTART

# Run GEOSgcm.x
# -------------
if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh
       @  NPES = $NX * $NY
$RUN_CMD $NPES ./GEOSgcm.x
if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh

@GPUEND

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc

>>>FORCAST<<<if ($rc != 0) then
>>>FORCAST<<<   touch $HOMDIR/PANICSTOP
>>>FORCAST<<<   exit
>>>FORCAST<<<endif
 
#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

set numrst = `echo $rst_files | wc -w`
set numchk = `echo $chk_files | wc -w`

@ n = 1
@ z = $numrst + 1
while ( $n <= $numchk )
   if ( -e $chk_file_names[$n] ) then
       @ m = 1
       while ( $m <= $numrst )
       if(    $chk_files[$n] == $rst_files[$m] || \
            \#$chk_files[$n] == $rst_files[$m]    ) then

            set   chk_type = `/usr/bin/file -Lb --mime-type $chk_file_names[$n]`
            if ( $chk_type =~ "application/octet-stream" ) then
                  set ext  = bin
            else
                  set ext  = nc4
            endif
           /bin/mv $chk_file_names[$n] $rst_file_names[$m]
           /bin/cp $rst_file_names[$m] ${EXPDIR}/restarts/$EXPID.${rst_file_names[$m]}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.$ext &
           @ m = $numrst + 999
       else
           @ m = $m + 1
       endif
       end
       wait
       if( $m == $z ) then
           echo "Warning!!  Could not find CHECKPOINT/RESTART match for:  " $chk_files[$n]
           exit
       endif
   endif
@ n = $n + 1
end

>>>COUPLED<<</bin/cp -r RESTART ${EXPDIR}/restarts/RESTART.${edate}
>>>COUPLED<<</bin/cp RESTART/* INPUT

# Rename and Move Intermediate Checkpoints
# ----------------------------------------
/bin/mv -f *_checkpoint* ${EXPDIR}/restarts

cd $EXPDIR/restarts
   $GEOSBIN/stripname _checkpoint. _rst.e
    if( $FSEGMENT == 00000000 ) then
	>>>DATAOCEAN<<<tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
        >>>COUPLED<<<tar cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.* RESTART.${edate}
        /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
	>>>COUPLED<<</bin/rm -rf RESTART.${edate}
    endif
cd $SCRDIR

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
>>>COUPLED<<< set dsets="ocean_month"
>>>FORCAST<<< if ($SUBSEASONAL == FALSE ) set dsets="ocean_daily"
>>>COUPLED<<< foreach dset ( $dsets )
>>>COUPLED<<< set num = `/bin/ls -1 $dset.nc | wc -l`
>>>COUPLED<<< if($num != 0) then
>>>COUPLED<<<    if(! -e $EXPDIR/MOM_Output) mkdir -p $EXPDIR/MOM_Output
>>>COUPLED<<<    /bin/mv $SCRDIR/$dset.nc $EXPDIR/MOM_Output/$dset.${edate}.nc
>>>COUPLED<<< endif
>>>COUPLED<<< end
>>>COUPLED<<<
#######################################################################
#              Submit Post-Processing and Forecasts
#######################################################################
cd   $EXPDIR/post
>>>FORCAST<<</bin/cp gcm_post_full.j gcm_post.j

>>>FORCAST<<<if ($SUBSEASONAL == TRUE ) then
foreach collection ( $collections ) # Note: Change $collections to "ALL" to run Single Post Job
/bin/rm -f sedfile
cat >      sedfile << EOF
s/@POST_O/gcm_post.${collection}.${edate}/g
s/@COLLECTION/$collection/g
EOF
>>>FORCAST<<<sed -f sedfile gcm_post.j > gcm_post.jtmp
>>>FORCAST<<<chmod 755  gcm_post.jtmp
>>>FORCAST<<</bin/rm -f gcm_post.jtmp
>>>FORCAST<<</bin/rm -f sedfile
>>>FORCAST<<<sleep 5
end
>>>FORCAST<<<else
>>>FORCAST<<<if ( ${capdateIC} == ${capdateCR} ) /bin/cp gcm_post_part.j gcm_post.j
>>>FORCAST<<<if ( ${monIC} == ${monCR} ) /bin/cp gcm_post_part.j gcm_post.j
>>>FORCAST<<</bin/rm -f sedfile
>>>FORCAST<<<cat >      sedfile << EOF
>>>FORCAST<<<s/@POST_O/gcm_post.${edate}/g
>>>FORCAST<<<EOF
sed -f sedfile gcm_post.j > gcm_post.jtmp
chmod 755  gcm_post.jtmp
set monNEXT = `cat cap_restart | cut -c5-6`
if ( ${monNEXT} != ${monCR} ) qsub        gcm_post.jtmp
/bin/rm -f gcm_post.jtmp
/bin/rm -f sedfile
sleep 5
>>>FORCAST<<<endif

cd   $SCRDIR

if( $FSEGMENT != 00000000 ) then
     set REPLAY_BEG_DATE = `grep BEG_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set REPLAY_END_DATE = `grep END_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set nday  = `echo $FSEGMENT | cut -c7-8`
         @ dt  = 10800 - 86400 * $nday
     set date  = `$GEOSBIN/tick $nymdc $nhmsc $dt`
     set nymdz =  $date[1]
     set nhmsz =  $date[2]

     if( $nymdz >= ${REPLAY_BEG_DATE} & \
         $nymdz <= ${REPLAY_END_DATE} ) then
         $EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $FSEGMENT TRUE
     endif
endif

#######################################################################
#                         Update Iteration Counter
#######################################################################
>>>FORCAST<<<set END_DATE = `grep     END_DATE:  $HOMDIR/CAP.rc | cut -d':' -f2`
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

#foreach rst ( $rst_file_names )
#   /bin/rm -f $EXPDIR/$rst  
#end
#   /bin/rm -f $EXPDIR/cap_restart

if ( -e tmprestart) /bin/rm -rf tmprestart
/bin/mkdir tmprestart
foreach rst ( $rst_file_names )
  /bin/cp $rst tmprestart/$rst &
  #/bin/cp $rst $EXPDIR/$rst & 
end
wait
/bin/cp cap_restart tmprestart
#/bin/cp cap_restart $EXPDIR/cap_restart

>>>COUPLED<<</bin/cp -r RESTART tmprestart
>>>COUPLED<<<#/bin/cp -rf RESTART $EXPDIR

>>>COUPLED<<<set nrstrts = `ls -1 tmprestart/RESTART/ocean_* | wc -l`
>>>COUPLED<<<if ( $nrstrts == 14 ) then
>>>COUPLED<<<   /bin/mv tmprestart/RESTART/ocean_* $EXPDIR/RESTART
   /bin/mv tmprestart/*_rst $EXPDIR
   /bin/mv tmprestart/cap_restart $EXPDIR
>>>COUPLED<<<   /bin/rmdir tmprestart/RESTART
>>>COUPLED<<<   /bin/rm -f tmprestart/veg*
>>>COUPLED<<<else
>>>COUPLED<<<   echo "KEEP PREVIOUS IC, START OVER: ${capdateCR}"
>>>COUPLED<<<   touch $HOMDIR/STARTOVER${capdateCR}
>>>COUPLED<<<endif

if ( $rc == 0 ) then
      cd   $HOMDIR
      echo "DATES (current last): ${capdate} ${enddate}"
      >>>FORCAST<<<if ( $SUBSEASONAL == TRUE ) then
      >>>FORCAST<<<   if ( $capdateIC < $capdate ) qsub $EXPDIR/archive/gcm_archive.j
      >>>FORCAST<<<else
      if ( $capdate < $enddate ) qsub $HOMDIR/gcm_run.j
      >>>FORCAST<<<set momm = `/bin/ls -1 $EXPDIR/MOM_Output/ocean_monthly*.nc | wc -l`
      >>>FORCAST<<<set momd = `/bin/ls -1 $EXPDIR/MOM_Output/ocean_daily*.nc | wc -l`
      >>>FORCAST<<<if($momd > 1) qsub $HOMDIR/averageMOM.j
      >>>FORCAST<<<if($momm > 7 ) echo "LAST MOMS NUMBER $enddate $momm"
      >>>FORCAST<<<endif
endif

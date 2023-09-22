#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Regress Job
#######################################################################

#@BATCH_TIME@RUN_T
#@RUN_P
#@BATCH_JOBNAME@REGRESS_N
#@RUN_Q
#@BATCH_GROUP

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

@SETENVS

@MPT_SHEPHERD

# Establish safe default number of OpenMP threads
# -----------------------------------------------
setenv OMP_NUM_THREADS 1

#######################################################################
#                    Command Line Arguments
#######################################################################

# Set the defaults for our arguments
# ----------------------------------

# We use positive logic here. So if we want to run
# both tests, we set both to TRUE. If we want to
# run only one, we set the other to FALSE

set RUN_STARTSTOP = TRUE
set RUN_LAYOUT    = TRUE

while ($#argv > 0)
  switch ($1)
    case --startstop:
      set RUN_LAYOUT = FALSE
      shift
      breaksw
    case --layout:
      set RUN_STARTSTOP = FALSE
      shift
      breaksw
    case -[Hh]:
    case --[Hh]elp:
      echo "Usage: $0 [--startstop] [--layout]"
      echo ""
      echo "By default, both the start/stop and layout tests are run."
      echo "You can specify one or the other with the following options:"
      echo "  --startstop: Only run the start/stop test"
      echo "  --layout:    Only run the layout test"
      echo "  -h, --help:      Print this help message"
      exit 0
    default:
      echo "Unknown argument: $1"
      echo "Usage: $0 [--startstop] [--layout]"
      echo ""
      echo "By default, both the start/stop and layout tests are run."
      echo "You can specify one or the other with the following options:"
      echo "  --startstop: Only run the start/stop test"
      echo "  --layout:    Only run the layout test"
      echo "  -h, --help:      Print this help message"
      exit 1
  endsw
end

# Now because of g5_modules we have to "clear" argv
# otherwise g5_modules will go into script mode and not
# source the modules

set argv = ()

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE    @SITE
setenv GEOSDIR @GEOSDIR
setenv GEOSBIN @GEOSBIN

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${GEOSDIR}/lib

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv EXPID  @EXPID
setenv EXPDIR @EXPDIR
setenv HOMDIR @HOMDIR
setenv SCRDIR $EXPDIR/scratch

#######################################################################
#                 Create Clean Regress Sub-Directory
#######################################################################

mkdir -p $EXPDIR/regress
cd $EXPDIR/regress
/bin/rm -rf `/bin/ls | grep -v  gcm_regress.j | grep -v slurm`

# Copy RC Files from Home Directory
# ---------------------------------
cd $HOMDIR
    set files = `ls -1 *.rc`
    foreach file ($files)
      set fname = `echo $file | cut -d "." -f1`
      cp $fname.rc $EXPDIR/regress
    end
cd $EXPDIR/regress

cp $EXPDIR/RC/*.rc     $EXPDIR/regress
cp $EXPDIR/RC/*.yaml   $EXPDIR/regress
cp $EXPDIR/GEOSgcm.x   $EXPDIR/regress
cp $EXPDIR/linkbcs     $EXPDIR/regress
cp $HOMDIR/*.yaml      $EXPDIR/regress
@COUPLED cp $HOMDIR/*.nml       $EXPDIR/regress
@MOM6cp $HOMDIR/MOM_input   $EXPDIR/regress
@MOM6cp $HOMDIR/MOM_override $EXPDIR/regress

cat fvcore_layout.rc >> input.nml

# Define Atmospheric Resolution
# -----------------------------
set IM = `grep  AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set JM = `grep  AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set LM = `grep  AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`

# Create Restart List
# -------------------
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

# Copy Restarts to Regress directory
# ----------------------------------
foreach rst ( $rst_file_names )
   cp $EXPDIR/$rst $EXPDIR/regress
end
cp $EXPDIR/cap_restart $EXPDIR/regress

# Get proper ridge scheme GWD internal restart
# --------------------------------------------
/bin/rm gwd_internal_rst
/bin/cp @GWDRSDIR/gwd_internal_c${IM} gwd_internal_rst

@COUPLED /bin/mkdir INPUT
@COUPLED cp $EXPDIR/RESTART/* INPUT

setenv YEAR `cat cap_restart | cut -c1-4`
./linkbcs
if(! -e tile.bin) $GEOSBIN/binarytile.x tile.data tile.bin

#######################################################################
#                Split Saltwater Restart if detected
#######################################################################

if ( (-e $EXPDIR/regress/openwater_internal_rst) && (-e $EXPDIR/regress/seaicethermo_internal_rst)) then
  echo "Saltwater internal state is already split, good to go!"
else
 if ( -e $EXPDIR/regress/saltwater_internal_rst ) then

   # The splitter script requires an OutData directory
   # -------------------------------------------------
   if (! -d OutData ) mkdir -p OutData

   # Run the script
   # --------------
   $RUN_CMD 1 $GEOSBIN/SaltIntSplitter tile.data $EXPDIR/regress/saltwater_internal_rst

   # Move restarts
   # -------------
   /bin/mv OutData/openwater_internal_rst OutData/seaicethermo_internal_rst .

   # Remove OutData
   # --------------
   /bin/rmdir OutData

 endif
endif

#######################################################################
#                 Create Simple History for Efficiency
#######################################################################

set         FILE = HISTORY.rc0
/bin/rm -f $FILE
cat << _EOF_ > $FILE

EXPID:  ${EXPID}
EXPDSC: ${EXPID}_Regression_Test

COLLECTIONS: test_collection
           ::

  test_collection.template:         '%y4%m2%d2_%h2%n2z.nc4' ,
  test_collection.archive:          '%c/Y%y4' ,
  test_collection.format:           'CFIO' ,
  test_collection.deflate:           1 ,
  test_collection.frequency:         060000 ,
@DATAOCEAN  test_collection.fields:           'PHIS', 'AGCM' ,
@DATAOCEAN                                    'SLP' , 'DYN'  ,
@DATAOCEAN                                    'T'   , 'DYN'  ,
@DATAOCEAN                                    'U;V' , 'DYN'  ,
@DATAOCEAN                                    'Q'   , 'MOIST', 'QV',
@MOM5  test_collection.fields:           'UW'   ,'MOM'  , 'US',
@MOM5                                    'VW'   ,'MOM'  , 'VS',
@MOM5                                    'TW'   ,'MOM'  , 'TS',
@MOM5                                    'SW'   ,'MOM'  , 'SS',
@MOM5                                    'SLV'  ,'MOM'  ,
@MOM5                                    'QFLUX','OCEAN' ,
@MOM6  test_collection.fields:           'UW'   ,'MOM6'  , 'US',
@MOM6                                    'VW'   ,'MOM6'  , 'VS',
@MOM6                                    'TW'   ,'MOM6'  , 'TS',
@MOM6                                    'SW'   ,'MOM6'  , 'SS',
@MOM6                                    'SLV'  ,'MOM6'  ,
@MOM6                                    'QFLUX','OCEAN' ,
  ::
_EOF_

##################################################################
######
######               Create Regression Test Script
######
##################################################################


# Create Strip Script
# -------------------
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


# Create Checkpoint File List that is currently EXEMPT from reproducibility test
# -----------------------------------------------------------------------------
set EXEMPT_files = `echo SOLAR_INTERNAL_CHECKPOINT_FILE \
                         SURFACE_IMPORT_CHECKPOINT_FILE `

set EXEMPT_chk = ""
foreach file ( ${EXEMPT_files} )
    set file = `cat AGCM.rc | grep "$file" | cut -d ":" -f2`
    set EXEMPT_chk = `echo ${EXEMPT_chk} $file`
end

# Get Current Date and Time from CAP Restart
# ------------------------------------------
set date = `cat cap_restart`
set nymd0 = $date[1]
set nhms0 = $date[2]

set  EXTDATA2G_TRUE = `grep -i '^\s*USE_EXTDATA2G:\s*\.TRUE\.'    CAP.rc | wc -l`

# Select proper AMIP GOCART Emission RC Files
# -------------------------------------------
setenv EMISSIONS @EMISSIONS
if( @EMISSIONS == AMIP_EMISSIONS ) then
    if( $EXTDATA2G_TRUE == 0 ) then
       set AMIP_Transition_Date = 20000301

       if( $nymd0 < ${AMIP_Transition_Date} ) then
         set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP.20C
       else
         set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
       endif
    else
       set AMIP_EMISSIONS_DIRECTORY = $EXPDIR/RC/AMIP
    endif

    if( $LM == 72 ) then
        cp ${AMIP_EMISSIONS_DIRECTORY}/*.rc .
        cp ${AMIP_EMISSIONS_DIRECTORY}/*.yaml .
    else
        set files = `/bin/ls -1 ${AMIP_EMISSIONS_DIRECTORY}/*.rc ${AMIP_EMISSIONS_DIRECTORY}/*.yaml`
        foreach file ($files)
          /bin/rm -f `basename $file`
          /bin/rm -f dummy
          cp $file dummy
          cat dummy | sed -e "s|/L72/|/L${LM}/|g" | sed -e "s|z72|z${LM}|g" > `basename $file`
        end
    endif
endif

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
   if ( ${EMISSIONS} == OPS_EMISSIONS && ${MODIS_Transition_Date} <= $nymd0 ) then
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

     # Link REPLAY files
     # -----------------
     /bin/ln -sf ${ANA_LOCATION}/aod .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE_TYPE} .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE09_TYPE} .

endif

# Copy the original files

cp CAP.rc      CAP.rc.orig
cp AGCM.rc     AGCM.rc.orig
cp HISTORY.rc0 HISTORY.rc

# Capture the original NX and NY

set NX0 = `grep "^ *NX:" AGCM.rc.orig | cut -d':' -f2`
set NY0 = `grep "^ *NY:" AGCM.rc.orig | cut -d':' -f2`

# Set the test_durations for the various tests in HHMMSS format
#
# We can generically set the lengths of startstop and layout tests
# and use those values to set the test durations for the various steps

set length_of_startstop_test = 240000
set length_of_layout_test    = 060000

# Now for safety, we need to make sure that the test durations
# are divisible by 3 hours due to a limitation in GOCART. We'll use modulo
# for this
@ length_of_startstop_test_mod = $length_of_startstop_test % 030000
@ length_of_layout_test_mod    = $length_of_layout_test    % 030000

# If either of these is not zero, then we exit with an error
if( $length_of_startstop_test_mod != 0 || $length_of_layout_test_mod != 0 ) then
   echo "ERROR: Both the length_of_startstop_test and length_of_layout_test"
   echo "       must be divisible by 3 hours due to a limitation in GOCART."
   exit 1
endif

# Now we can set the test durations for the various steps
set test_duration_step1 = $length_of_startstop_test
set test_duration_step2 = $length_of_layout_test

# The step3 duration is step1 - step2
@ test_duration_step3 = $test_duration_step1 - $test_duration_step2

# The step4 duration is identical to step2
set test_duration_step4 = $test_duration_step2

##################################################################
######
######               Perform Regression Test # 1
######                (1-Day Using NX:NY Layout)
######
##################################################################

# This only needs to be run in the startstop case

if( $RUN_STARTSTOP == TRUE ) then

   ./strip CAP.rc
   set oldstring = `cat CAP.rc | grep JOB_SGMT:`
   set newstring = "JOB_SGMT: 00000000 ${test_duration_step1}"
   /bin/mv CAP.rc CAP.tmp
   cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

   set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
   set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
   @ NPES = $NX * $NY

   echo "=== Running test of duration ${test_duration_step1} with NX = $NX and NY = $NY starting at $nymd0 $nhms0 ==="

   @OCEAN_PRELOAD $RUN_CMD $NPES ./GEOSgcm.x --logging_config 'logging.yaml'

   set date = `cat cap_restart`
   set nymde1 = $date[1]
   set nhmse1 = $date[2]

   foreach chk ( $chk_file_names )
      /bin/mv -v $chk ${chk}.${nymde1}_${nhmse1}.1
   end
   @MOM6/bin/mv -v RESTART/MOM.res.nc MOM.res.nc.1

   # Move history as well
   set hist_file_names = `ls -1 ${EXPID}.test_collection.*.nc4`
   # Need also make another variable storing all the history files
   set complete_startstop_hist_file_names = `ls -1 ${EXPID}.test_collection.*.nc4`

   foreach hist ( $hist_file_names )
      /bin/mv -v $hist ${hist}.${nymde1}_${nhmse1}.1
   end

endif

##################################################################
######
######               Perform Regression Test # 2
######               (6-Hour Using NX:NY Layout)
######
##################################################################

# This case runs for both startstop and layout

/bin/rm              cap_restart
echo $nymd0 $nhms0 > cap_restart

cp CAP.rc.orig  CAP.rc
cp AGCM.rc.orig AGCM.rc
cp HISTORY.rc0  HISTORY.rc

@COUPLED /bin/rm -rf INPUT
@COUPLED /bin/mkdir INPUT
@COUPLED cp $EXPDIR/RESTART/* INPUT

./strip CAP.rc
set oldstring = `cat CAP.rc | grep JOB_SGMT:`
set newstring = "JOB_SGMT: 00000000 ${test_duration_step2}"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

setenv YEAR `cat cap_restart | cut -c1-4`
./linkbcs
set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
@ NPES = $NX * $NY

echo "=== Running test of duration ${test_duration_step2} with NX = $NX and NY = $NY starting at $nymd0 $nhms0 ==="

@OCEAN_PRELOAD $RUN_CMD $NPES ./GEOSgcm.x --logging_config 'logging.yaml'

set date = `cat cap_restart`
set nymde2 = $date[1]
set nhmse2 = $date[2]

# If we are doing startstop we need to copy the checkpoints
# If we run layout only, we need to move the checkpoints
if ($RUN_STARTSTOP == TRUE) then
   set MOVE_OR_COPY = "/bin/cp -v"
else
   set MOVE_OR_COPY = "/bin/mv -v"
endif

foreach chk ( $chk_file_names )
   $MOVE_OR_COPY $chk ${chk}.${nymde2}_${nhmse2}.2
end
@MOM6 $MOVE_OR_COPY RESTART/MOM.res.nc MOM.res.nc.2

# *Copy* history as well
set hist_file_names = `ls -1 ${EXPID}.test_collection.*.nc4`

# Need also make another variable storing all the history files
set complete_layout_hist_file_names = `ls -1 ${EXPID}.test_collection.*.nc4`

# Note: We copy the history here because this lets the file(s)
#       be available for Test #3 so a full compare can be made
#       with Test #1
foreach hist ( $hist_file_names )
   $MOVE_OR_COPY $hist ${hist}.${nymde2}_${nhmse2}.2
end

foreach rst ( $rst_file_names )
  /bin/rm -f $rst
end
set numrst = `echo $rst_files | wc -w`
set numchk = `echo $chk_files | wc -w`

@ n = 1
@ z = $numrst + 1
while ( $n <= $numchk )
       @ m = 1
       while ( $m <= $numrst )
       if(  $chk_files[$n] == $rst_files[$m] || \
          \#$chk_files[$n] == $rst_files[$m] ) then
           /bin/mv $chk_file_names[$n] $rst_file_names[$m]
           @ m = $numrst + 999
       else
           @ m = $m + 1
       endif
       end
       if( $m == $z ) then
           echo "Warning!!  Could not find CHECKPOINT/RESTART match for:  " $chk_files[$n]
           exit
       endif
@ n = $n + 1
end

@COUPLED cp RESTART/* INPUT

##################################################################
######
######               Perform Regression Test # 3
######               (18-Hour Using NX:NY Layout)
######
##################################################################

# This case is only needed for startstop

if ($RUN_STARTSTOP == TRUE) then

   cp HISTORY.rc0 HISTORY.rc

   @MOM6# When you restart in MOM6 mode, you must change input_filename
   @MOM6# in the input.nml file from 'n' to 'r'
   @MOM6 /bin/cp input.nml input.nml.orig
   @MOM6 sed -i -e "s/input_filename = 'n'/input_filename = 'r'/g" input.nml

   ./strip CAP.rc
   set oldstring = `cat CAP.rc | grep JOB_SGMT:`
   set newstring = "JOB_SGMT: 00000000 ${test_duration_step3}"
   /bin/mv CAP.rc CAP.tmp
   cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

   setenv YEAR `cat cap_restart | cut -c1-4`
   ./linkbcs
   set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
   set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
   @ NPES = $NX * $NY

   set date = `cat cap_restart`
   set nymdb = $date[1]
   set nhmsb = $date[2]

   echo "=== Running test of duration ${test_duration_step3} with NX = $NX and NY = $NY starting at $nymdb $nhmsb ==="

   @OCEAN_PRELOAD $RUN_CMD $NPES ./GEOSgcm.x --logging_config 'logging.yaml'

   set date = `cat cap_restart`
   set nymde3 = $date[1]
   set nhmse3 = $date[2]

   foreach chk ( $chk_file_names )
      /bin/mv -v $chk ${chk}.${nymde3}_${nhmse3}.3
   end
   @MOM6/bin/mv -v RESTART/MOM.res.nc MOM.res.nc.3

   # Move history as well
   set hist_file_names = `ls -1 ${EXPID}.test_collection.*.nc4`

   foreach hist ( $hist_file_names )
      /bin/mv -v $hist ${hist}.${nymde3}_${nhmse3}.3
   end

endif

##################################################################
######
######               Perform Regression Test # 4
######               (6-Hour Using 1:6 Layout)
######
##################################################################

# This case runs only for layout

if ( $RUN_LAYOUT == TRUE) then

   set test_NX = 1
   set test_NY = 6

   # Copy Original Restarts to Regress directory
   # -------------------------------------------
   foreach rst ( $rst_file_names )
      /bin/rm -f $rst
      cp $EXPDIR/$rst $EXPDIR/regress
   end

   # Get proper ridge scheme GWD internal restart
   # --------------------------------------------
   /bin/rm gwd_internal_rst
   /bin/cp @GWDRSDIR/gwd_internal_c${IM} gwd_internal_rst

   @COUPLED /bin/rm -rf INPUT
   @COUPLED /bin/mkdir INPUT
   @COUPLED cp $EXPDIR/RESTART/* INPUT

   @COUPLED # restore original input.nml
   @COUPLED /bin/mv input.nml.orig input.nml

   /bin/rm              cap_restart
   echo $nymd0 $nhms0 > cap_restart

   cp CAP.rc.orig  CAP.rc
   cp AGCM.rc.orig AGCM.rc
   cp HISTORY.rc0  HISTORY.rc

   ./strip CAP.rc
   set oldstring = `cat CAP.rc | grep JOB_SGMT:`
   set newstring = "JOB_SGMT: 00000000 ${test_duration_step4}"
   /bin/mv CAP.rc CAP.tmp
   cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

   ./strip AGCM.rc
   set oldstring = `cat AGCM.rc | grep "^ *NX:"`
   set newstring = "NX: ${test_NX}"
   /bin/mv AGCM.rc AGCM.tmp
   cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
   set oldstring = `cat AGCM.rc | grep "^ *NY:"`
   set newstring = "NY: ${test_NY}"
   /bin/mv AGCM.rc AGCM.tmp
   cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
   @COUPLED set oldstring = `cat AGCM.rc | grep "^ *OGCM.NX:"`
   @COUPLED set newstring = "OGCM.NX: ${test_NY}"
   @COUPLED /bin/mv AGCM.rc AGCM.tmp
   @COUPLED cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
   @COUPLED set oldstring = `cat AGCM.rc | grep "^ *OGCM.NY:"`
   @COUPLED set newstring = "OGCM.NY: ${test_NX}"
   @COUPLED /bin/mv AGCM.rc AGCM.tmp
   @COUPLED cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc

   @MOM5sed -r -i -e "/^ *layout/ s#= ([0-9]+),*([0-9]+)#= ${test_NY},${test_NX}#" input.nml
   @MOM6sed -r -i -e "s/#override LAYOUT = 3, 2/#override LAYOUT = ${test_NY}, ${test_NX}/g" MOM_override

   setenv YEAR `cat cap_restart | cut -c1-4`
   ./linkbcs
   set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
   set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
   @ NPES = $NX * $NY

   echo "=== Running test of duration ${test_duration_step4} with NX = $test_NX and NY = $test_NY starting at $nymd0 $nhms0 ==="

   @OCEAN_PRELOAD $RUN_CMD $NPES ./GEOSgcm.x --logging_config 'logging.yaml'

   set date = `cat cap_restart`
   set nymde4 = $date[1]
   set nhmse4 = $date[2]

   foreach chk ( $chk_file_names )
      /bin/mv -v $chk ${chk}.${nymde4}_${nhmse4}.4
   end
   @MOM6/bin/mv -v RESTART/MOM.res.nc MOM.res.nc.4

   # Move history as well
   set hist_file_names = `ls -1 ${EXPID}.test_collection.*.nc4`

   foreach hist ( $hist_file_names )
      /bin/mv -v $hist ${hist}.${nymde4}_${nhmse4}.4
   end

endif

#######################################################################
#                          Compare Restarts
#                      for start stop regression
#######################################################################

# This part compares the restarts from the 24-hour NXxNY run (.1) with the
# restarts at the end of the 6-hour + 18-hour runs (.3)

if ($RUN_STARTSTOP == TRUE) then

   set NCCMP = `echo ${BASEDIR}/${ARCH}/bin/nccmp -dmfgBq `

   if( -e startstop_regress_test ) /bin/rm startstop_regress_test

   echo "=== Comparing restarts from ${NX0}x${NY0} run of duration ${test_duration_step1} with restarts from ${test_duration_step2} + ${test_duration_step3} ${NX0}x${NY0} runs ==="

   set startstop_pass = true
   foreach chk ( $chk_file_names )
   set file1 = ${chk}.${nymde1}_${nhmse1}.1
   set file2 = ${chk}.${nymde3}_${nhmse3}.3
   if( -e $file1 && -e $file2 ) then
         set check = true
         foreach exempt (${EXEMPT_chk})
            if( $chk == $exempt ) set check = false
         end
         if( $check == true ) then
            echo Comparing ${chk}

   # compare binary checkpoint files
            cmp $file1 $file2
            if( $status == 0 ) then
               echo Start-Stop Success!
               echo " "
            else
               echo Start-Stop Failed!
               echo " "
               set startstop_pass = false
            endif

   # compare NetCDF-4 checkpoint files
   # 	 ${NCCMP} $file1 $file2
   # 	 if( status == 0 ) then
   # 	     echo Start-Stop Success!
   # 	     echo " "
   # 	 else
   # 	     echo Start-Stop Failed!
   # 	     echo " "
   # 	     set startstop_pass = false
   # 	 endif

         endif
   endif
   end

   @MOM6# check MOM.res.nc (MOM6 restart)
   @MOM6set file1 = MOM.res.nc.1
   @MOM6set file2 = MOM.res.nc.3
   @MOM6if( -e $file1 && -e $file2 ) then
   @MOM6      set check = true
   @MOM6      if( $check == true ) then
   @MOM6         echo Comparing "MOM6 restarts"
   @MOM6         cmp $file1 $file2
   @MOM6         if( $status == 0 ) then
   @MOM6             echo Start-Stop Success!
   @MOM6             echo " "
   @MOM6         else
   @MOM6             echo Start-Stop Failed!
   @MOM6             echo " "
   @MOM6             set pass = false
   @MOM6         endif
   @MOM6      endif
   @MOM6endif

   echo "=== Comparing history files from ${NX0}x${NY0} run of duration ${test_duration_step1} with restarts from ${test_duration_step2} + ${test_duration_step3} ${NX0}x${NY0} runs ==="

   # Check history files
   foreach hist ( $complete_startstop_hist_file_names )
   set file1 = ${hist}.${nymde1}_${nhmse1}.1
   set file2 = ${hist}.${nymde3}_${nhmse3}.3
   if( -e $file1 && -e $file2 ) then
         set check = true
         if( $check == true ) then
            echo Comparing ${hist}

   # compare history files
            ${NCCMP} $file1 $file2
            if( $status == 0 ) then
               echo Start-Stop Success!
               echo " "
            else
               echo Start-Stop Failed!
               echo " "
               set startstop_pass = false
            endif

         endif
   endif
   end

   if( $startstop_pass == true ) then
      echo "<font color=green> PASS </font>"                > startstop_regress_test
   else
      echo "<font color=red> <blink> FAIL </blink> </font>" > startstop_regress_test
   endif

else

   # We need to set something here for the "overall" regress_test file
   set startstop_pass = true

endif

#######################################################################
#                          Compare Restarts
#                        for layout regression
#######################################################################

# This part compares the restarts from the 6-hour NXxNY run (.2) with the
# restarts from the 6-hour 1x6 run (.4)

if ($RUN_LAYOUT == TRUE) then

   set NCCMP = `echo ${BASEDIR}/${ARCH}/bin/nccmp -dmfgBq `

   if( -e layout_regress_test ) /bin/rm layout_regress_test

   echo "=== Comparing restarts from ${NX0}x${NY0} run of duration ${test_duration_step2} with restarts from ${test_NX}x${test_NY} run of duration ${test_duration_step4} ==="

   set layout_pass = true
   foreach chk ( $chk_file_names )
   set file1 = ${chk}.${nymde2}_${nhmse2}.2
   set file2 = ${chk}.${nymde4}_${nhmse4}.4
   if( -e $file1 && -e $file2 ) then
         set check = true
         foreach exempt (${EXEMPT_chk})
            if( $chk == $exempt ) set check = false
         end
         if( $check == true ) then
            echo Comparing ${chk}

   # compare binary checkpoint files
            cmp $file1 $file2
            if( $status == 0 ) then
               echo Layout Success!
               echo " "
            else
               echo Layout Failed!
               echo " "
               set layout_pass = false
            endif

   # compare NetCDF-4 checkpoint files
   # 	 ${NCCMP} $file1 $file2
   # 	 if( status == 0 ) then
   # 	     echo Layout Success!
   # 	     echo " "
   # 	 else
   # 	     echo Layout Failed!
   # 	     echo " "
   # 	     set layout_pass = false
   # 	 endif

         endif
   endif
   end

   @MOM6# check MOM.res.nc (MOM6 restart)
   @MOM6set file1 = MOM.res.nc.2
   @MOM6set file2 = MOM.res.nc.4
   @MOM6if( -e $file1 && -e $file2 ) then
   @MOM6      set check = true
   @MOM6      if( $check == true ) then
   @MOM6         echo Comparing "MOM6 restarts"
   @MOM6         cmp $file1 $file2
   @MOM6         if( $status == 0 ) then
   @MOM6             echo Layout Success!
   @MOM6             echo " "
   @MOM6         else
   @MOM6             echo Layout Failed!
   @MOM6             echo " "
   @MOM6             set pass = false
   @MOM6         endif
   @MOM6      endif
   @MOM6endif

   echo "=== Comparing history files from 6-hour ${NX0}x${NY0} run with restarts from 6-hour ${test_NX}x${test_NY} run ==="

   # Check history files
   foreach hist ( $complete_layout_hist_file_names )
   set file1 = ${hist}.${nymde2}_${nhmse4}.2
   set file2 = ${hist}.${nymde2}_${nhmse4}.4
   if( -e $file1 && -e $file2 ) then
         set check = true
         if( $check == true ) then
            echo Comparing ${hist}

   # compare history files
            ${NCCMP} $file1 $file2
            if( $status == 0 ) then
               echo Layout Success!
               echo " "
            else
               echo Layout Failed!
               echo " "
               set layout_pass = false
            endif

         endif
   endif
   end

   if( $layout_pass == true ) then
      echo "<font color=green> PASS </font>"                > layout_regress_test
   else
      echo "<font color=red> <blink> FAIL </blink> </font>" > layout_regress_test
   endif

else

   # We need to set something here for the "overal" regress_test file
   set layout_pass = true

endif

if( $startstop_pass == true && $layout_pass == true ) then
   echo "<font color=green> PASS </font>"                > regress_test
else
   echo "<font color=red> <blink> FAIL </blink> </font>" > regress_test
endif

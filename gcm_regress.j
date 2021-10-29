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

# Establish safe default number of OpenMP threads
# -----------------------------------------------
setenv OMP_NUM_THREADS 1

@GPUSTART

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSDIR          @GEOSDIR
setenv GEOSBIN          @GEOSBIN

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib:${GEOSDIR}/lib

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    EXPID   @EXPID
setenv    EXPDIR  @EXPDIR
setenv    HOMDIR  @HOMDIR
setenv    SCRDIR  $EXPDIR/scratch

#######################################################################
#                 Create Clean Regress Sub-Directory
#######################################################################

mkdir -p                    $EXPDIR/regress
cd                          $EXPDIR/regress
/bin/rm -rf `/bin/ls | grep -v  gcm_regress.j | grep -v slurm`

# Copy RC Files from Home Directory
# ---------------------------------
cd $HOMDIR
    set files = `ls -1 *.rc`
    foreach file ($files)
            set fname = `echo $file | cut -d "." -f1`
           @CPEXEC $fname.rc $EXPDIR/regress
    end
cd $EXPDIR/regress

/bin/ln -s $EXPDIR/RC/*.rc  $EXPDIR/regress
@CPEXEC $EXPDIR/GEOSgcm.x   $EXPDIR/regress
@CPEXEC $EXPDIR/linkbcs     $EXPDIR/regress
@CPEXEC $HOMDIR/*.yaml      $EXPDIR/regress
@COUPLED @CPEXEC $HOMDIR/*.nml       $EXPDIR/regress
>>>MOM6<<<@CPEXEC $HOMDIR/MOM_input   $EXPDIR/regress
>>>MOM6<<<@CPEXEC $HOMDIR/MOM_override $EXPDIR/regress

cat fvcore_layout.rc >> input.nml

# Define Atmospheric Resolution
# -----------------------------
set IM = `grep  AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set JM = `grep  AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set LM = `grep  AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`

@    IM6 = 6 * $IM
if( $IM6 == $JM ) then
   set CUBE = TRUE
else
   set CUBE = FALSE
endif

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
       @CPEXEC $EXPDIR/$rst $EXPDIR/regress
end
@CPEXEC $EXPDIR/cap_restart $EXPDIR/regress

@COUPLED /bin/mkdir INPUT
@COUPLED @CPEXEC $EXPDIR/RESTART/* INPUT

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

COLLECTIONS:
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

# Select proper MERRA-2 GOCART Emission RC Files
# ----------------------------------------------
setenv EMISSIONS @EMISSIONS
if( @EMISSIONS =~ MERRA2* ) then
    set MERRA2_Transition_Date = 20000401

    if( $nymd0 < ${MERRA2_Transition_Date} ) then
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/etc/@EMISSIONS/19600101-20000331
    else
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/etc/@EMISSIONS/20000401-present
    endif

    if( $LM == 72 ) then
        @CPEXEC --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          @CPEXEC $file dummy
              cat       dummy | sed -e "s|/L72/|/L${LM}/|g" | sed -e "s|z72|z${LM}|g" > `basename $file`
        end
    endif
endif

@MP_NO_USE_WSUB# 1MOM and GFDL microphysics do not use WSUB_NATURE
@MP_NO_USE_WSUB# -------------------------------------------------
@MP_NO_USE_WSUB/bin/mv WSUB_ExtData.rc WSUB_ExtData.tmp
@MP_NO_USE_WSUBcat WSUB_ExtData.tmp | sed -e '/^WSUB_NATURE/ s#ExtData.*#/dev/null#' > WSUB_ExtData.rc
@MP_NO_USE_WSUB/bin/rm WSUB_ExtData.tmp

if(-e ExtData.rc )    /bin/rm -f   ExtData.rc
set  extdata_files = `/bin/ls -1 *_ExtData.rc`
cat $extdata_files > ExtData.rc 


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

##################################################################
######
######               Perform Regression Test # 1
######                (1-Day Using NX:NY Layout)
######
##################################################################

set test_duration = 240000

@CPEXEC     CAP.rc      CAP.rc.orig
@CPEXEC    AGCM.rc     AGCM.rc.orig
@CPEXEC HISTORY.rc0 HISTORY.rc

set           NX0 = `grep "^ *NX:" AGCM.rc.orig | cut -d':' -f2`
set           NY0 = `grep "^ *NY:" AGCM.rc.orig | cut -d':' -f2`

@ NPES0 = $NX0 * $NY0


./strip CAP.rc
set oldstring =  `cat CAP.rc | grep JOB_SGMT:`
set newstring =  "JOB_SGMT: 00000000 ${test_duration}"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
@ NPES = $NX * $NY
$RUN_CMD $NPES ./GEOSgcm.x
                                                                                                                      

set date = `cat cap_restart`
set nymde = $date[1]
set nhmse = $date[2]

foreach   chk ( $chk_file_names )
 /bin/mv $chk  ${chk}.${nymde}_${nhmse}.1
end
>>>MOM6<<</bin/mv RESTART/MOM.res.nc MOM.res.nc.1

##################################################################
######
######               Perform Regression Test # 2
######
##################################################################

set test_duration = 180000

@DATAOCEAN if( $CUBE == TRUE ) then
@DATAOCEAN     @ test_NX = $NPES0 / 6
@DATAOCEAN     @ test_NP = $IM / $test_NX
@DATAOCEAN   if($test_NP < 4 ) then
@DATAOCEAN     @ test_NX = $IM / 4 # To ensure enough gridpoints for HALO
@DATAOCEAN   endif
@DATAOCEAN   set test_NY = 6
@DATAOCEAN else
@DATAOCEAN   set test_NX = $NY0
@DATAOCEAN   set test_NY = $NX0
@DATAOCEAN endif

@COUPLED set test_NX = $NX0
@COUPLED set test_NY = $NY0

/bin/rm              cap_restart
echo $nymd0 $nhms0 > cap_restart

@CPEXEC     CAP.rc.orig  CAP.rc
@CPEXEC    AGCM.rc.orig AGCM.rc
@CPEXEC HISTORY.rc0  HISTORY.rc

./strip CAP.rc
set oldstring =  `cat CAP.rc | grep JOB_SGMT:`
set newstring =  "JOB_SGMT: 00000000 ${test_duration}"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

./strip AGCM.rc
set oldstring =  `cat AGCM.rc | grep "^ *NX:"`
set newstring =  "NX: ${test_NX}"
/bin/mv AGCM.rc AGCM.tmp
cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
set oldstring =  `cat AGCM.rc | grep "^ *NY:"`
set newstring =  "NY: ${test_NY}"
/bin/mv AGCM.rc AGCM.tmp
cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc

setenv YEAR `cat cap_restart | cut -c1-4`
./linkbcs
set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
@ NPES = $NX * $NY
$RUN_CMD $NPES ./GEOSgcm.x

foreach rst ( $rst_file_names )
  /bin/rm -f  $rst
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

@COUPLED @CPEXEC RESTART/* INPUT

##################################################################
######
######               Perform Regression Test # 3
######
##################################################################

set test_duration = 060000

if( $CUBE == TRUE ) then
  set test_NX = 1
  set test_NY = 6
  set test_Cores = 6
else
  set test_NX = 1
  set test_NY = 2
  set test_Cores = 2
endif

./strip CAP.rc
set oldstring =  `cat CAP.rc | grep JOB_SGMT:`
set newstring =  "JOB_SGMT: 00000000 ${test_duration}"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc

./strip AGCM.rc
set oldstring =  `cat AGCM.rc | grep "^ *NX:"`
set newstring =  "NX: ${test_NX}"
/bin/mv AGCM.rc AGCM.tmp
cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
set oldstring =  `cat AGCM.rc | grep "^ *NY:"`
set newstring =  "NY: ${test_NY}"
/bin/mv AGCM.rc AGCM.tmp
cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
@COUPLED set oldstring =  `cat AGCM.rc | grep "^ *OGCM.NX:"`
@COUPLED set newstring =  "OGCM.NX: ${test_NY}"
@COUPLED /bin/mv AGCM.rc AGCM.tmp
@COUPLED cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc
@COUPLED set oldstring =  `cat AGCM.rc | grep "^ *OGCM.NY:"`
@COUPLED set newstring =  "OGCM.NY: ${test_NX}"
@COUPLED /bin/mv AGCM.rc AGCM.tmp
@COUPLED cat AGCM.tmp | sed -e "s?$oldstring?$newstring?g" > AGCM.rc

>>>MOM5<<<sed -r -i -e "/^ *layout/ s#= ([0-9]+),*([0-9]+)#= ${test_NY},${test_NX}#" input.nml
>>>MOM6<<<sed -r -i -e "/^ *LAYOUT/ s#= ([0-9]+), *([0-9]+)#= ${test_NY}, ${test_NX}#" MOM_input

setenv YEAR `cat cap_restart | cut -c1-4`
./linkbcs
set NX = `grep "^ *NX": AGCM.rc | cut -d':' -f2`
set NY = `grep "^ *NY": AGCM.rc | cut -d':' -f2`
@ NPES = $NX * $NY
$RUN_CMD $NPES ./GEOSgcm.x
                                                                                                                      
set date = `cat cap_restart`
set nymde = $date[1]
set nhmse = $date[2]

foreach   chk ( $chk_file_names )
 /bin/mv $chk  ${chk}.${nymde}_${nhmse}.2
end
>>>MOM6<<</bin/mv RESTART/MOM.res.nc MOM.res.nc.2

#######################################################################
#                          Compare Restarts
#######################################################################

set CDO = `echo ${BASEDIR}/${ARCH}/bin/cdo -Q -s diffn`

if( -e regress_test ) /bin/rm regress_test

set pass = true
foreach chk ( $chk_file_names )
  set file1 = ${chk}.${nymde}_${nhmse}.1
  set file2 = ${chk}.${nymde}_${nhmse}.2
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
             echo Success!
             echo " "
         else
             echo Failed!
             echo " "
             set pass = false
         endif

# compare NetCDF-4 checkpoint files
# 	 set NUMDIFF = `${CDO} $file1 $file2 | awk '{print $1}'`
# 	 if( "$NUMDIFF" == "" ) then
# 	     echo Success!
# 	     echo " "
# 	 else
# 	     echo Failed!
# 	     echo `${CDO} $file1 $file2`
# 	     echo " "
# 	     set pass = false
# 	 endif

      endif
  endif
end

# check MOM.res.nc (MOM6 restart)
>>>MOM6<<<set file1 = MOM.res.nc.1
>>>MOM6<<<set file2 = MOM.res.nc.2
>>>MOM6<<<if( -e $file1 && -e $file2 ) then
>>>MOM6<<<                             set check = true
>>>MOM6<<<      if( $check == true ) then
>>>MOM6<<<         echo Comparing "MOM6 restarts"
>>>MOM6<<<         cmp $file1 $file2
>>>MOM6<<<         if( $status == 0 ) then
>>>MOM6<<<             echo Success!
>>>MOM6<<<             echo " "
>>>MOM6<<<         else
>>>MOM6<<<             echo Failed!
>>>MOM6<<<             echo " "
>>>MOM6<<<             set pass = false
>>>MOM6<<<         endif
>>>MOM6<<<      endif
>>>MOM6<<<endif

@GPUEND

if( $pass == true ) then
     echo "<font color=green> PASS </font>"                > regress_test
else
     echo "<font color=red> <blink> FAIL </blink> </font>" > regress_test
endif

#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#SBATCH --time=5:00:00
#SBATCH --nodes=660 --ntasks-per-node=60
#SBATCH --job-name=Feature-c2160_L137_RUN
#SBATCH --constraint=mil --account=s1062
#SBATCH --reservation=geosCAM --qos=geos_xl --nice
##SBATCH --nice --partition=geosgms
#@BATCH_NAME -o gcm_run.o@RSTDATE

#######################################################################
#                         System Settings
#######################################################################

if ($?SLURM_JOBID) then
  echo $SLURM_JOBID
endif

umask 022

limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             NCCS
setenv GEOSDIR          /discover/nobackup/projects/gmao/osse2/GIT/GEOS-LM_v12-rc8/GEOSgcm/install-Aggressive-SLES15
setenv GEOSBIN          /discover/nobackup/projects/gmao/osse2/GIT/GEOS-LM_v12-rc8/GEOSgcm/install-Aggressive-SLES15/bin
setenv GEOSETC          /discover/nobackup/projects/gmao/osse2/GIT/GEOS-LM_v12-rc8/GEOSgcm/install-Aggressive-SLES15/etc
setenv GEOSUTIL         /discover/nobackup/projects/gmao/osse2/GIT/GEOS-LM_v12-rc8/GEOSgcm/install-Aggressive-SLES15

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${GEOSDIR}/lib
# We only add BASEDIR to the LD_LIBRARY_PATH if BASEDIR is defined (i.e., not running with Spack)
if ( $?BASEDIR ) then
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib
endif

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

setenv GCMVER 'GEOSgcm-SFE2025'
echo   VERSION: $GCMVER

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

set date0 = `cat cap_restart | cut -c1-8`
set time0 = `cat cap_restart | cut -c10-15`
set dateR  = `$GEOSUTIL/post/tick $date0 $time0 10800`
set nymdR = $dateR[1]
set nhmsR = $dateR[2]
echo $nymdR $nhmsR
set YEAR = `echo ${date0} | cut -c1-4`
set hr2 = `echo ${nhmsR} | cut -c1-2`


setenv  EXPID   Feature-c2160_L137
setenv  EXPDIR  /discover/nobackup/projects/gmao/osse2/HWT/CONUS02KM/Feature-c2160_L137
setenv  HOMDIR  /discover/nobackup/projects/gmao/osse2/HWT/CONUS02KM/Feature-c2160_L137
setenv  SSDDIR  /discover/nobackup/projects/gmao/osse2/TSE_staging/HWT/Feature-c2160_L137

setenv  RSTDATE @RSTDATE
setenv  GCMEMIP @GCMEMIP

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################

if (! -e $SSDDIR/restarts   ) mkdir -p $SSDDIR/restarts
if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot

if( $GCMEMIP == TRUE ) then
    if (! -e $EXPDIR/restarts/$RSTDATE ) mkdir -p $EXPDIR/restarts/$RSTDATE
    setenv  SCRDIR  $EXPDIR/scratch.$RSTDATE
else
    setenv  SCRDIR  $SSDDIR/scratch.replay
    if (-e $EXPDIR/scratch) /bin/rm -rf $EXPDIR/scratch
    /bin/ln -s $SCRDIR $EXPDIR/scratch
endif
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

set USE_IOSERVER      = 1
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
@ NCPUS_PER_NODE = $SLURM_NTASKS_PER_NODE

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
#                       GCMEMIP Setup
#######################################################################

if( $GCMEMIP == TRUE & ! -e $EXPDIR/restarts/$RSTDATE/cap_restart ) then

cd $EXPDIR/restarts/$RSTDATE

cp $HOMDIR/CAP.rc CAP.rc.orig
awk '{$1=$1};1' < CAP.rc.orig > CAP.rc

set year  = `echo $RSTDATE | cut -d_ -f1 | cut -b1-4`
set month = `echo $RSTDATE | cut -d_ -f1 | cut -b5-6`

# Copy Restarts from v11.5.2 REPLAY to MERRA-2
# ----------------------------------------------
cp /discover/nobackup/projects/gmao/geos_itv/sdrabenh/REMIP_Experiments/v11.5.2_L072_C180_M2_REMIP/restarts/restarts.e${year}${month}10_21z.tar .
tar -xvf restarts.e${year}${month}10_21z.tar --wildcards "*_internal_rst*"
#/bin/rm restarts.e${year}${month}10_21z.tar

# Regrid v11.5.2 Restarts
# ------------------------------------------------
set RSTID = `/bin/ls *catch* | /bin/grep -Po '^.*(?=\.\w+_rst\.)'`
set day   = `/bin/ls *catch* | /bin/grep -Po '(?<=\d{6})\d{2}(?=_21z)'`
$GEOSBIN/remap_restarts.py command_line -np -ymdh ${year}${month}${day}21 -grout C${AGCM_IM} -levsout ${AGCM_LM} -out_dir . -rst_dir . -expid $RSTID -bcvin NL3 -oceanin 1440x720 -in_bc_base /discover/nobackup/projects/gmao/bcs_shared/fvInput/ExtData/esm/tiles -newid regrid -nonhydrostatic -nobkg -nolcv -bcvout NL3 -rs 3 -oceanout CS -out_bc_base /discover/nobackup/projects/gmao/bcs_shared/fvInput/ExtData/esm/tiles

     set IMC = $AGCM_IM
if(     $IMC < 10 ) then
     set IMC = 000$IMC
else if($IMC < 100) then
     set IMC = 00$IMC
else if($IMC < 1000) then
     set IMC = 0$IMC
endif

set  chk_type = `ls -1 regrid.catch*_internal_rst.${year}${month}${day}_21z.nc4 | xargs /usr/bin/file -Lb --mime-type `
if( "$chk_type" =~ "application/octet-stream" ) set ext = bin
if( "$chk_type" =~ "application/x-hdf"        ) set ext = nc4

$GEOSBIN/stripname regrid.
$GEOSBIN/stripname .${year}${month}${day}_21z.$ext

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
cp     $EXPDIR/linkbcs .
cp -f  $HOMDIR/*.rc .
cp -f  $HOMDIR/*.nml .
cp -f  $HOMDIR/*.yaml .
cp     $GEOSBIN/bundleParser.py .

#cp -f  $HOMDIR/RC.ww3/mod_def.* .
#cp -f  $HOMDIR/RC.ww3/ww3*.nml .

cat fvcore_layout.rc >> input.nml
if (-z input.nml) then
   echo "try cat for input.nml again"
   cat fvcore_layout.rc >> input.nml
endif
if (-z input.nml) then
   echo "input.nml is zero-length"
   exit 0
endif

# Find regrid weights
# -------------------
set IM5 = `echo $AGCM_IM | awk '{printf "%5.5i", $1}'`
set JM5 = `echo $AGCM_JM | awk '{printf "%5.5i", $1}'`
set NX5 = `echo $NX      | awk '{printf "%5.5i", $1}'`
set NY5 = `echo $NY      | awk '{printf "%5.5i", $1}'`
set RH_WEIGHTS_DIR = "$SSDDIR/RH_WEIGHTS_${IM5}x${JM5}_${NX5}x${NY5}"
if ( -e $RH_WEIGHTS_DIR ) /bin/ln -s $RH_WEIGHTS_DIR/* .

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

/bin/cp /discover/nobackup/projects/gmao/osse2/stage/BCS_FILES/lambert_grid.nc4 .

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
setenv BCSDIR    /discover/nobackup/projects/gmao/bcs_shared/fvInput/ExtData/esm/tiles/NL3
setenv BCRSLV    CF2160x6C-SG001_CF2160x6C

#this is hard-wired for NAS for now - should make it more general
setenv BCTAG `basename $BCSDIR`
setenv EMISSIONS OPS_EMISSIONS
chmod +x linkbcs

#######################################################################
#                  Setup executable
#######################################################################

set date0 = `cat cap_restart | cut -c1-8`
set time0 = `cat cap_restart | cut -c10-15`
set dateR  = `$GEOSUTIL/post/tick $date0 $time0 10800`
set nymdR = $dateR[1]
set nhmsR = $dateR[2]
echo $nymdR $nhmsR
/bin/rm ${HOMDIR}/gcm_replay.o${nymdR}_${nhmsR}z
if (-e ${HOMDIR}/slurm-${SLURM_JOB_ID}.out) then
  /bin/ln -s ${HOMDIR}/slurm-${SLURM_JOB_ID}.out ${HOMDIR}/gcm_replay.o${nymdR}_${nhmsR}z
endif
if (-e ${HOMDIR}/gcm_run.j.o${SLURM_JOB_ID}) then
  /bin/ln -s ${HOMDIR}/gcm_run.j.o${SLURM_JOB_ID} ${HOMDIR}/gcm_replay.o${nymdR}_${nhmsR}z
endif

if( $FSEGMENT != 00000000 ) then
  cd ${HOMDIR}
  ./run_fcst.j
  cd $SCRDIR
endif

set ANA_EXPID = `grep '^\s*REPLAY_ANA_EXPID:'    AGCM.rc | cut -d: -f2`
if ($ANA_EXPID =~ "ec_prs2eta_L*") then
# ECMWF
set yr4 = `echo ${nymdR} | cut -c1-4`
set mo2 = `echo ${nymdR} | cut -c5-6`
set hr4 = `echo ${nhmsR} | cut -c1-4`
set hr2 = `echo ${nhmsR} | cut -c1-2`
if (-e ${HOMDIR}/ec_prs2eta_L${AGCM_LM}.${nymdR}_${hr2}z.nc4) /bin/ln -s ${HOMDIR}/ec_prs2eta_L${AGCM_LM}.${nymdR}_${hr2}z.nc4 .
set ANA_ETA  = "ec_prs2eta_L${AGCM_LM}.${nymdR}_${hr2}z.nc4"
set ANA_TMPL = "ec_prs2eta_L${AGCM_LM}.%y4%m2%d2_%h2z.nc4"
set num_ana   = `ls -l $ANA_ETA | grep -c nc4`
if ($num_ana <= 0) then
  echo NO ECMWF ANA FILE
  exit 0
endif
sed -i -e "s|@REPLAY_ANA_LOCATION|${ANA_LOC}|g" AGCM.rc
sed -i -e "s|@REPLAY_ANA_TEMPLATE|${ANA_TMPL}|g" AGCM.rc
else
#GEOS-FP
set yr4 = `echo ${nymdR} | cut -c1-4`
set mo2 = `echo ${nymdR} | cut -c5-6`
set hr4 = `echo ${nhmsR} | cut -c1-4`
set ANA_LOC   = "/discover/nobackup/projects/gmao/osse2/$ANA_EXPID"
set ANA_TMPL  = "ana/Y%y4/M%m2/$ANA_EXPID.ana.eta.%y4%m2%d2_%h2%n2z.nc4"
set ANA_ETA   = "$ANA_LOC/ana/Y${yr4}/M${mo2}/$ANA_EXPID.ana.eta.${nymdR}_${hr4}z.nc4"
set num_ana   = `ls -l $ANA_ETA | grep -c nc4`
if ($num_ana <= 0) then
set fprundir   = `cat /home/dao_ops/$ANA_EXPID/run/.../.FVWORK`
if (-e $fprundir) then
  set ANA_LOC   = "$fprundir"
  set ANA_TMPL  = "$ANA_EXPID.ana.eta.%y4%m2%d2_%h2%n2z.nc4"
  set ANA_ETA   = "$ANA_LOC/$ANA_EXPID.ana.eta.${nymdR}_${hr4}z.nc4"
  set num_ana   = `ls -l $ANA_ETA | grep -c nc4`
else
  set num_ana   = 0
endif
if ($num_ana <= 0) then
  set ANA_LOC    = "/home/dao_ops/$ANA_EXPID/run/.../"
  set ANA_TMPL   = "ana/Y%y4/M%m2/$ANA_EXPID.ana.eta.%y4%m2%d2_%h2%n2z.nc4"
  set ANA_ETA    = "$ANA_LOC/ana/Y${yr4}/M${mo2}/$ANA_EXPID.ana.eta.${nymdR}_${hr4}z.nc4"
  set num_ana    = `ls -l $ANA_ETA | grep -c nc4`
endif
if ($num_ana <= 0) then
  set ANA_LOC  = "/discover/nobackup/projects/gmao/geos_fp_arch/$ANA_EXPID"
  set ANA_TMPL = "ana/Y%y4/M%m2/$ANA_EXPID.ana.eta.%y4%m2%d2_%h2%n2z.nc4"
  set ANA_ETA  = "$ANA_LOC/ana/Y${yr4}/M${mo2}/$ANA_EXPID.ana.eta.${nymdR}_${hr4}z.nc4"
  set num_ana  = `ls -l $ANA_ETA | grep -c nc4`
endif
endif
echo $ANA_ETA
if ($num_ana <= 0) then
  echo NO $ANA_EXPID $ANA_ETA FILE
  exit 0
endif
sed -i -e "s|@REPLAY_ANA_LOCATION|${ANA_LOC}|g" AGCM.rc
sed -i -e "s|@REPLAY_ANA_TEMPLATE|${ANA_TMPL}|g" AGCM.rc
set dateN = `$GEOSBIN/tick $nymdR $nhmsR 21600`
set nymdz =  $dateN[1]
set nhmsz =  $dateN[2]
set n_yr4 = `echo $nymdz | cut -c1-4`
set n_mo2 = `echo $nymdz | cut -c5-6`
set n_hr4 = `echo $nhmsz | cut -c1-4`
set ANA_EXPID  = `grep '^\s*REPLAY_ANA_EXPID:' AGCM.rc | cut -d: -f2`
set ana_file = "$ANA_LOC/ana/Y${n_yr4}/M${n_mo2}/$ANA_EXPID.ana.eta.${nymdz}_${n_hr4}z.nc4"
set num_ana   = `ls -l $ana_file | grep -c nc4`
echo $ana_file
if ($num_ana <= 0) then
  echo NO $ANA_EXPID $ana_file FILE
  exit 0
endif
endif


 echo "Linking $HOMDIR/GEOSgcm.x to $SCRDIR"
 echo ""
 /bin/cp $HOMDIR/GEOSgcm.x $SCRDIR/GEOSgcm.x
 setenv GEOSEXE $SCRDIR/GEOSgcm.x


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

# check if it restarts by face
# ----------------------------------
set rst_by_face = NO
if( $GCMEMIP == TRUE ) then
   if(-e $EXPDIR/restarts/$RSTDATE/fvcore_internal_rst & -e $EXPDIR/restarts/$RSTDATE/fvcore_internal_face_1_rst) then
     echo "grid-based internal_rst and internal_face_x_rst should not co-exist"
     echo "please remove all *internal_rst except these tile-based restarts :"
     foreach rst ( $tile_rsts )
        echo ${rst}_internal_rst
     end
     exit
   endif
   if(-e $EXPDIR/restarts/$RSTDATE/fvcore_internal_face_1_rst) then
     set rst_by_face = YES
   endif
else
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

# WGCM runtime parameters
# -----------------------
set USE_WAVES = `grep '^\s*USE_WAVES:' AGCM.rc| cut -d: -f2`
set wavemodel = `cat WGCM.rc | grep "wave_model:" | cut -d "#" -f1 | cut -d ":" -f 2 | sed 's/\s//g'`
set wavewatch = 0
if (($USE_WAVES != 0) && ($wavemodel == "WW3") ) set wavewatch = 1

# Copy Restarts to Scratch Directory
# ----------------------------------
if( $GCMEMIP == TRUE ) then
    foreach rst ( $rst_file_names $monthly_chk_names )
      if(-e $EXPDIR/restarts/$RSTDATE/$rst ) cp $EXPDIR/restarts/$RSTDATE/$rst . &
    end
else
    set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
    foreach rst ( $rst_file_names $monthly_chk_names )
       if (-e $SSDDIR/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.nc4) then
         /bin/ln -s $SSDDIR/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.nc4 $rst
       else
         /bin/ln -s $HOMDIR/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.nc4 $rst
       endif 
    end

    # WW3 restart file
    if( $wavewatch ) then
        set rst_ww3 = "restart.ww3"
        if(-e $EXPDIR/${rst_ww3} ) /bin/cp $EXPDIR/${rst_ww3} . &
    endif
endif
wait

# Get proper ridge scheme GWD internal restart
# --------------------------------------------
if ( $rst_by_face == YES ) then
  echo "WARNING: The generated gwd_internal_face_x_rst are used"
  #foreach n (1 2 3 4 5 6)
    #/bin/rm gwd_internal_face_${n}_rst
    #/bin/cp /discover/nobackup/projects/gmao/osse2/stage/BCS_FILES/GWD_RIDGE/gwd_internal_c${AGCM_IM}_face_${n} gwd_internal_face_${n}_rst
  #end
else
  # We want to look for the existence of a gwd_internal_rst file
  # as not all resolutions have them (yet). If it exists, we copy
  # it to the scratch directory. If it doesn't exist, we need to
  # add "NCAR_NRDG: 0" to the AGCM.rc file to prevent the model from
  # trying to use it.

  if (-e /discover/nobackup/projects/gmao/osse2/stage/BCS_FILES/GWD_RIDGE/gwd_internal_c${AGCM_IM}) then
    echo "Found gwd_internal_c${AGCM_IM}. Copying to scratch directory"
    /bin/rm gwd_internal_rst
    /bin/cp /discover/nobackup/projects/gmao/osse2/stage/BCS_FILES/GWD_RIDGE/gwd_internal_c${AGCM_IM} gwd_internal_rst
  else
    echo "WARNING: gwd_internal_rst not found. Setting NCAR_NRDG to 0"
    # Now, if the user has already set an NCAR_NRDG value, we need to
    # change it to 0. If they haven't set it, we need to add it to the
    # AGCM.rc file.
    if ( `grep -c "NCAR_NRDG:" AGCM.rc` == 0 ) then
      echo "NCAR_NRDG: 0" >> AGCM.rc
    else
      sed -i '/NCAR_NRDG:/c\NCAR_NRDG: 0' AGCM.rc
    endif
  endif
endif


# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
if( $GCMEMIP == TRUE ) then
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_file_names )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            cp $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
   # WW3 restart file
   if( $wavewatch ) then
       set rst_ww3 = "restart.ww3"
       if( -e ${rst_ww3} ) cp ${rst_ww3}  ${EXPDIR}/restarts/$EXPID.${rst_ww3}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
   endif
   cd $EXPDIR/restarts
       tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
   cd $SCRDIR
endif
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
if( CF2160x6C != DE0360xPE0180 ) then
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

# Set WW3 start date and time
# ---------------------------
if( $wavewatch ) then
    cp ww3_multi.nml ww3_multi.nml.orig
    awk '{$1=$1};1' < ww3_multi.nml.orig > ww3_multi.nml

    # set start date
    set oldstring =  `grep '^\s*DOMAIN%START' ww3_multi.nml`
    set newstring =  "DOMAIN%START = '${nymdc} ${nhmsc}'"

    /bin/mv ww3_multi.nml ww3_multi.nml.tmp
    cat ww3_multi.nml.tmp | sed -e "s?$oldstring?$newstring?g" > ww3_multi.nml
    /bin/rm ww3_multi.nml.tmp

    # set end date
    set oldstring =  `grep '^\s*DOMAIN%STOP' ww3_multi.nml`
    set newstring =  "DOMAIN%STOP = '${nymde} ${nhmse}'"

    /bin/mv ww3_multi.nml ww3_multi.nml.tmp
    cat ww3_multi.nml.tmp | sed -e "s?$oldstring?$newstring?g" > ww3_multi.nml
    /bin/rm ww3_multi.nml.tmp
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

# 1MOM and GFDL microphysics do not use WSUB_CLIM
# -------------------------------------------------
if ($EXTDATA2G_TRUE == 0 ) then
   /bin/mv WSUB_ExtData.rc WSUB_ExtData.tmp
   cat WSUB_ExtData.tmp | sed -e '/^WSUB_CLIM/ s#ExtData.*#/dev/null#' > WSUB_ExtData.rc
else
   /bin/mv WSUB_ExtData.yaml WSUB_ExtData.tmp
   cat WSUB_ExtData.tmp | sed -e '/collection:/ s#WSUB_SWclim.*#/dev/null#' > WSUB_ExtData.yaml
endif
/bin/rm WSUB_ExtData.tmp

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
#   cat $instance.tmp | sed -e '/\bRRTMG\b/ s#RRTMG#RRTMGP#' > $instance
#   /bin/rm $instance.tmp
#end

# Link Boundary Conditions for Appropriate Date
# ---------------------------------------------
setenv YEAR $yearc
./linkbcs

if (! -e tile.bin) then
$GEOSBIN/binarytile.x tile.data tile.bin
endif

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
if( $GCMEMIP == TRUE ) then

if ( -x $GEOSBIN/rs_numtiles.x ) then

   set N_OPENW_TILES_EXPECTED = `grep '^\s*0' tile.data | wc -l`
    set N_OPENW_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x openwater_internal_rst | grep Total | awk '{print $NF}'`

   if ( $N_OPENW_TILES_EXPECTED != $N_OPENW_TILES_FOUND ) then
      echo "Error! Found $N_OPENW_TILES_FOUND tiles in openwater. Expect to find $N_OPENW_TILES_EXPECTED tiles."
      echo "Your restarts are probably for a different ocean."
      exit 7
   endif

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

# This flag prints out the Intel MPI state. Uncomment if needed
#setenv I_MPI_DEBUG 9
setenv I_MPI_FABRICS shm:ofi
setenv I_MPI_OFI_PROVIDER psm3 
setenv I_MPI_ADJUST_ALLREDUCE 11
setenv I_MPI_ADJUST_REDUCE 9
setenv I_MPI_ADJUST_BARRIER 8

# Establish safe default number of OpenMP threads
# -------------------
setenv OMP_NUM_THREADS 2
if ($OMP_NUM_THREADS > 1) then
  setenv OMP_STACKSIZE 16M
  setenv KMP_AFFINITY compact
  echo OMP_STACKSIZE    $OMP_STACKSIZE
  echo KMP_AFFINITY     $KMP_AFFINITY
endif

~/bin/strip GWD_GridComp.rc
if (${OMP_NUM_THREADS} > 1) then
  sed -i -e "s|FALSE|TRUE|g" GWD_GridComp.rc
endif

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

     /bin/ln -sf ${ANA_LOCATION}/aod chem
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE_TYPE} .
     /bin/ln -sf ${ANA_LOCATION}/${REPLAY_FILE09_TYPE} .

endif

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

  $RUN_CMD $TOTAL_PES $GEOSEXE $IOSERVER_OPTIONS $IOSERVER_EXTRA --logging_config 'logging.yaml'

if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh >& /dev/null


if( -e cap_restart ) then
  set dateF = `cat cap_restart | cut -c1-8`
  set timeF = `cat cap_restart | cut -c10-15`
  echo "cap_restart: $dateF $timeF"
else
  echo "cap_restart: missing"
  exit 0
endif

if( -e EGRESS ) then
   set rc = 0
   echo GEOSgcm Run Status: $rc
else
   set rc = -1
   echo GEOSgcm Run Status: $rc
   cd $HOMDIR
   sbatch gcm_run.j
   exit 0
endif

#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`awk '{print $1}' cap_restart`_`awk '{print $2}' cap_restart | cut -c1-2`z


# Move Intermediate Checkpoints to RESTARTS directory
# ---------------------------------------------------
set   checkpoints  =    `/bin/ls -1 *_checkpoint.*`
if( $#checkpoints != 0 ) /bin/mv -f *_checkpoint.* ${SSDDIR}/restarts


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

# Move Renamed Final Checkpoints to RESTARTS directory
# ----------------------------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
/bin/mv $restart ${SSDDIR}/restarts &
end
wait

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

# WW3 restarts - assumes that there is at least one NEW restart file
# ------------------------------------------------------------------
if( $wavewatch ) then

    set ww3checkpoint  = `/bin/ls -1 restart[0-9][0-9][0-9].ww3 | sort -n | tail -n 1`
    set rst_ww3 = "restart.ww3"
    if ( $#ww3checkpoint != 0 ) /bin/mv -f $ww3checkpoint $rst_ww3
    cp $rst_ww3 ${EXPDIR}/restarts/$EXPID.${rst_ww3}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.bin

    # remove intermediate restarts
    set ww3checkpoint  = `/bin/ls -1 restart[0-9][0-9][0-9].ww3 | sort -n | tail -n 1`
    if ( $#ww3checkpoint != 0 ) /bin/rm  ./restart[0-9][0-9][0-9].ww3
endif

# TAR ARCHIVED RESTARTS
# ---------------------
if( $FSEGMENT == 00000000 ) then
     cd $EXPDIR/restarts
         tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
endif


#######################################################################
#               Move HISTORY Files to Holding Directory
#######################################################################

# Move current files to /holding
# ------------------------------
cd $SCRDIR
@ start_time = `date +%s`
foreach collection ( $collections )
   /bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection &
end
wait
@ end_time = `date +%s`
@ diff = $end_time - $start_time
echo "It took $diff seconds to mov holding data"

#######################################################################
#                 Run Post-Processing and Forecasts
#######################################################################

$GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles

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
     /bin/cp cap_restart $EXPDIR/cap_restart
     if( $wavewatch ) then
        set rst_ww3 = "restart.ww3"
        /bin/rm -f $EXPDIR/$rst_ww3
        cp $rst_ww3 $EXPDIR/$rst_ww3
     endif
endif

if( $FSEGMENT != 00000000 ) then
     cd $HOMDIR
     ./run_fcst.j
endif

if ( $rc == 0 ) then
      cd  $HOMDIR
      if ( $GCMEMIP == TRUE ) then
          if( $capdate < $enddate ) sbatch $HOMDIR/gcm_run.j$RSTDATE
      else
          if( $capdate < $enddate ) sbatch $HOMDIR/gcm_run.j
      endif
      sbatch $HOMDIR/restarts_ssd_to_nbk.j
endif

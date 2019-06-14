#!/bin/csh
#
######################################################
#   prepare the set of IC from F25 (MOM), MERRA2 (ATM and VEGDYN), H30 (saltwater and seaice)
#   MERRA-2 restarts:  catch lake landice - convert, fvcore irrad moist(2) pchem solar surf turb(2) - as is
#   from /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan10/rs/Y2011/M10/d5124_m2_jan10.*20111003_03z.bin
#   to   /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2/
#   NOTE: ONLY use jmarshak account
######################################################

set IYR = 1999 #2015
set MONS = "4"  #"1 2 3 4 5 6 7 8 9 10 11 12"

foreach IMO ( $MONS )

#set IMO = 5

if ( $IMO == 1 )  set DAYS="01 06 11 16 21 26 31"
if ( $IMO == 2 )  set DAYS="05 10 15 20 25"
if ( $IMO == 3 )  set DAYS="02 07 12 17 22 27"
if ( $IMO == 4 )  set DAYS="01 06 11 16 21 26"
if ( $IMO == 5 )  set DAYS="01 06 11 16 21 26 31"
if ( $IMO == 6 )  set DAYS="05 10 15 20 25 30"
if ( $IMO == 7 )  set DAYS="05 10 15 20 25 30"
if ( $IMO == 8 )  set DAYS="04 09 14 19 24 29"
if ( $IMO == 9 )  set DAYS="03 08 13 18 23 28"
if ( $IMO == 10 ) set DAYS="03 08 13 18 23 28"
if ( $IMO == 11 ) set DAYS="02 07 12 17 22 27"
if ( $IMO == 12 ) set DAYS="02 07 12 17 22 27"

@ n = 0
foreach DAY1 ( $DAYS )

#set icdate = $1

if ($IMO < 10 ) set icdate = ${IYR}0${IMO}${DAY1}
if ($IMO >  9 ) set icdate = ${IYR}${IMO}${DAY1}

set icYYYY = `echo $icdate | cut -c1-4`
set icMO   = `echo $icdate | cut -c5-6`
set icDD   = `echo $icdate | cut -c7-8`

echo "PROCESSING $icYYYY $icMO : $icdate -----------------------------------------------------------------"

if ( $icMO == '01' ) set icMMM = 'jan'${icDD}
if ( $icMO == '02' ) set icMMM = 'feb'${icDD}
if ( $icMO == '03' ) set icMMM = 'mar'${icDD}
if ( $icMO == '04' ) set icMMM = 'apr'${icDD}
if ( $icMO == '05' ) set icMMM = 'may'${icDD}
if ( $icMO == '06' ) set icMMM = 'jun'${icDD}
if ( $icMO == '07' ) set icMMM = 'jul'${icDD}
if ( $icMO == '08' ) set icMMM = 'aug'${icDD}
if ( $icMO == '09' ) set icMMM = 'sep'${icDD}
if ( $icMO == '10' ) set icMMM = 'oct'${icDD}
if ( $icMO == '11' ) set icMMM = 'nov'${icDD}
if ( $icMO == '12' ) set icMMM = 'dec'${icDD}

@ icdatem1 = $icdate - 1
@ iym1 = ${icYYYY} - 1
if ( ${icMMM} == 'jan01' ) set icdatem1 = ${iym1}1231
if ( ${icMMM} == 'apr01') set icdatem1 = ${icYYYY}0331
if ( ${icMMM} == 'may01') set icdatem1 = ${icYYYY}0430
     
set icdatem1tag = rp_${icdatem1}21z
set ganymedtag = ".Ganymed-4_0_BETA10.Ganymed-4_0_MERRA-2_CF0180x6C_DE1440xPE0720.bin"
set RSTMERREPL = "/gpfsm/dnb02/projects/p58/subx/agcm_replay/rs_m200z/"

#set RSTMERDIRS = "/discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2" # Lena's
set RSTMERDIRS = "/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/restart/MERRA2" # Anna's location

set RSTMERFIL1 = "catch_internal_rst.e${icdate}_00z"
set RSTMERFIL2 = "lake_internal_rst.e${icdate}_00z"
set RSTMERFIL3 = "landice_internal_rst.e${icdate}_00z"

#set MK_RSTDIR = "/discover/nobackup/projects/gmao/t2ssp/build/hera/GEOSodas/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSsurface_GridComp/GEOSland_GridComp/GEOScatch_GridComp/mk_restarts"
set MK_RSTDIR = "/home/aborovik/esma/yuri-S2S-2_1_UNSTABLE/GEOSodas/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSsurface_GridComp/GEOSland_GridComp/GEOScatch_GridComp/mk_restarts"

######################################################
# UNTAR REPLAY DATA IN agcm_replay LOCATION
######################################################
cd ${RSTMERDIRS}/InData
if ( -e tmpagcm_replay${icdate}) /bin/rm -rf tmpagcm_replay${icdate}
mkdir -p tmpagcm_replay${icdate}
cd tmpagcm_replay${icdate}
tar xvf ${RSTMERREPL}/restarts.e${icdate}_00z.tar
/bin/mv ${icdatem1tag}.${RSTMERFIL1}${ganymedtag} ${RSTMERDIRS}/InData/${RSTMERFIL1}.bin
/bin/mv ${icdatem1tag}.${RSTMERFIL2}${ganymedtag} ${RSTMERDIRS}/InData/${RSTMERFIL2}.bin
/bin/mv ${icdatem1tag}.${RSTMERFIL3}${ganymedtag} ${RSTMERDIRS}/InData/${RSTMERFIL3}.bin

######################################################
# CHECK TILES 
######################################################
cmp /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5gcm/bcs/Ganymed-4_0/Ganymed-4_0_MERRA-2/CF0180x6C_DE1440xPE0720/CF0180x6C_DE1440xPE0720-Pfafstetter.til $RSTMERDIRS/InData/CF0180x6C_DE1440xPE0720-Pfafstetter.til
set tileCOMP1 = $status
if ($tileCOMP1 > 0 ) exit
cmp /discover/nobackup/yvikhlia/coupled/Forcings/a180x1080_o720x410/CF0180x6C_TM0720xTM0410-Pfafstetter.til $RSTMERDIRS/OutData/CF0180x6C_TM0720xTM0410-Pfafstetter.til
set tileCOMP2 = $status
if ($tileCOMP2 > 0 ) exit

#######################################################
#              NOW CONVERT
#######################################################
echo NOW CONVERT
cd ${MK_RSTDIR}
/bin/rm -f InData/*bin
/bin/rm -f InData/*til
/bin/rm -f OutData/*til
cp -p $RSTMERDIRS/InData/${RSTMERFIL1}.bin InData
cp -p $RSTMERDIRS/InData/${RSTMERFIL2}.bin InData
cp -p $RSTMERDIRS/InData/${RSTMERFIL3}.bin InData
cp -p $RSTMERDIRS/InData/CF0180x6C_DE1440xPE0720-Pfafstetter.til InData
cp -p $RSTMERDIRS/OutData/CF0180x6C_TM0720xTM0410-Pfafstetter.til OutData

#source /discover/nobackup/projects/gmao/t2ssp/build/hera/GEOSodas/src/g5_modules
module purge
module load comp/intel-15.0.2.164
module load mpi/impi-5.0.3.048
module load lib/mkl-15.0.2.164
module load other/comp/gcc-4.6.3-sp1
module load other/SIVO-PyD/spd_1.20.0_gcc-4.6.3-sp1_mkl-15.0.0.090
module load other/git-2.3.1
module list
echo ${MK_RSTDIR}
echo CONVERT catch
#ls -l InData
./mk_Restarts -catch -surflay 50
#ls -l OutData
echo CONVERT lake
#ls -l InData
./mk_Restarts -lake
#ls -l OutData
echo CONVERT landice
#ls -l InData
./mk_Restarts -landice
#ls -l OutData
set convstatus = $status
if ($convstatus > 0 ) then
   echo "EXIT ON CONVERT ERROR"
   exit
endif

echo MOVE 3 FILES TO $RSTMERDIRS/OutData/$icdate
mkdir -p $RSTMERDIRS/OutData/${icdate}
mv ${MK_RSTDIR}/OutData/*catch*bin $RSTMERDIRS/OutData/${icdate}/catch_internal_rst
mv ${MK_RSTDIR}/OutData/*lake*bin $RSTMERDIRS/OutData/${icdate}/lake_internal_rst
mv ${MK_RSTDIR}/OutData/*landice*bin $RSTMERDIRS/OutData/${icdate}/landice_internal_rst
mv ${MK_RSTDIR}/OutData/vegdyn_internal_rst $RSTMERDIRS/OutData/${icdate}/vegdyn_internal_rst

echo CLEAN  ${MK_RSTDIR} InData and OutData
/bin/rm -f ${MK_RSTDIR}/OutData/*til
/bin/rm -f ${MK_RSTDIR}/InData/*til
/bin/rm -f ${MK_RSTDIR}/InData/*_rst*
/bin/rm -f ${MK_RSTDIR}/OutData/*dat*
/bin/rm -rf ${RSTMERDIRS}/InData/tmpagcm_replay${icdate}
#/bin/rm -f ${RSTMERDIRS}/InData/*${icdate}_00z.bin  # KEEP all ATM retarts

@ n = $n + 1
sleep 5

end
echo "PROCESSED $n FILES"

end
echo "PROCESSED $MONS"

###################################################################################
#   Prepare the set of perturbed IC from MERRA2 analysis and replay
#   SCHEME: MERRA-2 restarts one day apart pp=central+(day1-day2)/2:  fvcore moist
#   INPUT:  MERRA-2 analysis:/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/
#                            d5124_m2_jan10/rs/Y2011/M10/d5124_m2_jan10.*20111003_21z.bin
#           MERRA-2 REPLAY:  /gpfsm/dnb02/projects/p58/subx/agcm_replay/
#   OUTPUT: /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2/OutData
#   NOTE: ONLY use jmarshak account
# dmget /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan91/rs/Y1999/M??/d5124_m2_jan91.[f,m]*_internal_rst.199906??_21z.bin
#/discover/nobackup/projects/gmao/m2oasf/build/yuri-h54p3-04042017-moist/GEOSodas/src
# 20010814 and 20010819 missing
###################################################################################

set IYR = 2008
#set MONS="1 2 3 4 5 6 7 8 9 10 11 12"
#set MONS="9 10 11 12"
#set MONS="5 6 7 8"
#set MONS="1 2 3 4"

set MONS="8"
foreach IMO ( $MONS )

if ( $IMO == 1 )  set DAYS="06 11 16 21 26 31 01"
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

foreach DAY1 ( $DAYS )

if ($IMO < 10 ) set icdate = ${IYR}0${IMO}${DAY1}
if ($IMO > 9 ) set icdate = ${IYR}${IMO}${DAY1}
echo "PROCESSING $icdate ___________________________________________________________"

set icYYYY = `echo $icdate | cut -c1-4`
set icMO   = `echo $icdate | cut -c5-6`
set icDD   = `echo $icdate | cut -c7-8`

set md='79'
if ($icYYYY > 1991 ) set md='91'
if ($icYYYY > 2000 ) set md='00'
if ($icYYYY > 2010 ) set md='10'
#        1991 in md 79 and 91; 2000 in md 91 and 00; 2010 in md 00 and 10
# taking 1991 from  79;        2000 from  91;        2010 from  00
if ($icdate == 19920101) set md='79'
if ($icdate == 20010101) set md='91'
if ($icdate == 20110101) set md='00'

if ( $icMO == '01' ) set icMMM = 'jan'
if ( $icMO == '02' ) set icMMM = 'feb'
if ( $icMO == '03' ) set icMMM = 'mar'
if ( $icMO == '04' ) set icMMM = 'apr'
if ( $icMO == '05' ) set icMMM = 'may'
if ( $icMO == '06' ) set icMMM = 'jun'
if ( $icMO == '07' ) set icMMM = 'jul'
if ( $icMO == '08' ) set icMMM = 'aug'
if ( $icMO == '09' ) set icMMM = 'sep'
if ( $icMO == '10' ) set icMMM = 'oct'
if ( $icMO == '11' ) set icMMM = 'nov'
if ( $icMO == '12' ) set icMMM = 'dec'

set icMND = ${icMMM}${icDD}

@ icdatem1 = $icdate - 1
@ iym1 = ${icYYYY} - 1

if ( ${icMND} == 'jan01') set icdatem1 = ${iym1}1231
if ( ${icMND} == 'apr01') set icdatem1 = ${icYYYY}0331
if ( ${icMND} == 'may01') set icdatem1 = ${icYYYY}0430
set AMO1 = M`echo $icdatem1 | cut -c5-6`

echo get MERRA2 all: catch fvcore gocart irrad lake landice moist pchem saltwater solar turb - internal
echo get MERRA2 all:                                   agcm moist       saltwater surf  turb - import
set RSTMERARCH = "/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${icYYYY}"
if ( ${icMND} == 'jan01') set RSTMERARCH = "/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${iym1}"
echo "dmget from $RSTMERARCH"
if ( ${icMND} == 'jan01') then
   echo dmget from /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${iym1}
   dmget /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${iym1}/M12//d5124_m2_jan${md}.*_rst.${icdatem1}_21z.bin
else
   echo dmget from /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${icYYYY}
   dmget /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${icYYYY}/M??/d5124_m2_jan${md}.*_rst.${icdatem1}_21z.bin
endif


######################################################
# DEFINE THE RESTARTS FOR CONVERSION FROM ARCHIVE
#####################################################
echo DEFINE THE RESTARTS FOR CONVERSION   
set RSTMERctM1 = "d5124_m2_jan${md}.catch_internal_rst.${icdatem1}_21z.bin"
set RSTMERlkM1 = "d5124_m2_jan${md}.lake_internal_rst.${icdatem1}_21z.bin"
set RSTMERlnM1 = "d5124_m2_jan${md}.landice_internal_rst.${icdatem1}_21z.bin"

######################################################
# CHECK THE AVAILABILITY OF RESTARTS ON THE DISK
echo CHECK THE AVAILABILITY OF RESTARTS ON THE DISK
######################################################
#set RSTMERDIRS = "$HERASSP/restart"
set RSTMERDIRS = "/discover/nobackup/projects/gmao/m2oasf/restart"
/bin/mkdir -p ${RSTMERDIRS}/InData/$icdate
/bin/mkdir -p ${RSTMERDIRS}/OutData/$icdate
if ( -e ${RSTMERDIRS}/OutData/${icdate}/restarts.e${icdatem1}_21z.tar) then
   echo "RESTARTS ALREADY GENERATED, EXIT"
   exit
endif
if ( ! -e ${RSTMERDIRS}/OutData/${icdate}/restarts.e${icdatem1}_21z.tar ) then

dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.catch_internal_rst.${icdatem1}_21z.bin
dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.lake_internal_rst.${icdatem1}_21z.bin
dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.landice_internal_rst.${icdatem1}_21z.bin

GETDATA:
echo "Do you wish to get the data from archive now? (Default: NO or FALSE)"
#set GETD = $<
set GETD = "Y"

if( .$GETD == . ) then
      set   GETD  = FALSE
else
      set   GETD  = `echo   $GETD | tr "[:lower:]" "[:upper:]"`
      if(  $GETD == "Y"     | \
           $GETD == "YES"   | \
           $GETD == "T"     | \
           $GETD == "TRUE"  ) set GETD = TRUE
      if(  $GETD == "N"     | \
           $GETD == "NO"    | \
           $GETD == "F"     | \
           $GETD == "FALSE" ) set GETD = FALSE
      if( $GETD != TRUE & $GETD != FALSE ) then
            echo
            echo "Answer must be set equal to TRUE or FALSE!"
            goto  GETDATA
      else
            echo
      endif
endif
if ($GETD == TRUE) then
   echo "GETTING DATA FROM ARCHIVE NOW ... "
   dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.catch_internal_rst.${icdatem1}_21z.bin
   dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.lake_internal_rst.${icdatem1}_21z.bin
   dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.landice_internal_rst.${icdatem1}_21z.bin

   ## copy MERRA2 to InData ; we will need all but agcm
   cp -p $RSTMERARCH/$AMO1/d5124_m2_jan${md}.*_rst.${icdatem1}_21z.bin ${RSTMERDIRS}/InData/${icdate}
   /bin/mv ${RSTMERDIRS}/InData/${icdate}/$RSTMERctM1 ${RSTMERDIRS}/InData/${icdate}/catch_internal_rst.${icdatem1}_21z.bin
   /bin/mv ${RSTMERDIRS}/InData/${icdate}/$RSTMERlkM1 ${RSTMERDIRS}/InData/${icdate}/lake_internal_rst.${icdatem1}_21z.bin
   /bin/mv ${RSTMERDIRS}/InData/${icdate}/$RSTMERlnM1 ${RSTMERDIRS}/InData/${icdate}/landice_internal_rst.${icdatem1}_21z.bin

else
   echo "GET RESTARTS LATER, EXIT NOW..."
   exit 
endif
endif

#end
#end

#######################################################
#              NOW CONVERT 
#######################################################
######################################################
#   prepare the set of IC from F25 (MOM), MERRA2 (ATM and VEGDYN), H30 (saltwater and seaice)
#   MERRA-2 restarts:  catch lake landice - convert, fvcore irrad moist(2) pchem solar surf turb(2) - as is
#   from /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan10/rs/Y2011/M10/d5124_m2_jan10.*20111003_21z.bin
#   to   $HERASP/restart/
######################################################

set RSTMERFIL1 = "catch_internal_rst.${icdatem1}_21z"
set RSTMERFIL2 = "lake_internal_rst.${icdatem1}_21z"
set RSTMERFIL3 = "landice_internal_rst.${icdatem1}_21z"
set MK_RSTDIR = "/discover/nobackup/projects/gmao/m2oasf/build/geos-s2s/GEOSodas/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSsurface_GridComp/GEOSland_GridComp/GEOScatch_GridComp/mk_restarts"

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
#              NOW CONVERT IN MK_RSTDIR!!!!!
#######################################################
echo NOW CONVERT:
echo $RSTMERctM1
echo $RSTMERlkM1
echo $RSTMERlnM1
cd ${MK_RSTDIR}
/bin/rm -f InData/*bin
/bin/rm -f InData/*til
/bin/rm -f OutData/*til
cp -p $RSTMERDIRS/InData/${icdate}/${RSTMERFIL1}.bin InData
cp -p $RSTMERDIRS/InData/${icdate}/${RSTMERFIL2}.bin InData
cp -p $RSTMERDIRS/InData/${icdate}/${RSTMERFIL3}.bin InData
cp -p $RSTMERDIRS/InData/CF0180x6C_DE1440xPE0720-Pfafstetter.til InData
cp -p $RSTMERDIRS/OutData/CF0180x6C_TM0720xTM0410-Pfafstetter.til OutData

#source /discover/nobackup/projects/gmao/m2oasf/build/geos-s2s/GEOSodas/src/g5_modules 
# where geos-s2s -> yuri-h54p3-04042017-moist
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
mk_Restarts -catch -surflay 50
#ls -l OutData
echo CONVERT lake
#ls -l InData
mk_Restarts -lake
#ls -l OutData
echo CONVERT landice
#ls -l InData
mk_Restarts -landice
#ls -l OutData
set convstatus = $status
if ($convstatus > 0 ) then
   echo "EXIT ON CONVERT ERROR"
   exit
endif

echo MOVE 4 FILES TO $RSTMERDIRS/OutData/$icdate
mv ${MK_RSTDIR}/OutData/*catch*bin $RSTMERDIRS/OutData/${icdate}/catch_internal_rst
mv ${MK_RSTDIR}/OutData/*lake*bin $RSTMERDIRS/OutData/${icdate}/lake_internal_rst
mv ${MK_RSTDIR}/OutData/*landice*bin $RSTMERDIRS/OutData/${icdate}/landice_internal_rst
mv ${MK_RSTDIR}/OutData/vegdyn_internal_rst $RSTMERDIRS/OutData/${icdate}/vegdyn_internal_rst

echo CLEAN  ${MK_RSTDIR} InData and OutData
/bin/rm -f ${MK_RSTDIR}/OutData/*til
/bin/rm -f ${MK_RSTDIR}/InData/*til
/bin/rm -f ${MK_RSTDIR}/InData/*_rst*
/bin/rm -f ${MK_RSTDIR}/OutData/*dat*

echo RENAME OTHER 12 IN $RSTMERDIRS/InData/$icdate
cd ${RSTMERDIRS}
/bin/mv InData/${icdate}/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/fvcore_internal_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.gocart_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/gocart_internal_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.irrad_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/irrad_internal_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.moist_import_rst.${icdatem1}_21z.bin OutData/${icdate}/moist_import_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.moist_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/moist_internal_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.pchem_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/pchem_internal_rst
#/bin/mv InData/${icdate}/d5124_m2_jan${md}.saltwater_import_rst.${icdatem1}_21z.bin OutData/${icdate}/saltwater_import_rst
#/bin/mv InData/${icdate}/d5124_m2_jan${md}.saltwater_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/saltwater_internal_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.solar_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/solar_internal_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.surf_import_rst.${icdatem1}_21z.bin OutData/${icdate}/surf_import_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.turb_import_rst.${icdatem1}_21z.bin OutData/${icdate}/turb_import_rst
/bin/mv InData/${icdate}/d5124_m2_jan${md}.turb_internal_rst.${icdatem1}_21z.bin OutData/${icdate}/turb_internal_rst

set numrst = `ls -1 OutData/$icdate | wc -l`
if ( $numrst == 14 ) then
   cd OutData/${icdate}
   tar cvf restarts.e${icdatem1}_21z.tar *_rst
   set ts = $status
   /bin/rm -rf ${RSTMERDIRS}/InData/${icdate}
   if ( $ts == 0 ) /bin/rm -f *_rst
   echo RESTARTS GENERATED
else
   echo CONVERSION FAILED
endif

end
end

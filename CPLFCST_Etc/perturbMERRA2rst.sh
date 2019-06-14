###################################################################################
#   Prepare the set of perturbed IC from MERRA2 analysis
#   SCHEME: MERRA-2 restarts one day apart pp=central+(day1-day2)/2:  fvcore and moist
#   INPUT:  MERRA-2 analysis:/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/
#                            d5124_m2_jan10/rs/Y2011/M10/d5124_m2_jan10.*20111003_03z.bin
#   OUTPUT: /discover/nobackup/projects/gmao/m2oasf/restart/OutData
#   NOTE: ONLY use jmarshak account
# dmget /archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan91/rs/Y1999/M??/d5124_m2_jan91.[f,m]*_internal_rst.199906??_03z.bin
###################################################################################

set IYR = 2015
#set MONS="1 2 3 4 5 6 7 8 9 10 11 12"
#set MONS="9 10 11 12"
#set MONS="5 6 7 8"
#set MONS="1 2 3 4"
set MONS="7"
foreach IMO ( $MONS )

if ( $IMO == 1 )  set DAYS="06 11 16 21 26 31 01"
if ( $IMO == 2 )  set DAYS="05 10 15 20 25"
#if ( $IMO == 3 )  set DAYS="02 07 12 17 22 27"
if ( $IMO == 3 )  set DAYS="02"
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
#set icdate = 19990411
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
@ febd = 28
if (${icYYYY} == 1980) @ febd = 29
if (${icYYYY} == 1984) @ febd = 29
if (${icYYYY} == 1988) @ febd = 29
if (${icYYYY} == 1992) @ febd = 29
if (${icYYYY} == 1996) @ febd = 29
if (${icYYYY} == 2000) @ febd = 29
if (${icYYYY} == 2004) @ febd = 29
if (${icYYYY} == 2008) @ febd = 29
if (${icYYYY} == 2012) @ febd = 29
if (${icYYYY} == 2016) @ febd = 29

if ( ${icMND} == 'jan01') set icdatem1 = ${iym1}1231
if ( ${icMND} == 'apr01') set icdatem1 = ${icYYYY}0331
if ( ${icMND} == 'may01') set icdatem1 = ${icYYYY}0430

@ icdatem2 = $icdatem1 - 1
if ( ${icMND} == 'mar02' ) set icdatem2 = ${icYYYY}02${febd}
if ( ${icMND} == 'nov02' ) set icdatem2 = ${icYYYY}1031
if ( ${icMND} == 'dec02' ) set icdatem2 = ${icYYYY}1130

@ icdatem3 = $icdatem2 - 1
if ( ${icMND} == 'sep03' ) set icdatem3 = ${icYYYY}0831
if ( ${icMND} == 'oct03' ) set icdatem3 = ${icYYYY}0930

@ icdatem4 = $icdatem3 - 1
if ( ${icMND} == 'aug04' ) set icdatem4 = ${icYYYY}0731

@ icdatem5 = $icdatem4 - 1
if ( ${icMND} == 'feb05' ) set icdatem5 = ${icYYYY}0131
if ( ${icMND} == 'jun05' ) set icdatem5 = ${icYYYY}0531
if ( ${icMND} == 'jul05' ) set icdatem5 = ${icYYYY}0630

echo "PERTURB USING FOLLOWING DATES "
echo "CENTRAL: $icdate"
echo "MERRA-2: $icdatem1 $icdatem2 $icdatem3 $icdatem4 $icdatem5"

set RSTMERDIRS = "/discover/nobackup/projects/gmao/m2oasf/restart"

######################################################
# LOCATION - central for perturbation
# HERE WE NEED fvcore, moist that will be perturbed
######################################################
echo "GET CENTRAL DATA TO InData"
mkdir -p ${RSTMERDIRS}/OutData/${icdate}
if ( ! -e ${RSTMERDIRS}/OutData/${icdate}/restarts.e${icdatem1}_21z.tar ) then
   echo "Central data not yet in place, do conversion first"
   exit
endif
if ( -e ${RSTMERDIRS}/OutData/${icdate}/moist_internal_rstPlus2 ) then
   ls -l ${RSTMERDIRS}/OutData/${icdate}/moist_internal_rstPlus2
   echo "Perturbed I.C. exist, exiting."
   exit
endif
cd ${RSTMERDIRS}/InData
tar xvf ${RSTMERDIRS}/OutData/${icdate}/restarts.e${icdatem1}_21z.tar
/bin/mv fvcore_internal_rst ${RSTMERDIRS}/OutData/central_fort.13
/bin/mv moist_internal_rst ${RSTMERDIRS}/OutData/central_fort.23
/bin/rm -f *_rst
######################################################
# DEFINE THE RESTARTS FOR PERTURBATION
# input dtpert : fvcore, moist from MERRA2
#####################################################
echo DEFINE THE RESTARTS FOR PERTURBATION
set RSTMERARCH = "/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${icYYYY}"
if ( ${icMND} == 'jan01') set RSTMERARCH = "/archive/users/gmao_ops/MERRA2/gmao_ops/GEOSadas-5_12_4/d5124_m2_jan${md}/rs/Y${iym1}"
set AMO1 = M`echo $icdatem1 | cut -c5-6`
set AMO2 = M`echo $icdatem2 | cut -c5-6`
set AMO3 = M`echo $icdatem3 | cut -c5-6`
set AMO4 = M`echo $icdatem4 | cut -c5-6`
set AMO5 = M`echo $icdatem5 | cut -c5-6`

set RSTMERfvM1 = "d5124_m2_jan${md}.fvcore_internal_rst.${icdatem1}_03z.bin"
set RSTMERfvM2 = "d5124_m2_jan${md}.fvcore_internal_rst.${icdatem2}_03z.bin"
set RSTMERfvM3 = "d5124_m2_jan${md}.fvcore_internal_rst.${icdatem3}_03z.bin"
set RSTMERfvM4 = "d5124_m2_jan${md}.fvcore_internal_rst.${icdatem4}_03z.bin"
set RSTMERfvM5 = "d5124_m2_jan${md}.fvcore_internal_rst.${icdatem5}_03z.bin"
set RSTMERmoM1 = "d5124_m2_jan${md}.moist_internal_rst.${icdatem1}_03z.bin"
set RSTMERmoM2 = "d5124_m2_jan${md}.moist_internal_rst.${icdatem2}_03z.bin"
set RSTMERmoM3 = "d5124_m2_jan${md}.moist_internal_rst.${icdatem3}_03z.bin"
set RSTMERmoM4 = "d5124_m2_jan${md}.moist_internal_rst.${icdatem4}_03z.bin"
set RSTMERmoM5 = "d5124_m2_jan${md}.moist_internal_rst.${icdatem5}_03z.bin"

######################################################
# CHECK THE AVAILABILITY OF RESTARTS ON THE DISK
echo CHECK THE AVAILABILITY OF RESTARTS ON THE DISK
######################################################
#set PAIRS = "1 2 3 4"
set PAIRS = "1 2"
if ( ! -e ${RSTMERDIRS}/OutData/${icdate}/fvcore_internal_rstPlus1 ) then

dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem1}_03z.bin
dmls -l $RSTMERARCH/$AMO2/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem2}_03z.bin
dmls -l $RSTMERARCH/$AMO3/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem3}_03z.bin
dmls -l $RSTMERARCH/$AMO4/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem4}_03z.bin
dmls -l $RSTMERARCH/$AMO5/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem5}_03z.bin
dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.moist_internal_rst.${icdatem1}_03z.bin
dmls -l $RSTMERARCH/$AMO2/d5124_m2_jan${md}.moist_internal_rst.${icdatem2}_03z.bin
dmls -l $RSTMERARCH/$AMO3/d5124_m2_jan${md}.moist_internal_rst.${icdatem3}_03z.bin
dmls -l $RSTMERARCH/$AMO4/d5124_m2_jan${md}.moist_internal_rst.${icdatem4}_03z.bin
dmls -l $RSTMERARCH/$AMO5/d5124_m2_jan${md}.moist_internal_rst.${icdatem5}_03z.bin

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
   echo dmget $RSTMERARCH/${AMO1}/d5124_m2_jan${md}.[f,m]*_internal_rst.*_03z.bin
   dmget $RSTMERARCH/${AMO1}/d5124_m2_jan${md}.[f,m]*_internal_rst.*_03z.bin
   dmget $RSTMERARCH/${AMO2}/d5124_m2_jan${md}.[f,m]*_internal_rst.*_03z.bin
   dmget $RSTMERARCH/${AMO3}/d5124_m2_jan${md}.[f,m]*_internal_rst.*_03z.bin
   dmget $RSTMERARCH/${AMO4}/d5124_m2_jan${md}.[f,m]*_internal_rst.*_03z.bin
   dmget $RSTMERARCH/${AMO5}/d5124_m2_jan${md}.[f,m]*_internal_rst.*_03z.bin
   dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem1}_03z.bin
   dmls -l $RSTMERARCH/$AMO2/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem2}_03z.bin
   dmls -l $RSTMERARCH/$AMO3/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem3}_03z.bin
   dmls -l $RSTMERARCH/$AMO4/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem4}_03z.bin
   dmls -l $RSTMERARCH/$AMO5/d5124_m2_jan${md}.fvcore_internal_rst.${icdatem5}_03z.bin
   dmls -l $RSTMERARCH/$AMO1/d5124_m2_jan${md}.moist_internal_rst.${icdatem1}_03z.bin
   dmls -l $RSTMERARCH/$AMO2/d5124_m2_jan${md}.moist_internal_rst.${icdatem2}_03z.bin
   dmls -l $RSTMERARCH/$AMO3/d5124_m2_jan${md}.moist_internal_rst.${icdatem3}_03z.bin
   dmls -l $RSTMERARCH/$AMO4/d5124_m2_jan${md}.moist_internal_rst.${icdatem4}_03z.bin
   dmls -l $RSTMERARCH/$AMO5/d5124_m2_jan${md}.moist_internal_rst.${icdatem5}_03z.bin
   cp -p $RSTMERARCH/$AMO1/$RSTMERfvM1 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO2/$RSTMERfvM2 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO3/$RSTMERfvM3 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO4/$RSTMERfvM4 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO5/$RSTMERfvM5 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO1/$RSTMERmoM1 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO2/$RSTMERmoM2 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO3/$RSTMERmoM3 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO4/$RSTMERmoM4 ${RSTMERDIRS}/InData
   cp -p $RSTMERARCH/$AMO5/$RSTMERmoM5 ${RSTMERDIRS}/InData
else
   echo "GET RESTARTS LATER, EXIT NOW..."
   exit 
endif
endif

#######################################################
#              NOW PERTURB PAIRS
#######################################################
echo NOW PERTURB N TIMES 
cd ${RSTMERDIRS}/OutData
/bin/rm -f fort.*
# analysis data for perturbation is from MERRA-2
/bin/mv central_fort.13  fort.13
/bin/mv central_fort.23  fort.23
foreach PERTPAIR ( $PAIRS )
  echo "DOING $PERTPAIR"
  if ( ${PERTPAIR} == '1') then
     set rstfvcore1 = ${RSTMERfvM1}
     set rstfvcore2 = ${RSTMERfvM2}
     set rstmoisti1 = ${RSTMERmoM1}
     set rstmoisti2 = ${RSTMERmoM2}
  endif
  if ( ${PERTPAIR} == '2') then
     set rstfvcore1 = ${RSTMERfvM2}
     set rstfvcore2 = ${RSTMERfvM3}
     set rstmoisti1 = ${RSTMERmoM2}
     set rstmoisti2 = ${RSTMERmoM3}
  endif
  if ( ${PERTPAIR} == '3') then
     set rstfvcore1 = ${RSTMERfvM3}
     set rstfvcore2 = ${RSTMERfvM4}
     set rstmoisti1 = ${RSTMERmoM3}
     set rstmoisti2 = ${RSTMERmoM4}
  endif
  if ( ${PERTPAIR} == '4') then
     set rstfvcore1 = ${RSTMERfvM4}
     set rstfvcore2 = ${RSTMERfvM5}
     set rstmoisti1 = ${RSTMERmoM4}
     set rstmoisti2 = ${RSTMERmoM5}
  endif

  echo previous days data for perturbation is from MERRA-2 analysis
  /bin/cp ${RSTMERDIRS}/InData/$rstfvcore1 fort.11
  /bin/cp ${RSTMERDIRS}/InData/$rstmoisti1 fort.21
  /bin/cp ${RSTMERDIRS}/InData/$rstfvcore2 fort.12
  /bin/cp ${RSTMERDIRS}/InData/$rstmoisti2 fort.22

  echo Execute perturb IC
  #set SRCDIRH54p1 = "/discover/nobackup/projects/gmao/t2ssp/build/yuri-h54p1c180o05_1_moistdiag/GEOSodas/src"
  #set SRCDIRH54p3 = "/discover/nobackup/projects/gmao/m2oasf/build/yuri-h54p3-06032017/GEOSodas/src"
  #set SRCDIRH54p3 = "/discover/nobackup/projects/gmao/m2oasf/build/yuri-h54p3-04042017-moist/GEOSodas/src"
  #source $SRCDIRH54p3/g5_modules
  #in $M2OASF/restart compiled - slices=72x6
  # ifort fvcore_pert.f -o fvcore_pert.x
  # ifort moist_pert.f -o moist_pert.x
  echo "$M2OASF/restart/fvcore_pert.x"
  $M2OASF/restart/fvcore_pert.x
  echo "$M2OASF/restart/moist_pert.x"
  $M2OASF/restart/moist_pert.x

  echo Move perturbed IC to OutData/icdate
  /bin/mv fort.31 $icdate/fvcore_internal_rstPlus${PERTPAIR}
  /bin/mv fort.32 $icdate/fvcore_internal_rstMinu${PERTPAIR}
  /bin/mv fort.41 $icdate/moist_internal_rstPlus${PERTPAIR}
  /bin/mv fort.42 $icdate/moist_internal_rstMinu${PERTPAIR}

  # clean
  wait
  echo "PERTURBATION SET GENERATED FROM :"
  echo "$rstfvcore1 and $rstfvcore2"
  echo "$rstmoisti1 and $rstmoisti2"
  ls -l $icdate/*Plus${PERTPAIR}
  ls -l $icdate/*Minu${PERTPAIR}
  /bin/rm -f fort.11
  /bin/rm -f fort.12
  /bin/rm -f fort.21
  /bin/rm -f fort.22
end
echo "PERTURBATION COMPLETED FOR $icdate"
/bin/rm -f fort.*
cd ${RSTMERDIRS}/InData
rm -f d5124_m2_*bin

echo "PROCESSED DAY $icdate"
sleep 10
end
echo "PROCESSED MONS DAYS $IMO $DAYS"
end
echo "PROCESSED MONS $MONS" 

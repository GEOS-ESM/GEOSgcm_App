###################################################################################
#   Prepare the set of perturbed IC from GEOSS2S  analysis
#   SCHEME: restarts one day apart pp=central+(day1-day2)/2:  fvcore and moist
#   INPUT:  analysis every 5th day starting from Jan01
#   OUTPUT: perturbed restarts are placed $GEOSS2S/run/YYYY/MMMDD/OutData directory
#   script uses two scripts written by Deepthi to perturb fvcore and moist restarts:
#     echo Execute perturb IC fvcore
#     fvcore_pert.sh
#     echo Execute perturb IC moist
#     moist_pert.sh
###################################################################################
set icdate = $1
set ENSM = $2
echo "PROCESSING $icdate ___________________________________________________________"

set icYYYY = `echo $icdate | cut -c1-4`
set icMO   = `echo $icdate | cut -c5-6`
set icDD   = `echo $icdate | cut -c7-8`

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

echo "PERTURB USING FOLLOWING DATES "
echo "CENTRAL: $icdate"
echo "PERTURB: $icdatem1 $icdatem2 $icdatem3"

######################################################
# LOCATION - central for perturbation
# HERE WE NEED fvcore, moist that will be perturbed
######################################################
#set RSTMERDIRS = "$GEOSS2S/runx/${icYYYY}/${icMND}/ens$ENSM"
set RSTMERDIRS = "$GEOSS2S/runx/${icYYYY}/${icMND}"
pwd
mkdir -p $RSTMERDIRS/OutData
echo "SPECIFY CENTRAL DATA "
if ( ! -e ${RSTMERDIRS}/ens$ENSM/central_moist_internal_rst ) then
   echo "Central moist is not yet in place, exit"
   exit
endif
if ( ! -e ${RSTMERDIRS}/ens$ENSM/central_fvcore_internal_rst ) then
   echo "Central fvcore is not yet in place, exit"
   exit
endif

#####################################################
echo "DEFINE THE RESTARTS FOR PERTURBATION"
#####################################################
set RESTARTS = '/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/hindcast_restarts/'
set RSTMERfvM1 = "$RESTARTS/fvcore_internal_checkpoint.${icdatem1}_2100z.bin"
set RSTMERfvM2 = "$RESTARTS/fvcore_internal_checkpoint.${icdatem2}_2100z.bin"
set RSTMERfvM3 = "$RESTARTS/fvcore_internal_checkpoint.${icdatem3}_2100z.bin"
set RSTMERmoM1 = "$RESTARTS/moist_internal_checkpoint.${icdatem1}_2100z.bin"
set RSTMERmoM2 = "$RESTARTS/moist_internal_checkpoint.${icdatem2}_2100z.bin"
set RSTMERmoM3 = "$RESTARTS/moist_internal_checkpoint.${icdatem3}_2100z.bin"

######################################################
echo "NOW PERTURB PAIRS"
#######################################################
set PAIRS = "1 2"
cd ${RSTMERDIRS}/OutData
/bin/rm -f fort.*
#ln -sf fort.13 ${RSTMERDIRS}/central_fvcore_internal_rst 
#ln -sf fort.23 ${RSTMERDIRS}/central_moist_internal_rst
/bin/cp -p ${RSTMERDIRS}/ens$ENSM/central_fvcore_internal_rst fort.13
/bin/cp -p ${RSTMERDIRS}/ens$ENSM/central_moist_internal_rst fort.23
/bin/cp -p $GEOSS2S/util/fvcore_pert.sh ./
/bin/cp -p $GEOSS2S/util/moist_pert.sh ./
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

  echo previous days data for perturbation is from analysis
  #ln -sf fort.11 $rstfvcore1
  #ln -sf fort.21 $rstmoisti1
  #ln -sf fort.12 $rstfvcore2
  #ln -sf fort.22 $rstmoisti2
  /bin/cp -p $rstfvcore1 fort.11
  /bin/cp -p $rstmoisti1 fort.21
  /bin/cp -p $rstfvcore2 fort.12
  /bin/cp -p $rstmoisti2 fort.22

  echo Execute perturb IC fvcore
  fvcore_pert.sh
  echo Execute perturb IC moist
  moist_pert.sh

  echo Rename perturbed IC 
  /bin/mv fort.31 fvcore_internal_rstPlus${PERTPAIR}
  /bin/mv fort.32 fvcore_internal_rstMinu${PERTPAIR}
  /bin/mv fort.41 moist_internal_rstPlus${PERTPAIR}
  /bin/mv fort.42 moist_internal_rstMinu${PERTPAIR}

  # clean
  wait
  echo "PERTURBATION SET GENERATED FROM :"
  echo "$rstfvcore1 and $rstfvcore2"
  echo "$rstmoisti1 and $rstmoisti2"
  ls -l *Plus${PERTPAIR}
  ls -l *Minu${PERTPAIR}
  /bin/rm -f fort.11
  /bin/rm -f fort.12
  /bin/rm -f fort.21
  /bin/rm -f fort.22
end
echo "PERTURBATION COMPLETED FOR $icdate"
/bin/rm -f fort.*
/bin/rm -f *_pert.sh
cd ${RSTMERDIRS}
echo "PROCESSED DAY $icdate"

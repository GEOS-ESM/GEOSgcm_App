#!/bin/csh
######################################################
#   prepare the set of IC from F25 (MOM), MERRA2 (ATM and VEGDYN), H30 (saltwater and seaice)
#   ODAS MOM
#   from /archive/u/gmaofcst/GEOS5.0/seasonal/mar02/ens1/restarts/Y2011/restarts.e20110302_00z.tar
#   to   /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/ODAS/RESTART.e*_00z
#   recommend to get ODAS beforehand from dirac for years 1999-2009
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/jan??/ens1/restarts/Y2001/restarts.e200101?[1,6]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/feb??/ens1/restarts/Y2001/restarts.e200102?[0,5]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/mar??/ens1/restarts/Y2001/restarts.e200103?[2,7]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/apr??/ens1/restarts/Y2001/restarts.e200104?[1,6]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/may??/ens1/restarts/Y2001/restarts.e200105?[1,6]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/jun??/ens1/restarts/Y2001/restarts.e200106?[0,5]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/jul??/ens1/restarts/Y2001/restarts.e200107?[0,5]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/aug??/ens1/restarts/Y2001/restarts.e200108?[4,9]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/sep??/ens1/restarts/Y2001/restarts.e200109?[3,8]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/oct??/ens1/restarts/Y2001/restarts.e200110?[3,8]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/nov??/ens1/restarts/Y2001/restarts.e200111?[2,7]_00z.tar
#   dmget /archive/u/gmaofcst/GEOS5.0/seasonal/dec??/ens1/restarts/Y2001/restarts.e200112?[2,7]_00z.tar
#   MISSING 2010 apr01 20100401 yes, not anymore
#   MISSING 2011 jun10 20110610 yes, not anymore
#   MISSING 2012 aug29 20120829 yes, not anymore
#   MISSING 2014 oct23,oct28 20141023,20141028 yes, not anymore
######################################################

set IYR = 2015

#set MONS = "7 8 9 10 11 12"
set MONS = "10"
#set MONS = "1 2 3 4 5 6"

foreach IMO ( $MONS )

if ( $IMO == 1 )  set DAYS="01 06 11 16 21 26 31"
if ( $IMO == 2 )  set DAYS="05 10 15 20 25"
if ( $IMO == 3 )  set DAYS="02 07 12 17 22 27"
if ( $IMO == 4 )  set DAYS="01 06 11 21 26"
if ( $IMO == 5 )  set DAYS="01 06 11 16 21 26 31"
if ( $IMO == 6 )  set DAYS="05 10 15 20 25 30"
if ( $IMO == 7 )  set DAYS="05 10 15 20 25 30"
if ( $IMO == 8 )  set DAYS="04 09 14 19 24 29"
if ( $IMO == 9 )  set DAYS="03 08 13 18 23 28"
#if ( $IMO == 10 ) set DAYS="03 08 13 18 23 28"
if ( $IMO == 10 ) set DAYS="03 08 13 18 23"
if ( $IMO == 11 ) set DAYS="02 07 12 17 22 27"
if ( $IMO == 12 ) set DAYS="02 07 12 17 22 27"

@ n = 0
foreach DAY1 ( $DAYS )

if ($IMO < 10 ) set icdate = ${IYR}0${IMO}${DAY1}
if ($IMO >  9 ) set icdate = ${IYR}${IMO}${DAY1}

#set icdate = $1

set icYYYY = `echo $icdate | cut -c1-4`
set icMM   = `echo $icdate | cut -c5-6`
set icDD   = `echo $icdate | cut -c7-8`

echo "PROCESSING $icYYYY $icMM : $icdate -----------------------------------------------------------------"

if ( $icMM == '01' ) set icMMM = 'jan'
if ( $icMM == '02' ) set icMMM = 'feb'
if ( $icMM == '03' ) set icMMM = 'mar'
if ( $icMM == '04' ) set icMMM = 'apr'
if ( $icMM == '05' ) set icMMM = 'may'
if ( $icMM == '06' ) set icMMM = 'jun'
if ( $icMM == '07' ) set icMMM = 'jul'
if ( $icMM == '08' ) set icMMM = 'aug'
if ( $icMM == '09' ) set icMMM = 'sep'
if ( $icMM == '10' ) set icMMM = 'oct'
if ( $icMM == '11' ) set icMMM = 'nov'
if ( $icMM == '12' ) set icMMM = 'dec'

set RSTODASDIRS = "/discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/ODAS"
set RSTODASFILE = "$RSTODASDIRS/RESTART.e${icdate}_00z"
set RSTODASARCH = "/archive/u/gmaofcst/GEOS5.0/seasonal/${icMMM}$icDD/ens1/restarts/Y{$icYYYY}"

if ( ${icDD} == '19' & ${icMMM} == 'aug') set RSTODASARCH = "/archive/u/gmaofcst/GEOS5.0/seasonal/${icMMM}${icDD}_orig/ens1/restarts/Y{$icYYYY}"

######################################################
# CHECK THE AVAILABILITY OF RESTARTS ON THE DISK
######################################################
echo $RSTODASARCH/restarts.e${icdate}_00z.tar
ls -l $RSTODASARCH/restarts.e${icdate}_00z.tar
if ( ! -e ${RSTODASFILE} ) then
dmls -l $RSTODASARCH/restarts.e${icdate}_00z.tar
GETDATA:
echo "Do you wish to get the data from archive now? (Default: NO or FALSE)"
####set GETD = $<
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
   dmget $RSTODASARCH/restarts.e${icdate}_00z.tar
   cp -pr ${RSTODASARCH}/restarts.e${icdate}_00z.tar $RSTODASDIRS
else
   echo "GET RESTARTS LATER, EXIT NOW..."
   exit
endif
endif

#######################################################
#               UNTAR THE RESTARTS
#######################################################
cd ${RSTODASDIRS}
tar xvf restarts.e${icdate}_00z.tar
set tarstatus = $status
if ( $tarstatus > 0 ) then
 echo "$icdate NOT AVAILABLE, EXIT"
 exit
endif
/bin/rm -f *_rst.e${icdate}_00z
/bin/rm -f restarts.e${icdate}_00z.tar
/bin/rm -f vegdyn.data.e${icdate}_00z

@ n = $n + 1
sleep 10
end

echo "PROCESSED $n FILES for ${IMO}"
end

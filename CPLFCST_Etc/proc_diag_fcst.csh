#!/bin/csh -f

# module load other/nco-4.5.5-gcc-5.3-sp3
# ocean_monthly.199401.nc

set year  = $1
set month = $2

## Get some date parameters

#@ year = 1994
#@ month = 1

@ yrmo = $year * 100 + $month

switch ($month)
  case '02':
    @ leap = $year % 4
    switch ($leap)
      case [1-3]:
        @ nday = 28
        breaksw
      default:
        @ nday = 29
        breaksw
    endsw
    breaksw
  case '09':
    @ nday = 30
    breaksw
  case '04':
    @ nday = 30
    breaksw
  case '06':
    @ nday = 30
    breaksw
  case '11':
    @ nday = 30
    breaksw
  default:
    @ nday = 31
    breaksw
endsw

echo "NUMBER DAYS: $nday"


## Determine which file in the directory that I should start from
## Then grab about 7 files after that to cover the month

#set alist_tmp = `ls /gpfsm/dnb42/projects/p17/bzhao/cp103/MOM_Output_Dailies/*.nc | nl | grep 199401`
set alist_tmp = `ls MOM_Output/*.nc | nl | grep ${yrmo}`


@ startnum = $alist_tmp[1]

#set flist = `ls /gpfsm/dnb42/projects/p17/bzhao/cp103/MOM_Output_Dailies/*.nc | tail +{$startnum} | head -8`
set flist = `ls MOM_Output/*.nc | tail +{$startnum} | head -8`


## Get the date off the first file.
## Then figure out how many fields in that file are in the month.

#echo $flist[1]

#@ no_in_first_file = `echo $flist[1] | cut -c 31-32`
# note to self... this cut command is dependent on what the file path looks like

set filename = `echo $flist[1] | rev | cut -d "/" -f 1 | rev`
@ no_in_first_file = `echo $filename  | cut -c 20-21`

@ startplace = 5 - $no_in_first_file
if ( ${startplace} < 0 ) @ startplace = 0
@ endplace = $startplace + $nday - 1

echo "FIRST LAST: $startplace $endplace"
echo "FLIST: $flist"

ncra -d time,{$startplace},{$endplace} $flist ocean_monthly.$yrmo.nc
set rc = $status 
if ( $rc != 0 ) echo "STATUS NCRA NOT EQUAL 0: $rc"


#discover05> proc_diag.csh 2013 08
#31
#2 32
#discover05> proc_diag.csh 2013 09
#31
#3 33



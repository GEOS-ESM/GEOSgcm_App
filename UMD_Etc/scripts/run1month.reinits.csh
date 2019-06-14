#!/bin/csh

#  script to brute force submit all the re-init jobs for a particular month 
#
#  usage:   ./run1month.reinits.csh YYYY MMM
#    where YYYY is year (19 
#    where mmm is year 3 char abbreviation of they month e.g. 'jan' 
#
#   example:    ./run1month.reinits.csh 2009 jun  
#

if ($#argv != 2) then 
 echo NEEDS 2 arguments 
 echo usage:   ./run1month.reinits.csh 2009 jun
 exit
endif

set yyyy           = ${1}
set mmm            = ${2}

echo $yyyy $mmm

if ( $yyyy > 2015 )then
  echo 'Year too big! STOP' $yyyy
  exit
endif
if ( $yyyy < 1999 )then
  echo 'Year too small! STOP' $yyyy
  exit
endif
 
set foundit = 0

if ($mmm == 'jan')then
 echo SUBMITTING  'jan01', 'jan06', 'jan11', 'jan16', 'jan21', 'jan26', 'jan31',
 sbatch --job-name=reinit_jan01${yyyy} MOtoM2O.csh $yyyy jan01 
 sbatch --job-name=reinit_jan06${yyyy} MOtoM2O.csh $yyyy jan06
 sbatch --job-name=reinit_jan10${yyyy} MOtoM2O.csh $yyyy jan11
 sbatch --job-name=reinit_jan16${yyyy} MOtoM2O.csh $yyyy jan16
 sbatch --job-name=reinit_jan20${yyyy} MOtoM2O.csh $yyyy jan21
 sbatch --job-name=reinit_jan26${yyyy} MOtoM2O.csh $yyyy jan26
 sbatch --job-name=reinit_jan31${yyyy} MOtoM2O.csh $yyyy jan31
 set foundit = 1
endif

if ($mmm == 'feb')then
 SUBMITTING  'feb05', 'feb10', 'feb15', 'feb2O', 'feb25',
 sbatch --job-name=reinit_feb05${yyyy} MOtoM2O.csh $yyyy feb05
 sbatch --job-name=reinit_feb10${yyyy} MOtoM2O.csh $yyyy feb10
 sbatch --job-name=reinit_feb15${yyyy} MOtoM2O.csh $yyyy feb15
 sbatch --job-name=reinit_feb20${yyyy} MOtoM2O.csh $yyyy feb20
 sbatch --job-name=reinit_feb25${yyyy} MOtoM2O.csh $yyyy feb25
 set foundit = 1
endif

if ($mmm == 'mar')then
 SUBMITTING  'mar02', 'mar07', 'mar12', 'mar17', 'mar22', 'mar27',
 sbatch --job-name=reinit_mar02${yyyy} MOtoM2O.csh $yyyy mar02
 sbatch --job-name=reinit_mar07${yyyy} MOtoM2O.csh $yyyy mar07
 sbatch --job-name=reinit_mar12${yyyy} MOtoM2O.csh $yyyy mar12
 sbatch --job-name=reinit_mar17${yyyy} MOtoM2O.csh $yyyy mar17
 sbatch --job-name=reinit_mar22${yyyy} MOtoM2O.csh $yyyy mar22
 sbatch --job-name=reinit_mar27${yyyy} MOtoM2O.csh $yyyy mar27
 set foundit = 1
endif

if ($mmm == 'apr')then
 SUBMITTING 'apr01', 'apr06', 'apr11', 'apr16', 'apr21', 'apr26',
 sbatch --job-name=reinit_apr01${yyyy} MOtoM2O.csh $yyyy apr01
 sbatch --job-name=reinit_apr06${yyyy} MOtoM2O.csh $yyyy apr06
 sbatch --job-name=reinit_apr11${yyyy} MOtoM2O.csh $yyyy apr11
 sbatch --job-name=reinit_apr16${yyyy} MOtoM2O.csh $yyyy apr16
 sbatch --job-name=reinit_apr21${yyyy} MOtoM2O.csh $yyyy apr21
 sbatch --job-name=reinit_apr26${yyyy} MOtoM2O.csh $yyyy apr26
 set foundit = 1
endif

if ($mmm == 'may')then
 SUBMITTING 'may01', 'may06', 'may11', 'may16', 'may21', 'may26', 'may31',
 sbatch --job-name=reinit_may01${yyyy} MOtoM2O.csh $yyyy may01
 sbatch --job-name=reinit_may06${yyyy} MOtoM2O.csh $yyyy may06
 sbatch --job-name=reinit_may11${yyyy} MOtoM2O.csh $yyyy may11
 sbatch --job-name=reinit_may16${yyyy} MOtoM2O.csh $yyyy may16
 sbatch --job-name=reinit_may21${yyyy} MOtoM2O.csh $yyyy may21
 sbatch --job-name=reinit_may26${yyyy} MOtoM2O.csh $yyyy may26
 sbatch --job-name=reinit_may31${yyyy} MOtoM2O.csh $yyyy may31
 set foundit = 1
endif

if ($mmm == 'jun')then
 echo SUBMITTING 'jun05', 'jun10', 'jun15', 'jun2O', 'jun25', 'jun30',
 sbatch --job-name=reinit_jun05${yyyy} MOtoM2O.csh $yyyy jun05
 sbatch --job-name=reinit_jun10${yyyy} MOtoM2O.csh $yyyy jun10
 sbatch --job-name=reinit_jun15${yyyy} MOtoM2O.csh $yyyy jun15
 sbatch --job-name=reinit_jun20${yyyy} MOtoM2O.csh $yyyy jun20
 sbatch --job-name=reinit_jun25${yyyy} MOtoM2O.csh $yyyy jun25
 sbatch --job-name=reinit_jun30${yyyy} MOtoM2O.csh $yyyy jun30
 set foundit = 1
endif

if ($mmm == 'jul')then
 SUBMITTING 'jul05', 'jul10', 'jul15', 'jul2O', 'jul25', 'jul30',
 sbatch --job-name=reinit_jul05${yyyy} MOtoM2O.csh $yyyy jul05
 sbatch --job-name=reinit_jul10${yyyy} MOtoM2O.csh $yyyy jul10
 sbatch --job-name=reinit_jul15${yyyy} MOtoM2O.csh $yyyy jul15
 sbatch --job-name=reinit_jul20${yyyy} MOtoM2O.csh $yyyy jul20
 sbatch --job-name=reinit_jul25${yyyy} MOtoM2O.csh $yyyy jul25
 sbatch --job-name=reinit_jul30${yyyy} MOtoM2O.csh $yyyy jul30
 set foundit = 1
endif

if ($mmm == 'aug')then
 SUBMITTING 'aug04', 'aug09', 'aug14', 'aug19', 'aug24', 'aug29',
 sbatch --job-name=reinit_aug04${yyyy} MOtoM2O.csh $yyyy aug04
 sbatch --job-name=reinit_aug09${yyyy} MOtoM2O.csh $yyyy aug09
 sbatch --job-name=reinit_aug14${yyyy} MOtoM2O.csh $yyyy aug14
 sbatch --job-name=reinit_aug19${yyyy} MOtoM2O.csh $yyyy aug19
 sbatch --job-name=reinit_aug24${yyyy} MOtoM2O.csh $yyyy aug24
 sbatch --job-name=reinit_aug29${yyyy} MOtoM2O.csh $yyyy aug29
 set foundit = 1
endif

if ($mmm == 'sep')then
 SUBMITTING 'sep03', 'sep08', 'sep13', 'sep18', 'sep23', 'sep28',
 sbatch --job-name=reinit_sep03${yyyy} MOtoM2O.csh $yyyy sep03
 sbatch --job-name=reinit_sep08${yyyy} MOtoM2O.csh $yyyy sep08
 sbatch --job-name=reinit_sep13${yyyy} MOtoM2O.csh $yyyy sep13
 sbatch --job-name=reinit_sep18${yyyy} MOtoM2O.csh $yyyy sep18
 sbatch --job-name=reinit_sep23${yyyy} MOtoM2O.csh $yyyy sep23
 sbatch --job-name=reinit_sep18${yyyy} MOtoM2O.csh $yyyy sep28
 set foundit = 1
endif

if ($mmm == 'oct')then
 SUBMITTING 'oct03', 'oct08', 'oct13', 'oct18', 'oct23', 'oct28',
 sbatch --job-name=reinit_oct03${yyyy} MOtoM2O.csh $yyyy oct03
 sbatch --job-name=reinit_oct08${yyyy} MOtoM2O.csh $yyyy oct08
 sbatch --job-name=reinit_oct13${yyyy} MOtoM2O.csh $yyyy oct13
 sbatch --job-name=reinit_oct18${yyyy} MOtoM2O.csh $yyyy oct18
 sbatch --job-name=reinit_oct23${yyyy} MOtoM2O.csh $yyyy oct23
 sbatch --job-name=reinit_oct28${yyyy} MOtoM2O.csh $yyyy oct28
 set foundit = 1
endif

if ($mmm == 'nov')then
 echo SUBMITTING 'nov02', 'nov07', 'nov12', 'nov17', 'nov22', 'nov27',
 sbatch --job-name=reinit_nov02${yyyy} MOtoM2O.csh $yyyy nov02
 sbatch --job-name=reinit_nov07${yyyy} MOtoM2O.csh $yyyy nov07
 sbatch --job-name=reinit_nov12${yyyy} MOtoM2O.csh $yyyy nov12
 sbatch --job-name=reinit_nov17${yyyy} MOtoM2O.csh $yyyy nov17
 sbatch --job-name=reinit_nov22${yyyy} MOtoM2O.csh $yyyy nov22
 sbatch --job-name=reinit_nov17${yyyy} MOtoM2O.csh $yyyy nov27
 set foundit = 1
endif

if ($mmm == 'dec')then
 SUBMITTING 'dec02', 'dec07', 'dec12', 'dec17', 'dec22', 'dec27'
 sbatch --job-name=reinit_dec02${yyyy} MOtoM2O.csh $yyyy dec02
 sbatch --job-name=reinit_dec07${yyyy} MOtoM2O.csh $yyyy dec07
 sbatch --job-name=reinit_dec12${yyyy} MOtoM2O.csh $yyyy dec12
 sbatch --job-name=reinit_dec17${yyyy} MOtoM2O.csh $yyyy dec17
 sbatch --job-name=reinit_dec22${yyyy} MOtoM2O.csh $yyyy dec22
 sbatch --job-name=reinit_dec27${yyyy} MOtoM2O.csh $yyyy dec27
 set foundit = 1
endif
 
if ( $foundit == 0 )then
 echo spelling mistake $mmm
endif
exit

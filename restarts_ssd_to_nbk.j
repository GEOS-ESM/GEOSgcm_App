#!/bin/csh -f
#SBATCH --job-name=SSD_to_NBK
##SBATCH --output=%x.o%j
#SBATCH --output=/dev/null
#SBATCH --ntasks=120 --ntasks-per-node=120
#SBATCH --partition=geosgms --constraint=mil --account=s1062
#SBATCH --time=24:00:00

setenv GEOSBIN /discover/nobackup/projects/gmao/osse2/GIT/GEOS-LM_v12-rc8/GEOSgcm/install-Aggressive-SLES15/bin

set RSTDIR = "/discover/nobackup/projects/gmao/osse2/HWT/CONUS02KM/Feature-c2160_L137/restarts"
set SSDDIR = "/discover/nobackup/projects/gmao/osse2/TSE_staging/HWT/Feature-c2160_L137/restarts/"

# current date
set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set hhc   = `cat cap_restart | cut -c10-11`
echo $nymdc
echo $nhmsc
echo $hhc
echo 

# previous date 12hr
set datep = `$GEOSBIN/tick $nymdc $nhmsc -43200`
set nymdp =  $datep[1]
set nhmsp =  $datep[2]
set hhp   = `echo $nhmsp | cut -c1-2`
echo $nymdp
echo $nhmsp
echo $hhp
echo 
set rsts = `ls -d $SSDDIR/*${nymdp}_${hhp}*`
@ start_time = `date +%s`
foreach rst ($rsts)
  set file = `basename $rst`
  if (! -e $RSTDIR/$file) /bin/cp $rst $RSTDIR/$file &
end
wait
foreach rst ($rsts)
  /bin/rm $rst
end


# previous date 24hr
set datep = `$GEOSBIN/tick $nymdc $nhmsc -86400`
set nymdp =  $datep[1]
set nhmsp =  $datep[2]
set hhp   = `echo $nhmsp | cut -c1-2`
echo $nymdp
echo $nhmsp
echo $hhp
echo
set rsts = `ls -d $SSDDIR/*${nymdp}_${hhp}*`
@ start_time = `date +%s`
foreach rst ($rsts)
  set file = `basename $rst`
  if (! -e $RSTDIR/$file) /bin/cp $rst $RSTDIR/$file &
end
wait
foreach rst ($rsts)
  /bin/rm $rst
end

@ end_time = `date +%s`
@ diff = $end_time - $start_time
echo "It took $diff seconds"
exit 0


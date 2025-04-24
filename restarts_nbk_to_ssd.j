#!/bin/csh -f
#SBATCH --job-name=NBK_to_SSD
##SBATCH --output=%x.o%j
##SBATCH --output=/dev/null
#SBATCH --ntasks=120 --ntasks-per-node=120
#SBATCH --partition=geosgms --constraint=mil --account=s1062
#SBATCH --parsable --dependency=afterok:44841706
#SBATCH --time=24:00:00

set RSTDIR = "/discover/nobackup/projects/gmao/osse2/HWT/CONUS02KM/Feature-c2160_L137/restarts"
set SSDDIR = "/discover/nobackup/projects/gmao/osse2/TSE_staging/HWT/Feature-c2160_L137/restarts/"

set nymdc = 20240515 #`cat cap_restart | cut -c1-8`
set nhmsc = 21       #`cat cap_restart | cut -c10-11`

set rsts = `ls -d $RSTDIR/*${nymdc}_${nhmsc}*`
@ start_time = `date +%s`
foreach rst ($rsts)
  set file = `basename $rst`
  if (! -e $SSDDIR/$file) /bin/cp $rst $SSDDIR/$file &
end
wait
@ end_time = `date +%s`
@ diff = $end_time - $start_time
echo "It took $diff seconds"
exit 0


#!/usr/bin/ksh 
#-xv
#-----------------------------------------------

year=$1

mkdir -p /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/$year

mkdir -p /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/$year/aice/
mkdir -p /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/$year/hice/

chmod 755  -R /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/$year


remainder=$(($year%4))
if [ $remainder -eq 0 ]; then
set -A DAYS 31 29 31 30 31 30 31 31 30 31 30 31
set -A DAY_INTERVAL 30 28 30 29 30 29 30 30 29 30 29 30
else
set -A DAYS 31 28 31 30 31 30 31 31 30 31 30 31
set -A DAY_INTERVAL 30 27 30 29 30 29 30 30 29 30 29 30
fi
mon=1
while [ ${mon} -le 12 ] ; do
day=1 
nn=$((mon-1))
##python interpo_aice_hice.py $year $mon $day ${DAY_INTERVAL[${nn}]} &
#python onlyhice_interpo.py $year $mon $day ${DAY_INTERVAL[${nn}]} &
python nomissing_onlyaice_interpo.py $year $mon $day ${DAY_INTERVAL[${nn}]} &
mon=$((mon+1))
done
chmod 755 -R /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT
echo "finished year:" $year


#!/bin/csh 

# Set up ODAS for a new experiment after running Yuri's gcm_setup

#set EXPID = 'rk036'
set EXPID  = $1

# Diag Table
####################################################################
set line1 = 'set dsets="ocean_month"'
set line2 = 'set dsets="ocean_month ocean_daily"'
sed "s/${line1}/${line2}/g" gcm_run.j > tmp
mv tmp gcm_run.j

# Change the executable
####################################################################
set line2 = '$EXPDIR/ocean_das/UMD_Etc/scripts/oda_run.j $NX $NY'
grep -n '$RUN_CMD $NPES ./GEOSgcm.x' gcm_run.j | cut -d : -f 1 >tmp
set line = `cat tmp`
sed    "${line}i $line2" gcm_run.j > tmp
mv tmp gcm_run.j
grep -n '$RUN_CMD $NPES ./GEOSgcm.x' gcm_run.j | cut -d : -f 1 >tmp
set line = `cat tmp`
sed -e "${line}d" gcm_run.j > tmp
mv tmp gcm_run.j

####################################################################
# Add tile files
set line1 = 'Pfafstetter.til tile_hist.data'
set line2 = '/bin/ln -s /discover/nobackup/yvikhlia/coupled/Forcings/a360x181_o720x410/DC0360xPC0181_TM0720xTM0410-Pfafstetter.til tile_hist_360x180.data'
sed "/${line1}/a ${line2}" gcm_run.j > tmp
mv tmp gcm_run.j
set line2 = '/bin/ln -s /discover/nobackup/yvikhlia/coupled/Forcings/a720x361_o720x410/DC0720xPC0361_TM0720xTM0410-Pfafstetter.til tile_hist_720x361.data'
sed "/${line1}/a ${line2}" gcm_run.j > tmp
mv tmp gcm_run.j
set line2 = '/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP'
sed "/${line1}/a ${line2}" gcm_run.j > tmp
mv tmp gcm_run.j

set line1 = 'binarytile.x tile_hist.data tile_hist.bin'
set line2 = '$GEOSBIN/binarytile.x tile_hist_360x180.data tile_hist_360x180.bin'
sed "/${line1}/a ${line2}" gcm_run.j > tmp
mv tmp gcm_run.j
set line2 = '$GEOSBIN/binarytile.x tile_hist_720x361.data tile_hist_720x361.bin'
sed "/${line1}/a ${line2}" gcm_run.j > tmp
mv tmp gcm_run.j

####################################################################
# Fix Batch Parameters
set line1 = '#PBS -l walltime=12:00:00'
set line2 = '#SBATCH --time=12:00:00'
sed "s/${line1}/${line2}/g" gcm_run.j > tmp
mv tmp gcm_run.j

sed "s/ntasks=300/ntasks=1200/g" gcm_run.j > tmp
mv tmp gcm_run.j

set line1 = '#PBS -N '$EXPID'_RUN'
set line2 = '#SBATCH --ntasks-per-node=24'
sed "s/${line1}/${line2}/g" gcm_run.j > tmp
mv tmp gcm_run.j

set line1 = '#SBATCH --constraint=hasw'
set line2 = '#SBATCH --job-name='$EXPID
sed "s/${line1}/${line2}/g" gcm_run.j > tmp
mv tmp gcm_run.j

sed '/#SBATCH -A g0609/a #SBATCH --qos=ocndev'  gcm_run.j > tmp
mv tmp gcm_run.j

set line1 = '#SBATCH -o '${EXPID}'.o%j'
sed "/#SBATCH -A g0609/a ${line1}" gcm_run.j > tmp
mv tmp gcm_run.j

set line1 = '#SBATCH -o '${EXPID}'.e%j'
sed "/#SBATCH -A g0609/a ${line1}" gcm_run.j > tmp
mv tmp gcm_run.j
####################################################################


exit  0


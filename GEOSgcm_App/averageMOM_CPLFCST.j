#!/bin/csh -fx

#SBATCH -A g0609
#SBATCH -N 1 -n 1
#SBATCH -t 01:00:00
#SBATCH --job-name=@RUN_M
#SBATCH --partition=compute
#SBATCH --qos=gmaofcst

source /usr/share/modules/init/csh
module purge
module load other/nco-4.5.5-gcc-5.3-sp3

# Make monthly files out of daily diag files

setenv    EXPDIR  @EXPDIR
setenv    HOMDIR  @HOMDIR
cd $EXPDIR

cp -p $GEOSS2S/util/proc_diag_fcst.csh ./
set capdateIC = `cat $HOMDIR/cap_restartIC | cut -c1-8`
set yyyyIC = `cat $HOMDIR/cap_restartIC | cut -c1-4`
set yyyyCR = `cat cap_restart | cut -c1-4`
set mmIC = `cat $HOMDIR/cap_restartIC | cut -c5-6`
set mmCR = `cat cap_restart | cut -c5-6`
if ( $mmIC == $mmCR ) exit

set yyyy = $yyyyCR
@ mm = $mmCR - 1
if ( $mm == 0 ) @ yyyy = $yyyy - 1
if ( $mm == 0 ) @ mm = 12
if ( $mmIC == $mm ) exit
if ( $mmIC == 0$mm ) exit

set ms = $mm
if ( $mm < 10 ) set ms = 0$mm

if ( ! -e  MOM_Output ) exit
if ( -e  MOM_Output/YESMOM.${yyyy}${ms} ) exit
if ( -e  MOM_Output/NOTMOM.${yyyy}${ms} ) /bin/rm -f MOM_Output/NOTMOM.${yyyy}${ms}
proc_diag_fcst.csh $yyyy $ms
if ( -e ocean_monthly.${yyyy}${ms}.nc ) then
    mv ocean_monthly.${yyyy}${ms}.nc MOM_Output
    rm -f MOM_Output/ocean_daily.e${yyyy}${ms}*.nc
    touch MOM_Output/YESMOM.${yyyy}${ms}
else
    touch MOM_Output/NOTMOM.${yyyy}${ms}
endif


#/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/S2S-2_1_ANA_001

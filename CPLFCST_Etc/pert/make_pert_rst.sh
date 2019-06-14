##########################################################################
#  Driver for generating interpolated restarts 
#  from 5 coupled restarts dated 5 day apart.
#  Executed after the setup: it uses setup restarts in perturbation procedure
#  Prerequisite:
#   existence of RDIR set up 
#   restarts availability at forecast and 5-day previous time
##########################################################################

module purge
module load comp/intel-11.0.083
module load mpi/impi-4.1.2.040
module load lib/mkl-10.0.3.020
module list

# Pass the center (forecast) date ex: cymd =20100131
set cymd = $1

# Pass the center (forecast) ensemble member
set ensn = $2

# Extract the month and day from the date passed as YYYYMMDD, which be a central one
set cyear = `echo $cymd  | cut -c1-4`
set cmont = `echo $cymd  | cut -c5-6`
set cdday = `echo $cymd  | cut -c7-8`
@ cyearm1 = $cyear - 1
@ cdate = $cymd - 1

if ( $cymd == ${cyear}'0131' ) set bdate = ${cyear}'0125'
if ( $cymd == ${cyear}'0225' ) set bdate = ${cyear}'0219'
if ( $cymd == ${cyear}'0327' ) set bdate = ${cyear}'0321'
if ( $cymd == ${cyear}'0426' ) set bdate = ${cyear}'0420'
if ( $cymd == ${cyear}'0531' ) set bdate = ${cyear}'0525'
if ( $cymd == ${cyear}'0630' ) set bdate = ${cyear}'0624'
if ( $cymd == ${cyear}'0730' ) set bdate = ${cyear}'0724'
if ( $cymd == ${cyear}'0829' ) set bdate = ${cyear}'0823'
if ( $cymd == ${cyear}'0928' ) set bdate = ${cyear}'0922'
if ( $cymd == ${cyear}'1028' ) set bdate = ${cyear}'1022'
if ( $cymd == ${cyear}'1127' ) set bdate = ${cyear}'1121'
if ( $cymd == ${cyear}'1227' ) set bdate = ${cyear}'1221'

echo DATES BEFORE AND AFTER: $bdate $cdate

if ($cmont == '01') set EXPID='jan'$cdday
if ($cmont == '02') set EXPID='feb'$cdday
if ($cmont == '03') set EXPID='mar'$cdday
if ($cmont == '04') set EXPID='apr'$cdday
if ($cmont == '05') set EXPID='may'$cdday
if ($cmont == '06') set EXPID='jun'$cdday
if ($cmont == '07') set EXPID='jul'$cdday
if ($cmont == '08') set EXPID='aug'$cdday
if ($cmont == '09') set EXPID='sep'$cdday
if ($cmont == '10') set EXPID='oct'$cdday
if ($cmont == '11') set EXPID='nov'$cdday
if ($cmont == '12') set EXPID='dec'$cdday

# Set expr directory of run 
setenv RDIR $GEOSS2S/runx/$cyear/$EXPID/ens${ensn}
setenv XDIR $GEOSS2S/runx/$cyear/$EXPID/OutData

# Set perturb  directory in the driver directory
setenv XBVDIR $XDIR/perts
mkdir -p $XBVDIR

# Restarts that the perturbations will be centered at
setenv RSTDIR_ASSIM $XBVDIR/cdata
# Positively perturbed 
setenv RSTDIR_PBV $XBVDIR/pdata 
# Negatively perturbed 
setenv RSTDIR_NBV $XBVDIR/ndata 

# Create the PT directories
if ( -e $XBVDIR/PANICSTOP_PT ) /bin/rm -f $XBVDIR/PANICSTOP_PT
if ( -e $XBVDIR/cdata ) /bin/rm -rf $XBVDIR/?data
mkdir -p $XBVDIR/cdata
mkdir -p $XBVDIR/ndata
mkdir -p $XBVDIR/pdata

# Get all restarts needed to cdata,ndata,pdata (ndata&pdata will be updated)
set odasRUNS = "/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/hindcast_restarts"
#set odasRUNS = "/gpfsm/dnb04/projects/p71/aogcm/g5fcst/forecast/production/geos-s2s/data"
echo SAVE CURRENT ANALYSIS RESTARTS in cdata
cd ${RSTDIR_ASSIM}
ln -sf ${RDIR}/RESTART/ocean_temp_salt.res.nc aocean_temp_salt.res.nc
ln -sf ${RDIR}/RESTART/ocean_velocity.res.nc aocean_velocity.res.nc
ln -sf ${RDIR}/RESTART/ocean_sbc.res.nc aocean_sbc.res.nc
ln -sf ${RDIR}/fvcore_internal_rst afvcore_internal_rst
ln -sf ${RDIR}/moist_internal_rst amoist_internal_rst
ln -sf ${RDIR}/saltwater_internal_rst asaltwater_internal_rst
ln -sf ${RDIR}/seaice_internal_rst aseaice_internal_rst

echo SAVE PREVIOUS FORECAST RESTARTS in ndata
cd ${RSTDIR_NBV}
ln -sf  ${odasRUNS}/RESTART/${bdate}_2100z.ocean_temp_salt.res.nc uocean_temp_salt.res.nc
ln -sf  ${odasRUNS}/RESTART/${bdate}_2100z.ocean_velocity.res.nc uocean_velocity.res.nc
ln -sf  ${odasRUNS}/RESTART/${bdate}_2100z.ocean_sbc.res.nc uocean_sbc.res.nc
ln -sf  ${odasRUNS}/fvcore_internal_checkpoint.${bdate}_2100z.bin ufvcore_internal_rst
ln -sf  ${odasRUNS}/moist_internal_checkpoint.${bdate}_2100z.bin umoist_internal_rst
ln -sf  ${odasRUNS}/saltwater_internal_checkpoint.${bdate}_2100z.bin usaltwater_internal_rst
ln -sf  ${odasRUNS}/seaice_internal_checkpoint.${bdate}_2100z.bin useaice_internal_rst

echo SAVE CURRENT ANALYSIS RESTARTS in pdata
cd ${RSTDIR_PBV}
ln -sf  ${odasRUNS}/RESTART/${cdate}_2100z.ocean_temp_salt.res.nc uocean_temp_salt.res.nc
ln -sf  ${odasRUNS}/RESTART/${cdate}_2100z.ocean_velocity.res.nc uocean_velocity.res.nc
ln -sf  ${odasRUNS}/RESTART/${cdate}_2100z.ocean_sbc.res.nc uocean_sbc.res.nc
ln -sf  ${odasRUNS}/fvcore_internal_checkpoint.${cdate}_2100z.bin ufvcore_internal_rst
ln -sf  ${odasRUNS}/moist_internal_checkpoint.${cdate}_2100z.bin umoist_internal_rst
ln -sf  ${odasRUNS}/saltwater_internal_checkpoint.${cdate}_2100z.bin usaltwater_internal_rst
ln -sf  ${odasRUNS}/seaice_internal_checkpoint.${cdate}_2100z.bin useaice_internal_rst

cd $XBVDIR

echo SAVE CURRENT ANALYSIS RESTARTS in ndata as templates
/bin/cp ${odasRUNS}/RESTART/${cdate}_2100z.ocean_temp_salt.res.nc ${RSTDIR_NBV}/ocean_temp_salt.res.nc
/bin/cp ${odasRUNS}/RESTART/${cdate}_2100z.ocean_velocity.res.nc ${RSTDIR_NBV}/ocean_velocity.res.nc
/bin/cp ${odasRUNS}/RESTART/${cdate}_2100z.ocean_sbc.res.nc ${RSTDIR_NBV}/ocean_sbc.res.nc
/bin/cp ${odasRUNS}/fvcore_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_NBV}/fvcore_internal_rst
/bin/cp ${odasRUNS}/moist_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_NBV}/moist_internal_rst
/bin/cp ${odasRUNS}/saltwater_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_NBV}/saltwater_internal_rst
/bin/cp ${odasRUNS}/seaice_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_NBV}/seaice_internal_rst

echo SAVE CURRENT ANALYSIS RESTARTS in pdata as templates
/bin/cp ${odasRUNS}/RESTART/${cdate}_2100z.ocean_temp_salt.res.nc ${RSTDIR_PBV}/ocean_temp_salt.res.nc
/bin/cp ${odasRUNS}/RESTART/${cdate}_2100z.ocean_velocity.res.nc ${RSTDIR_PBV}/ocean_velocity.res.nc
/bin/cp ${odasRUNS}/RESTART/${cdate}_2100z.ocean_sbc.res.nc ${RSTDIR_PBV}/ocean_sbc.res.nc
/bin/cp ${odasRUNS}/fvcore_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_PBV}/fvcore_internal_rst
/bin/cp ${odasRUNS}/moist_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_PBV}/moist_internal_rst
/bin/cp ${odasRUNS}/saltwater_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_PBV}/saltwater_internal_rst
/bin/cp ${odasRUNS}/seaice_internal_checkpoint.${cdate}_2100z.bin ${RSTDIR_PBV}/seaice_internal_rst

# norm and rescaling 
#
# varname : Variable - Should be matched  
# avgz : lower bound of vertical levels for norm calculation
# slon : start longitude for norm calculation
# elon : end longitude
# slat : start latitude
# elat : end latitude
# longvar : natural variance
# pct : percentage to reduce the norm

cat << _EOF_ > bv.nml
 &dimension_nml
  nx = 720, ny = 410, nz = 40 /

 &variable_nml
  varname = 'temp' /

 &region_nml
  avgz = 10,
  slon =-240, elon =-90, slat = -10, elat = 10 /

 &recl_nml
  longvar = 0.48, pct = 0.10 /
_EOF_

# Rescale restarts in RSTDIR_NBV and RSTDIR_PBV
echo RESCALE RESTARTS IN ndata and pdata
if ( -e  ${XBVDIR}/perturb.out) then
 /bin/rm ${XBVDIR}/perturb.out
endif
ln -sf /home/yvikhlia/nobackup/coupled/Forcings/a288x181_o720x410/INPUT/dave/grid_spec.nc grid_spec.nc
ln -sf  ${odasRUNS}/RESTART/${bdate}_2100z.ocean_temp_salt.res.nc exp1.nc
ln -sf  ${odasRUNS}/RESTART/${cdate}_2100z.ocean_temp_salt.res.nc exp2.nc
$GEOSS2S/pert/cal_norm4rst_savebv.exe
echo PERTURB RESTARTS IN ndata and pdata
$GEOSS2S/pert/perturb_aogcm.exe > $XBVDIR/perturb.out
set pertstat = $status
if ( $pertstat != 0 ) then
     touch $XBVDIR/PANICSTOP_PT
     exit
endif
if ( ! -e $XBVDIR/perturb.out ) then
     touch $XBVDIR/PANICSTOP_PT
     exit
endif
set nnn = `grep ErrVAR perturb.out | wc -l`
if ( $nnn > 0 ) then
     echo ERROR - ErrVAR RANGE ERROR
     touch $XBVDIR/PANICSTOP_PT
     exit
endif

# Clean after the perturbation
if ( ! -e $XBVDIR/PANICSTOP_PT ) then
   /bin/rm -f $XBVDIR/exp?.nc
   /bin/rm -f $XBVDIR/grid_spec.nc
   /bin/rm -rf $XBVDIR/cdata
   /bin/rm -f $XBVDIR/?data/u*
   /bin/mv $XBVDIR/coef.dat $XBVDIR/coef.dat$cdate
   /bin/mv $XBVDIR/perturb.out $XBVDIR/perturb.out$cdate
   touch $XBVDIR/DONEPERT
   echo PERTURBATION COMPLETED
endif

exit

#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#@RUN_ST
#@RUN_FP
#@BATCH_NAME @RUN_FN
#@RUN_Q
#@BATCH_GROUP
#@BATCH_TYPE -o STATOUT
#@BATCH_JOIN

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

@SETENVS

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSBIN          @GEOSBIN
setenv GEOSUTIL         @GEOSUTIL

setenv MODINIT @MODINIT
setenv SITEMODULES modules.${SITE}

if ($?MODINIT) then
if ( -e $GEOSBIN/$SITEMODULES) then
  source $MODINIT
  module purge
  module load $GEOSBIN/$SITEMODULES
else if ( -e $GEOSBIN/modules) then
  source $MODINIT
  module purge
  module load $GEOSBIN/modules
endif
endif

setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    EXPID   @EXPID
setenv    EXPDIR  @EXPDIR
setenv    HOMDIR  @HOMDIR

#######################################################################
#                  Set Forecast Run Parameters
#######################################################################

set ANA = NCEP

set nymdb = 20150129
set nymde = 20150129
set nymd  = $nymdb

set yyyymm = `echo $nymdb | cut -b1-6`

set       statsdir = $EXPDIR/forecasts/G5${ANA}.stats.$yyyymm
mkdir -p $statsdir
cd       $statsdir

#######################################################################
#                               Create STATS
#######################################################################

while( $nymd <= $nymde )

set date0  =  $nymd
set time0  =  000000
set yyyymm = `echo $nymd | cut -b1-6`

set year0      = `echo $nymd | cut -b1-4`
set month0     = `echo $nymd | cut -b5-6`
set fcst_files = `/bin/ls -1 $EXPDIR/forecasts/${nymd}_00z/G5${ANA}*geosgcm_fcst*nc4`

set anal_files = ''
@ n = 1
while ($n <= 6)

set year  = `echo $nymd | cut -b1-4`
set month = `echo $nymd | cut -b5-6`


# G5REPLAY Verification
# ---------------------
set next = `/bin/ls -1 $EXPDIR/holding/geosgcm_prog/${year}*/$EXPID.geosgcm_prog.${nymd}*nc4`

set anal_files = `echo $anal_files $next`
set date = `$GEOSUTIL/post/tick $nymd 0 86400`
set nymd = $date[1]
@ n = $n + 1
end

$GEOSUTIL/post/stats.x -fcst $fcst_files \
                       -ana  $anal_files \
                       -cli $SHARE/gmao_ops/verification/stats/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_00z.576x361.data.nc4 \
                            $SHARE/gmao_ops/verification/stats/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_06z.576x361.data.nc4 \
                            $SHARE/gmao_ops/verification/stats/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_12z.576x361.data.nc4 \
                            $SHARE/gmao_ops/verification/stats/MERRA-2.inst3_3d_asm_Np.198501_201412.clim_18z.576x361.data.nc4 \
                       -tag $EXPID -nfreq 060000 -rc $GEOSUTIL/post/stats.rc &

set date = `$GEOSUTIL/post/tick $date0 $time0 86400`
set nymd = $date[1]

end

wait

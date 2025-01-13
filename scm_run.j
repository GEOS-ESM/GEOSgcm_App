#!/bin/tcsh -f
#@BATCH_TIME
#@BATCH_TASKS
#@BATCH_JOBNAME
#@BATCH_GROUP

umask 022
limit stacksize unlimited

setenv ARCH `uname`
setenv SITE             @SITE
setenv GEOSDIR          @INSTALLDIR
setenv GEOSBIN          @INSTALLDIR/bin
setenv GEOSETC          @INSTALLDIR/etc
setenv GEOSUTIL         @INSTALLDIR

source $GEOSBIN/g5_modules
setenv @LD_LIBRARY_PATH_CMD ${LD_LIBRARY_PATH}:${GEOSDIR}/lib
# We only add BASEDIR to the @LD_LIBRARY_PATH_CMD if BASEDIR is defined (i.e., not running with Spack)
if ( $?BASEDIR ) then
    setenv @LD_LIBRARY_PATH_CMD ${@LD_LIBRARY_PATH_CMD}:${BASEDIR}/${ARCH}/lib
endif

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

setenv GCMVER `cat $GEOSETC/.AGCM_VERSION`
echo   VERSION: $GCMVER

setenv EXPDIR  @EXPDIR

cd $EXPDIR

$GEOSBIN/construct_extdata_yaml_list.py GEOS_ChemGridComp.rc

echo "file_weights: true" >> extdata.yaml

cp fvcore_layout.rc input.nml

$RUN_CMD 1 ./GEOSgcm.x --logging_config 'logging.yaml'

#!/bin/csh 

# Set up ODAS for a new experiment after running Yuri's gcm_setup
#set EXPID       = $1	# S2S-2_1_ANA_001
#set EXP_DIR     = $2   # /gpfsm/dnb42/projects/p17/rkovach/geos5/exp/
#set SANDBOX     = $2	# yuri-S2S-2_1_UNSTABLE
#set SANDBOX_DIR = $3	# /gpfsm/dnb42/projects/p17/rkovach/geos5/sandbox/

echo "Enter Experiment Name (EXPID)"
set EXPID = $<

echo "Enter Path to Experiment (/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/)"
set EXP_DIR = $<

echo "Enter Sandbox Name (yuri-S2S-2_1_UNSTABLE)"
set SANDBOX = $<

echo "Enter Path to Sandbox (/gpfsm/dnb42/projects/p17/rkovach/geos5/sandbox/)"
set SANDBOX_DIR = $<

set SANDBOX_PATH = ${SANDBOX_DIR}${SANDBOX}/GEOSodas

# Copy Files from UMD_rc
set EXPPATH = ${EXP_DIR}/${EXPID}
if (! -d ${EXPPATH} ) then
        mkdir ${EXPPATH}
endif

cd ${EXPPATH}
if (! -d ocean_das ) then
        mkdir ocean_das
endif
cp -r ${SANDBOX_DIR}/${SANDBOX}/GEOSodas/src/Applications/UMD_Etc ocean_das
cp ocean_das/UMD_Etc/UMD_scripts/ocean_das_config . 
cp ocean_das/UMD_Etc/UMD_rc/HISTORY.rc .
cp ocean_das/UMD_Etc/UMD_rc/diag_table .
cp ocean_das/UMD_Etc/UMD_rc/input.nml .
cp ocean_das/UMD_Etc/UMD_rc/AGCM.rc .
cp ocean_das/UMD_Etc/UMD_rc/CAP.rc .


sed "s/EXPNAME/$EXPID/g" HISTORY.rc > tmp
mv tmp HISTORY.rc
sed "s/EXPERIMENT_NAME/${EXPID}/g" ocean_das_config > tmp1
sed "s:EXPERIMENT_PATH:${EXPPATH}:g" tmp1 > tmp2
sed "s:SANDBOX_DIR:${SANDBOX_PATH}:g" tmp2 > tmp3
mv tmp3 ocean_das_config 

#####################################################################
# To make all changes to gcm_run.j
fix_gcm_run.csh $EXPID

#####################################################################

rm tmp*

exit  0


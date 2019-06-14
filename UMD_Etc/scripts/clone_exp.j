#!/bin/bash
#
#  Argument: EXPTEMPLATE = yuri or ocean_das experiment (from G, R , E or A)
#

EXPTEMPLATE=$1

if [ $EXPTEMPLATE == 'yuri' ]; then
    expdir='expdir'
else
    expdir='.'
fi

echo ${expdir}


echo "Enter experiment name"
read EXPNAME
echo $EXPNAME

echo "Enter experiment to clone"
read EXP2CLONE
echo $EXP2CLONE

echo "Enter path to sandbox (path to Linux src)"
read SANDBOX
echo $SANDBOX

mkdir $EXPNAME
#AGCM
echo "Setting up AGCM ..."
cp $EXP2CLONE/*.rc ./$EXPNAME
cp $EXP2CLONE/*_rst ./$EXPNAME
cp -r $EXP2CLONE/RC ./$EXPNAME/
cp -r $EXP2CLONE/vegdyn.data ./$EXPNAME/

if [ $EXPTEMPLATE == 'yuri' ]; then
    cp $EXP2CLONE/expdir/*.rc ./$EXPNAME
    cp $EXP2CLONE/expdir/*_rst ./$EXPNAME
    cp -r $EXP2CLONE/expdir/RC ./$EXPNAME/
    cp -r $EXP2CLONE/expdir/vegdyn.data ./$EXPNAME/
fi

#OGCM
echo "Setting up OGCM ..."
cp -r $EXP2CLONE/*_table ./$EXPNAME/
cp -r $EXP2CLONE/input.nml ./$EXPNAME/
cp -r $EXP2CLONE/RESTART ./$EXPNAME/

if [ $EXPTEMPLATE == 'yuri' ]; then
    cp -r $EXP2CLONE/expdir/*_table ./$EXPNAME/
    cp -r $EXP2CLONE/expdir/input.nml ./$EXPNAME/
    cp -r $EXP2CLONE/expdir/RESTART ./$EXPNAME/
fi

#Edit scripts
cp $EXP2CLONE/gcm_run.j ./$EXPNAME/
emacs $EXPNAME/gcm_run.j -nw
emacs $EXPNAME/HISTORY.rc -nw

#GET GCM exec
cp $SANDBOX/Linux/bin/GEOSgcm.x ./$EXPNAME/
cp $EXP2CLONE/$expdir/cap_restart ./$EXPNAME/

#GET OCEAN DAS EXEC AND SCRIPTS
mkdir ./$EXPNAME/ocean_das
cp -r $SANDBOX/src/Applications/UMD_Etc ./$EXPNAME/ocean_das/
cp ./$EXPNAME/ocean_das/UMD_Etc/UMD_scripts/ocean_das_config ./$EXPNAME/
emacs $EXPNAME/ocean_das_config -nw
cp ./$EXPNAME/ocean_das/UMD_Etc/UMD_scripts/ocean_das.csh ./$EXPNAME/
cp ./$EXPNAME/ocean_das/UMD_Etc/UMD_scripts/oda_run.j ./$EXPNAME/
cp ./$EXPNAME/ocean_das/UMD_Etc/UMD_scripts/ocean_observer.py ./$EXPNAME/
cp ./$EXPNAME/ocean_das/UMD_Etc/UMD_scripts/ocean_observer.csh ./$EXPNAME/
cp ./$EXPNAME/ocean_das/UMD_Etc/UMD_scripts/ocean_recenter.csh ./$EXPNAME/






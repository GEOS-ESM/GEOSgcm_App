#!/bin/csh

source ocean_das_config
source $GEOSDIR/Linux/bin/g5_modules


foreach anadir (`ls -d ./ocean_das/oana-*/ocean_observer_*`)
    #echo $anadir
    cd $EXPDIR/$anadir
    set yyyy = `echo $anadir | cut -c44-47`
    set mm   = `echo $anadir | cut -c48-49`
    set dd   = `echo $anadir | cut -c50-51`
    set hh   = `echo $anadir | cut -c53-54`
    echo $yyyy$mm$dd

    python $UMD_LETKFUTILS/mean_ods.py $yyyy $mm $dd $hh omf
end

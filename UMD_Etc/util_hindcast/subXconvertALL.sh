###########################################################################################
#
# execute scripts in the order to make them ready for subX Forecast
#
#ODAS from FORTUNA archive
#    $ARCHIVE/GEOS5.0/seasonal/apr16/ens1/
#MERRA-2 REPLAY 
#    /gpfsm/dnb02/projects/p58/subx/agcm_replay/rp_${DDATE}_21z_m2noGC/restarts
#H54 FREE RUN to produce background saltwater_internal 
#    /gpfsm/dnb02/dvarier/subx_NOAA/aogcm_rs/cgcm_bkg_Yury/19910416
#SALT data
#    /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/seaice_aice_hice_*
#
#FCST ODAS /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/ODAS           1999-2015 OK
#FCST SALT /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/H54/OutData    1999-2015 OK
#FCST ATMS /gpfsm/dnb02/projects/p58/subx/agcm_replay/rp_${DDATE}_21z_m2noGC/restarts 1999-2015 NA
###########################################################################################
set ICDATE = 19990416

cd /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/util
pwd
convrstODAS.sh $ICDATE
sleep 1m
pwd
cd /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/util
convrstSALT.sh $ICDATE
sleep 1m
pwd
cd /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/util
convrstMERRA2fromReplay.sh $ICDATE

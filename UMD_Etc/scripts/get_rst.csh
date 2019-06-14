#!/bin/csh -v
#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

set yyyy = $1
set mm = $2
set dd = $3
set hh = $4

set yyyymmdd = ${yyyy}${mm}${dd} #20130104
set hh = ${hh} #03
set yyyymmdd_hh = ${yyyymmdd}_${hh}
set suffix = '.nc4' #'.bin'
#set prefix = 'h52c90o05_2'
set prefix = 'rk018'
set version = 'Heracles-5_4_p3'
#set resolution = 'a180x1080_o1440x1080_CF0180x6C_DE0360xPE0180'
set resolution = 'a90x540_o1440x1080_CF0090x6C_DE0360xPE0180'
set rstdir = 'restarts'

echo "${yyyymmdd} ${hh}0000" > cap_restart
cd ${rstdir}

tar xvf restarts.e${yyyymmdd_hh}z.tar
rename _rst.e${yyyymmdd_hh}z.${version}.${resolution}${suffix} _rst *_rst.e${yyyymmdd_hh}z.${version}.${resolution}${suffix}
cd ..

cp ./${rstdir}/${prefix}.catch_internal_rst catch_internal_rst
cp ./${rstdir}/${prefix}.fvcore_internal_rst fvcore_internal_rst
#cp ./${rstdir}/${prefix}.gocart_internal_rst.e${yyyymmdd_hh}z.${version}.a90x540_o720x410_CF0090x6C_DE0360xPE0180${suffix} gocart_internal_rst
cp ./${rstdir}/${prefix}.guest_import_rst guest_import_rst
cp ./${rstdir}/${prefix}.irrad_internal_rst irrad_internal_rst
cp ./${rstdir}/${prefix}.lake_internal_rst lake_internal_rst
cp ./${rstdir}/${prefix}.landice_internal_rst landice_internal_rst
cp ./${rstdir}/${prefix}.moist_import_rst moist_import_rst
cp ./${rstdir}/${prefix}.moist_internal_rst moist_internal_rst
cp ./${rstdir}/${prefix}.orad_import_rst orad_import_rst
cp ./${rstdir}/${prefix}.pchem_internal_rst pchem_internal_rst
cp ./${rstdir}/${prefix}.saltwater_import_rst saltwater_import_rst
cp ./${rstdir}/${prefix}.saltwater_internal_rst saltwater_internal_rst
cp ./${rstdir}/${prefix}.seaice_import_rst seaice_import_rst
cp ./${rstdir}/${prefix}.seaice_internal_rst seaice_internal_rst
cp ./${rstdir}/${prefix}.solar_internal_rst solar_internal_rst
cp ./${rstdir}/${prefix}.surf_import_rst surf_import_rst
cp ./${rstdir}/${prefix}.turb_import_rst turb_import_rst
cp ./${rstdir}/${prefix}.turb_internal_rst turb_internal_rst

mkdir RESTART
cp ./${rstdir}/RESTART.e${yyyymmdd_hh}z/ocean_*.nc ./RESTART/

#Get analysis
#cp /gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/GEOS-Yuri/exp_a1_o05/test-letkf3/gmao_letkf/wrkdir/oana-${yyyymmdd}_2/mean_ana_restart/ocean_temp_salt.res.nc ./RESTART/

echo ${yyyymmdd}' '${hh}'0000' > cap_restart


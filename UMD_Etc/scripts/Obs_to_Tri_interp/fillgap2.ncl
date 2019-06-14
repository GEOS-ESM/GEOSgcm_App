;**********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"


;**********************************************************
;* Here I compute fields to fill in for the 41-day gap in the ssmi record.
;* I first read in tripolar-interpolated merra-2 sea ice fields for all of
;* 1987 and 1988.
;* I then read in corresponding nsidc data from Zhao's directory. Zhao
;* had previously filled in the gap with uncorrected merra-2 fields,
;* so the number of fields for both merra-2 and from Zhao's directory
;* are the same. 
;* I then compute the bias averaged over 5 days prior to the gap,
;* and 5 days after the gap. I then fill the gap with merra-2 values
;* minus a bias correction. The correction consists of the two bias values
;* linearly interpolated over the gap. The computed ice is then constrained
;* to values between 0 and 1.
;* In the three loops below, I print the nh ice extent from 11/1/87
;* up to the gap,through the gap, and then from the end of the gap to 1/31/88.
;**********************************************************



begin

  g = 9.80665
  fili1 = systemfunc("ls merra2_seaice_????.nc")
  nc_infile1 = addfiles(fili1, "r")
  merra2 = nc_infile1[:]->merra2
  lat2d = nc_infile1[0]->LAT
  lon2d = nc_infile1[0]->LON

  fili2 = systemfunc("ls /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/1987/aice/seaice_aice_????????.nc /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/OUTPUT/1988/aice/seaice_aice_????????.nc")
  nc_infile2 = addfiles(fili2, "r")
  ssmi = nc_infile2[:]->AICE

  printVarSummary(merra2)
  printVarSummary(ssmi)


  ssmi_av1 = dim_avg_n_Wrap(ssmi(331:335,:,:), 0)
  merra_av1 = dim_avg_n_Wrap(merra2(331:335,:,:), 0)
  bias1 = merra_av1 - ssmi_av1
  bias1 = where(ismissing( ssmi(0,:,:)), bias1@_FillValue, bias1)

  ssmi_av2 = dim_avg_n_Wrap(ssmi(377:381,:,:), 0)
  merra_av2 = dim_avg_n_Wrap(merra2(377:381,:,:), 0)
  bias2 = merra_av2 - ssmi_av2
  bias2 = where(ismissing( ssmi(0,:,:)), bias2@_FillValue, bias2)


  nc_infile3 = addfile("/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/iodas_utils/data/grid_spec_720x410x40.nc", "r")
  area = nc_infile3->area_T
  tmask = nc_infile3->wet

  wks  = gsn_open_wks("ncgm","tmp2")  ; open a ncgm file
  gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")       ; choose colormap

  res1 = True
  res1@gsnMaximize = True
  res1@gsnSpreadColors     = True                ; use full colormap

  res1@mpDataBaseVersion = "MediumRes"
  res1@mpProjection = "Stereographic"
  res1@mpGeophysicalLineThicknessF = 2.
  res1@mpEllipticalBoundary = False
  res1@mpGridAndLimbOn = False
  res1@mpCenterLatF  = 90.
  res1@mpCenterLonF = -45.
  res1@mpLimitMode = "LatLon"
  res1@mpMinLatF = 55.
  res1@mpFillOn          = False                 ; color fill

  res1@cnFillOn          = True                  ; color fill
  res1@cnLinesOn         = False                 ; no contour lines
  res1@cnLineLabelsOn    = False
  res1@cnFillMode = "CellFill"
  res1@cnMissingValFillColor = "purple"
  res1@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
  res1@cnMinLevelValF   =  0.05             ; set min contour level
  res1@cnMaxLevelValF   =  0.95               ; set max contour level
  res1@cnLevelSpacingF  =  0.05          ; set contour spacing
  res1@cnLineLabelsOn = False
  res1@lbOrientation  = "Vertical"

  do k = 304, 335
    aice = ssmi(k,:,:)
    aice@lat2d = lat2d
    aice@lon2d = lon2d
    extent_nh = where(aice.gt.(0.15), area, 0.)
    extent_nh = where(tmask.gt.(0.5),extent_nh, 0.)
    extent_nh = where(lat2d.gt.(0.), extent_nh, 0.)
    extent_nh = where( ismissing(aice), 0., extent_nh)
    tot_extent_nh = sum(extent_nh) / 1000000000000.  ; in 10^6 km2
    print(k+" "+tot_extent_nh)
    plot = gsn_csm_contour_map(wks, aice, res1)
  end do

  year = 1987
  month = 12
  do k = 336, 376
    aice = merra2(k,:,:) - ( bias1 + (bias2 - bias1)*(k - 333.)/46. )
;   aice = merra2(k,:,:)
    aice = where(aice.gt.1., 1., aice)
    aice = where(aice.lt.0., 0., aice)
    aice = where(ismissing( ssmi(0,:,:)), aice@_FillValue, aice)

    day = k - 333
    if(day.gt.31) then
      day = day - 31
      month = 1
      year = 1988
    end if
    date = year * 10000 + month * 100 + day

    extent_nh = where(aice.gt.(0.15), area, 0.)
    extent_nh = where(tmask.gt.(0.5),extent_nh, 0.)
    extent_nh = where(lat2d.gt.(0.), extent_nh, 0.)
    extent_nh = where(ismissing(aice), 0., extent_nh)
    tot_extent_nh = sum(extent_nh) / 1000000000000.  ; in 10^6 km2
    print(date+" "+k+" "+tot_extent_nh)
    plot = gsn_csm_contour_map(wks, aice, res1)

    aice2 = new( (/1, 410, 720/), typeof(aice), aice@_FillValue)
    aice2(0,:,:) = aice
    delete_VarAtts(aice2, -1)

    aice2!0 = "Time"
    aice2!1 = "yaxis_1"
    aice2!2 = "xaxis_1"
    nc_outfile = addfile("seaice_aice_"+date+".nc", "c")
    nc_outfile->AICE=aice2

    delete(nc_outfile)
    delete(aice2)

    


  end do

  do k = 377, 395
    aice = ssmi(k,:,:)
    extent_nh = where(aice.gt.(0.15), area, 0.)
    extent_nh = where(tmask.gt.(0.5),extent_nh, 0.)
    extent_nh = where(lat2d.gt.(0.), extent_nh, 0.)
    extent_nh = where(ismissing(aice), 0., extent_nh)
    tot_extent_nh = sum(extent_nh) / 1000000000000.  ; in 10^6 km2
    print(k+" "+tot_extent_nh)
    plot = gsn_csm_contour_map(wks, aice, res1)
  end do




end


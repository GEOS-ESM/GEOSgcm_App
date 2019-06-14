;**********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"


;**********************************************************
;* This is code used to interpolate merra-2 sea ice to
;* the tripolar grid. It's all a bit hardwired.
;* I read the raw 1440x720 merra-2 binary file. The actual file location
;* is commented out, I copied the file to give it the bin suffix
;* for reading in ncl. Note the number of days in a year 
;* is hardwired.
;*
;* I then read a forecast file to get the lat2d and lon2d of
;* the tripolar grid.
;* Then the tripolar grid is looped through. For each lat2d and 
;* lon2d of the tripolar grid, I can compute ii and jj of the merra-2
;* lat/lon grid. It amounts to a nearest neighbor interpolation.
;* I then output the fields with a minimal number of netcdf
;* file and variable attributes.
;**********************************************************
 

begin

  g = 9.80665

; fili = "/discover/nobackup/projects/gmao/merra2/merra2/intermediate/d5124_m2_jan10/fvInput/g5gcm/bcs/SST/1440x720/dataoceanfile_MERRA2_ICE.1440x720.1987.data"
  fili = "tmp.1987.bin"


  in_aice = new( (/365, 720, 1440/), "float", -999.)

  do k = 0, 364
    recnum = (k + 1 )* 2 - 1
;   header = fbinrecread( fili, recnum - 1, (/14/), "float")
;   print(k+" "+header)
    print(recnum+" ")
    in_aice(k,:,:) = fbinrecread( fili, recnum, (/720, 1440/), "float")
;   chead = sprinti("%0.5i", floattoint(header))
  end do

; lat = fspan(-89.875,89.875,720)
; lat@units = "degrees north"
; lon = fspan(-179.875, 179.875, 1440)
; lon@units = "degrees east"
; in_aice!0 = "time"
; in_aice!1 = "lat"
; in_aice!2 = "lon"
; in_aice&lat = lat
; in_aice&lon = lon

  printVarSummary(in_aice)

  nc_infile2 = addfile("/discover/nobackup/projects/gmao/m2oasf/aogcm/g5fcst/forecast/production/READY2RM_Heracles-5.4p13/runx/2000/aug14/ens1/geosgcm_seaice/aug14.geosgcm_seaice.monthly.200009.nc4", "r")
  ay = nc_infile2->lat
  ax = nc_infile2->lon
  out_lat2d = nc_infile2->LAT
  out_lon2d = nc_infile2->LON
  tmp_aice = nc_infile2->AICE(0,:,:)
  out_area = nc_infile2->AREA(0,:,:)
  tmp_aice = where(tmp_aice.gt.2., tmp_aice@_FillValue, tmp_aice)

  out_lon2d = where(out_lon2d.lt.-180., out_lon2d + 360., out_lon2d)
  out_lon2d = where(out_lon2d.ge.180., out_lon2d - 360., out_lon2d)

  out_aice = new( (/365, 410, 720/), "float", tmp_aice@_FillValue)

  by = tofloat(ay)
  bx = tofloat(ax)

  do j = 0, 409
    do i = 0, 719
      jj = round( 4. * ( out_lat2d(j,i) + 89.875), 3)
      jj = min( (/719, jj/) )
      jj = max( (/0, jj/) )
      ii = round( 4. * ( out_lon2d(j,i) + 179.875), 3)
      ii = min( (/1439, ii/) )
      ii = max( (/0, ii/) )
;     print(jj+" "+ii)
      out_aice(:, j, i) = in_aice(:, jj, ii)
    end do
  end do

  printVarSummary(out_aice)
  printVarSummary(by)
  printVarSummary(bx)
  printVarSummary(out_lat2d)
  printVarSummary(out_lon2d)

  out_aice!0 = "time"
  out_aice!1 = "lat"
  out_aice!2 = "lon"
; out_aice&y = by
; out_aice&x = bx
  out_aice@long_name = "ice_concentration_of_grid_cell"
  out_aice@units = "1"

; out_lat2d!0 = "y"
; out_lat2d!1 = "x"
; out_lat2d&y = by
; out_lat2d&x = bx
; out_lon2d!0 = "y"
; out_lon2d!1 = "x"
; out_lon2d&y = by
; out_lon2d&x = bx


  dim_names = (/ "time", "y",  "x" /)
  dim_sizes = (/ 365, 410, 720 /)
  dimUnlim = (/ True, False , False  /)


; filedimdef(fout1, dim_names, dim_sizes, dimUnlim)

  delete_VarAtts(out_aice, -1)
  delete_VarAtts(out_lat2d, -1)
  delete_VarAtts(out_lon2d, -1)
  printVarSummary(out_aice)
  printVarSummary(out_lat2d)
  printVarSummary(out_lon2d)
  fout1 = addfile("merra2_seaice_1987.nc", "c")
  fout1->merra2=out_aice
  fout1->LAT=out_lat2d
  fout1->LON=out_lon2d



end


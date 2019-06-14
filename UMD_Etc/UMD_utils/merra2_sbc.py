#!/usr/bin/python

'''
main script

SA, Sep 2016
Guillaume Dec 2016
'''
#-----------------------------------------------------------------

import sys

import  pylab                as      pylab
import  numpy                as      np
import	matplotlib.pyplot    as	     plt
import matplotlib.cm         as      cm
from mpl_toolkits.basemap    import  Basemap

from	netCDF4		     import Dataset
from    datetime             import datetime, timedelta
from	mpl_toolkits.basemap import Basemap

from scipy import interpolate
import  os 
from	read_merra2_bcs	     import read_bin_data	# read a binary file, uses f2py, see file for compile instructions... change as you please!
#-----------------------------------------------------------------

def get_merra2_file_name(date_in):
	
	path_		= '/discover/nobackup/projects/gmao/share/dao_ops/fvInput/g5gcm/bcs/'
	file_pref	= 'dataoceanfile_MERRA2_'
	rslv		= '1440x720'		# 0/4 deg lat-lon resolution of boundary conditions

	sst_path_	= path_ + 'SST/' + rslv + '/'
	sic_path_	= path_ + 'SST/' + rslv + '/'

	# these are yearly files- notice the "%Y" in file name
	sst_file_	= sst_path_ + file_pref + 'SST' + '.' + rslv + '.' + date_in.strftime('%Y') + '.data'
	sic_file_	= sic_path_ + file_pref + 'ICE' + '.' + rslv + '.' + date_in.strftime('%Y') + '.data'

	print   'Reading SST from:\n[%s]'%(sst_file_)
	print '\nReading SIC from:\n[%s]'%(sic_file_)

	return sst_file_, sic_file_
#-----------------------------------------------------------------

def apply_mask(field_):

	ls_masked_field2_	   = np.copy(field_)

	fname='/gpfsm/dnb02/projects/p43/bzhao/oceanmask_merra2_1440x720.bin' 
	mask2=np.fromfile(fname, dtype='float32')
	mask2=np.reshape(mask2, (720, 1440))

	ls_masked_field2_[mask2==0]  = np.nan
	
	return ls_masked_field2_	

#-----------------------------------------------------------------

def plt2d(lon,lat,stuff, minval=-.1, maxval=.1, colmap=cm.spectral):


	levels = np.arange(minval, maxval+(maxval-minval)/25, (maxval-minval)/25)
	map = Basemap(projection='mill', llcrnrlon=20, llcrnrlat=-80, urcrnrlon=380, urcrnrlat=90, resolution='l')
	map.drawcoastlines()
	map.drawcountries()
	map.fillcontinents(color='coral')
	map.drawmapboundary()
	for shift in [0, 360]:
		x, y =map(lon+shift,lat)
		#x, y =map(lon,lat)
		ch=map.contourf(x,y,stuff, levels, origin='lower', extend='both',cmap=colmap)
    
	return ch
#-----------------------------------------------------------------

def write2file(lon,lat,sst_, mask_, fname):
	
	nx=sst_.shape[1]
	ny=sst_.shape[0]

	ncfile = Dataset(fname,'w') 
	ncfile.createDimension('lon',nx)
	ncfile.createDimension('lat',ny)

	sst = ncfile.createVariable('sst',np.dtype('float').char,('lat','lon'))
	sst.long_name='sst'
	sst.units='C'

	mask = ncfile.createVariable('mask',np.dtype('float').char,('lat','lon'))
	mask.long_name='mask'
	mask.units='1'
	
	sst[:] = sst_[:]-273.15
	mask[:] = mask_[:]

	ncfile.close()

#-----------------------------------------------------------------

def write2file_sic(lon,lat,sic_, mask_, fname):
	
	nx=sic_.shape[1]
	ny=sic_.shape[0]

	ncfile = Dataset(fname,'w') 
	ncfile.createDimension('lon',nx)
	ncfile.createDimension('lat',ny)

	#sic = ncfile.createVariable('sic',np.dtype('float').char,('lat','lon'))
	sic = ncfile.createVariable('AICE',np.dtype('float').char,('lat','lon'))
	#sic.long_name='sic'
	sic.long_name='AICE'
	sic.units='%'

	mask = ncfile.createVariable('mask',np.dtype('float').char,('lat','lon'))
	mask.long_name='mask'
	mask.units='1'
	
	sic[:] = sic_[:]
	mask[:] = mask_[:]

	ncfile.close()

def nearest_interp(lon, lat, z, LON, LAT, undef=np.nan):

    lon[lon>80.0]=lon[lon>80.0]-360.0
    I=np.where(np.isfinite(z))
    points=np.array( (lon[I].flatten(), lat[I].flatten()) ).swapaxes(0, 1)
    zout=interpolate.NearestNDInterpolator(points, z[I].flatten())(LON, LAT)
    
    return zout

class ModelGrid():
    def __init__(self, fname='grid_spec.nc'):
        ncfile = Dataset(fname, 'r')
        self.x = ncfile.variables['x_T'][:]
        self.y = ncfile.variables['y_T'][:]
        self.wet=np.transpose(ncfile.variables['wet'][:])
        ncfile.close()

def get_sst_sic(yy_, mm_, dd_):

	#----------------------------------------------------------------------------
        # processing date-- you would want to loop over yy_, mm_, dd_
        #yy_             = 2015          # year
        #mm_             = 10            # month    of above year
        #dd_             = 1             # day      of above month

	#yy_ = int(sys.argv[1])
	#mm_ = int(sys.argv[2])
	#dd_ = int(sys.argv[3])

	proc_date       = datetime(yy_, mm_, dd_, 0,0,0)
	#----------------------------------------------------------------------------
	
	# get names of MERRA-2 SST & SIC file names
	[sst_file_, sic_file_] = get_merra2_file_name(proc_date)

	# read the MERRA-2 SST & SIC. Both are of dimension(720,1440); see read_merra2_bcs.pyf
	#----------------------------------------------------------------------------
	[date_ofSST, nlon, nlat, lon, lat, sst_] = read_bin_data(sst_file_, proc_date.strftime('%Y%m%d'))	# SST is in K
	[date_ofSIC, nlon, nlat, lon, lat, sic_] = read_bin_data(sic_file_, proc_date.strftime('%Y%m%d'))	# SIC is a fraction (non-dimensional)
	lon, lat =np.meshgrid(lon, lat)

        if ( date_ofSST-np.int(proc_date.strftime('%Y%m%d'))):
		sys.exit('date error in reading SST; Input and Output dates do not match')

        if ( date_ofSIC-np.int(proc_date.strftime('%Y%m%d'))):
		sys.exit('date error in reading SIC; Input and Output dates do not match')
	#----------------------------------------------------------------------------

	# apply a land-sea mask to SST & SIC
	#----------------------------------------------------------------------------

	sic_ = apply_mask(sic_)

#       sic_[sic_>0.15] = np.nan

	ogrid=ModelGrid(fname='grid_spec.nc')
 
	sic_o = nearest_interp(lon, lat, sic_, ogrid.x, ogrid.y, undef=np.nan)
        
	mask2 = np.ones(np.shape(sic_o))
	mask2[np.isnan(sic_o)]=0.0
	mask2=mask2*np.transpose(ogrid.wet)

	#fname='sic_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
	fname='AICE_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
	write2file_sic(ogrid.x,ogrid.y,sic_o, mask2, fname)

	sst_ = apply_mask(sst_)

#       sst_[sic_>0.15] = np.nan

	ogrid=ModelGrid(fname='grid_spec.nc')

	sst_o = nearest_interp(lon, lat, sst_, ogrid.x, ogrid.y, undef=np.nan)

#       print sic_o

	mask = np.ones(np.shape(sst_o))
	mask[np.isnan(sst_o)]=0.0
	mask=mask*np.transpose(ogrid.wet)
       
	fname='sst_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
	write2file(ogrid.x,ogrid.y,sst_o, mask, fname)

#   debug write the original MERRA2 data
        mask3 = np.ones(np.shape(sst_))
        mask3[np.isnan(sst_)]=0.0
	fname='rawM2sst_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'.nc'
        write2file(lon,lat,sst_,mask3,fname)
	fname='rawM2sic_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'.nc'
        write2file_sic(lon,lat,sic_,mask3,fname)

	#----------------------------------------------------------------------------



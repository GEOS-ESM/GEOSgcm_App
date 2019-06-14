from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import array
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
import glob
import struct
import time
import sys
from mpl_toolkits.basemap import Basemap, shiftgrid, addcyclic
from scipy import interpolate
import getopt
import string
import datetime
import scipy.interpolate as interp
from scipy.spatial import cKDTree
import scipy.optimize as optm
import scipy.stats as scstats
import os.path
import math
from scipy.io import netcdf
import calendar
import datetime
sys.path.append('/home/bzhao/python_utils')
import read_utils
import plot_utils
import math_utils
import os.path

#### usage:
## 

##     python hice_dec2015jan2016_interpo.py 2015 12 15 16

#####
## this script is modified from /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/20160301_test/interpo_aice_hice.py
## major changes include: write out hice and seaice in different files
#####
##### 
## current version of this scripts are only good until year 2014
## for year 2016 has different input files for hice (heff)
## to run for 2016 onwards, gmask and gm need to be used (uncomment line 360 to 366)
#####
def write_state_hice(fname, HICE):

    nx=HICE.shape[1]
    ny=HICE.shape[0]
    nt=1

    ncfile = Dataset(fname,'w')
    ncfile.createDimension('xaxis_1',nx)
    ncfile.createDimension('yaxis_1',ny)
    ncfile.createDimension('Time',nt)

    hice = ncfile.createVariable('HICE',np.dtype('float32').char,('Time','yaxis_1','xaxis_1'))

    hice[:] = HICE
    ncfile.close()


def nearest_interp(lon, lat, z, LON, LAT, undef=np.nan):

    lon[lon>80.0]=lon[lon>80.0]-360.0
    points=np.array( (lon.flatten(), lat.flatten()) ).swapaxes(0, 1)

    zout=interpolate.NearestNDInterpolator(points, z.flatten())(LON, LAT)

    return zout

def nearest_interp_new(lon, lat, z, LON, LAT):
    lon[lon>80.0]=lon[lon>80.0]-360.0
    xs, ys, zs = lon_lat_to_cartesian(lon.flatten(), lat.flatten())
    xt, yt, zt = lon_lat_to_cartesian(LON.flatten(), LAT.flatten())
    tree = cKDTree(zip(xs, ys, zs))
    #find indices of the nearest neighbors in the flattened array
    #d, inds = tree.query(zip(xt, yt, zt), k = 1)
    #get interpolated 2d field
    #zout = z.flatten()[inds].reshape(LON.shape)

    d, inds = tree.query(zip(xt, yt, zt), k = 10)
    w = 1.0 / d**2
    zout = np.sum(w * z.flatten()[inds], axis=1) / np.sum(w, axis=1)
    zout.shape = LON.shape
    return zout

def lon_lat_to_cartesian(lon, lat, R = 1):
    """
    calculates lon, lat coordinates of a point on a sphere with
    radius R
    """
    lon_r = np.radians(lon)
    lat_r = np.radians(lat)

    x =  R * np.cos(lat_r) * np.cos(lon_r)
    y = R * np.cos(lat_r) * np.sin(lon_r)
    z = R * np.sin(lat_r)
    return x,y,z

def get_grid(): #reads lat lon for tripolar ocean grid 
    ##ncfile=Dataset('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/PLOT_ODAS/DATA/grid_spec_720x410x40.nc', "r")
    ncfile=Dataset('/discover/nobackup/yvikhlia/coupled/Forcings/Ganymed/a90x540_o720x410/INPUT/grid_spec.nc',"r")
    #ncfile=Dataset('/gpfsm/dnb42/projects/p17/bzhao/cp100/scratch/INPUT/grid_spec.nc',"r")
    LON     = ncfile.variables['x_T'][:]
    LAT     = ncfile.variables['y_T'][:]
    numlev     = ncfile.variables['num_levels'][:]
    ncfile.close()

    return LON, LAT, numlev

# Read in NSIDC

### 
##choose year, month, day for aice
yearstart=int(sys.argv[1])
yearend=int(sys.argv[1])
month=int(sys.argv[2])
daystart=int(sys.argv[3])
day_interval=int(sys.argv[4])

year=yearstart
year1=year
while (year <= yearend):
       pre_year=year-1
       next_year=year+1
       day=daystart
       
       startdays=datetime.datetime(year,month,day,0,0,0).timetuple().tm_yday
       enddays=startdays+day_interval
       leapday1=366
       leapday2=365
       preyear=[-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1]

       if (day <= 15):
	       kk1=month-2
	       kk2=month-1
       else:
	       kk1=month-1
	       kk2=month
	       print startdays

##linear interpo monthly to daily
       XP=[]
       mm=0
	       
       if (calendar.isleap(year)):
		calenderdays=np.arange(leapday1)
		hice_day=np.zeros((leapday1,276,360))
		nextyear=[367,368,369,370,371,372,373,374,375,376,377,378,379,380,381]
		preXP=[-16]
		nextXP=[381]
		caldayend=381
       else:
		calenderdays=np.arange(leapday2)
		hice_day=np.zeros((leapday2,276,360))
		nextyear=[366,367,368,369,370,371,372,373,374,375,376,377,378,379,380]
		preXP=[-16]
		nextXP=[380]
		caldayend=380
       calenderdays_extend=np.concatenate((preyear,calenderdays,nextyear))
       while (mm <= 11):
               nn=mm+1
               dum=datetime.datetime(year,nn,15,0,0,0).timetuple().tm_yday
               XP.append(dum)
               mm=mm+1
       XP_extend=np.concatenate((preXP,XP,nextXP))
       	# read in hice
       gmask=np.genfromtxt('/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/io.dat_360_276.output',dtype='int32',delimiter=2)
       ncfile=netcdf.netcdf_file('/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/giomass_grid.nc','r')
       dum=ncfile.variables['LON'][:]
       lon2=np.zeros(np.shape(dum))
       lon2[:]=dum
       lon2 = np.transpose(lon2)
       dum=ncfile.variables['LAT'][:]
       lat2=np.zeros(np.shape(dum))
       lat2[:]=dum
       lat2 = np.transpose(lat2)
       ncfile.close()

       filein2pre='/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/GIOMAS_HICE.201512.nc'
       filein2next='/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/GIOMAS_HICE.201601.nc'
       ncfilepre=netcdf.netcdf_file(filein2pre,'r')
       dum=ncfilepre.variables['HICE'][:]
       hice_p_pre=np.zeros(np.shape(dum))
       hice_p_pre[:]=dum
       ncfilenext=netcdf.netcdf_file(filein2next,'r')
       dum=ncfilenext.variables['HICE'][:]
       hice_p_next=np.zeros(np.shape(dum))
       hice_p_next[:]=dum
       ncfilepre.close()
       ncfilenext.close()

       hice_p_2mon=np.concatenate((hice_p_pre,hice_p_next)) 
       hice_p_2mon=np.ma.masked_where(hice_p_2mon>9999, hice_p_2mon)
       hice_p_extend=hice_p_2mon[:,:,:]
       print 'hice_p_2mon shape ', hice_p_2mon.shape
 
       ii=0
       while (ii <= 275):
               jj=0
               while (jj <= 359):
                       #dum=np.interp(calenderdays_extend[15:caldayend],XP_extend[:],hice_p_extend[:,ii,jj])
                       dum=np.interp(calenderdays_extend[XP_extend[12]:caldayend],XP_extend[12:13],hice_p_extend[:,ii,jj])
                       hice_day[:,ii,jj]=dum
                       jj=jj+1
               ii=ii+1
       
       ###### 
       startdaysforhice=startdays-1
       enddaysforhice=enddays-1
       days=startdaysforhice
       print 'days: ', days
       while (days <= enddaysforhice):
         yyyymmdd=year*10000+month*100+day
         print yyyymmdd
	         
	 fileout_hice='seaice_hice_%d.nc' % (year,yyyymmdd)
         
	 ######get tripolar LON LAT
 	 LON, LAT, numlevels = get_grid()
         hice_temp = hice_day[days,:,:].flatten()
         hice_in = hice_temp[hice_temp<1000]
         lon_in = lon2.flatten()[hice_temp<1000]
         lat_in = lat2.flatten()[hice_temp<1000]
         hice_out = nearest_interp(lon_in, lat_in, hice_in, LON, LAT, undef=np.nan)
        
         write_state_hice(fileout_hice, hice_out) #hice_out)
         days = days + 1
         day = day + 1
         
       year = year +1
        

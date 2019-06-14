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
## run for 1987 jan whole month: 

##     python interpo_aice_hice.py 1987 1 1 30

#####
## this script is modified from /discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/20160301_test/interpo_aice_hice.py
## major changes include: write out hice and seaice in different files
#####
##### 
## current version of this scripts are only good until year 2014
## for year 2016 has different input files for hice (heff)
## to run for 2016 onwards, gmask and gm need to be used (uncomment line 360 to 366)
#####
def write_state_aice(fname, AICE):

    nx=AICE.shape[1]
    ny=AICE.shape[0]
    nt=1

    ncfile = Dataset(fname,'w')
    ncfile.createDimension('xaxis_1',nx)
    ncfile.createDimension('yaxis_1',ny)
    ncfile.createDimension('Time',nt)

    aice = ncfile.createVariable('AICE',np.dtype('float32').char,('Time','yaxis_1','xaxis_1'))

    aice[:] = AICE
    ncfile.close()
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
       
       #if (day == 1):
       #	day1=31
       #	month1=month-1
       #else:
       #day1=day-1
       day1=day
       month1=month
       #if (day == 1 & month == 1):
       #	day1=31
       #	month1=12
       #	year1=year-1
       	
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
       print 'lon2 min: ', lon2.min()
       print 'lon2 max: ', lon2.max()
       print 'lon2 shape ', lon2.shape
       print 'lat2 shape ', lat2.shape
       if (year == 2016):
      		 filein2='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d' %year
      		 #filein2pre='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d.nc' %pre_year
      		 filein2pre='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d' %year
       		 filein2next='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d' %year
       		 ss=np.fromfile(filein2,dtype='float32')
       		 hice_p_thisyear=np.reshape(ss,(12, lon2.shape[0], lon2.shape[1]))
       		 ss_next=np.fromfile(filein2next,dtype='float32')
       		 hice_p_next=np.reshape(ss_next,(12, lon2.shape[0], lon2.shape[1]))
                 ss_pre=np.fromfile(filein2pre,dtype='float32')
                 hice_p_pre=np.reshape(ss_pre,(12, lon2.shape[0], lon2.shape[1]))

                 #ncfilepre=netcdf.netcdf_file(filein2pre,'r')
                 #dum=ncfilepre.variables['lon_scaler'][:]
                 #lon2pre=np.zeros(np.shape(dum))
                 #lon2pre[:]=dum

                 #dum=ncfilepre.variables['lat_scaler'][:]
                 #lat2pre=np.zeros(np.shape(dum))
                 #lat2pre[:]=dum

                 #dum=ncfilepre.variables['heff'][:]
                 #hice_p_pre=np.zeros(np.shape(dum))
                 #ice_p_pre[:]=dum
                 #ncfilepre.close()
       if (year == 2015): 
		 filein2='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d.nc' %year

                 filein2pre='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d.nc' %pre_year
       		 filein2next='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d' %next_year
		 ncfilethis=netcdf.netcdf_file(filein2,'r')
		 dum=ncfilethis.variables['heff'][:]
	         hice_p_thisyear=np.zeros(np.shape(dum))
       		 hice_p_thisyear[:]=dum
		 ncfilepre=netcdf.netcdf_file(filein2pre,'r')
		 dum=ncfilepre.variables['heff'][:]
	         hice_p_pre=np.zeros(np.shape(dum))
       		 hice_p_pre[:]=dum
		 ss_next=np.fromfile(filein2next,dtype='float32')
       		 hice_p_next=np.reshape(ss_next,(12, lon2.shape[0], lon2.shape[1]))
		 ncfilethis.close()
		 ncfilepre.close()
       if (year < 2015):
                 filein2='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d.nc' %year

                 filein2pre='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d.nc' %pre_year
                 filein2next='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/GIOMAS/RAW/heff/heff.H%d.nc' %next_year
                 ncfilethis=netcdf.netcdf_file(filein2,'r')
                 dum=ncfilethis.variables['heff'][:]
                 hice_p_thisyear=np.zeros(np.shape(dum))
                 hice_p_thisyear[:]=dum
                 ncfilepre=netcdf.netcdf_file(filein2pre,'r')
                 dum=ncfilepre.variables['heff'][:]
                 hice_p_pre=np.zeros(np.shape(dum))
                 hice_p_pre[:]=dum
                 ncfilenext=netcdf.netcdf_file(filein2next,'r')
                 dum=ncfilenext.variables['heff'][:]
                 hice_p_next=np.zeros(np.shape(dum))
                 hice_p_next[:]=dum
                 ncfilethis.close()
                 ncfilepre.close()
                 ncfilenext.close()

       hice_p_3year=np.concatenate((hice_p_pre,hice_p_thisyear,hice_p_next)) 
       hice_p_3year=np.ma.masked_where(hice_p_3year>9999, hice_p_3year)
       hice_p_extend=hice_p_3year[11:25,:,:]
       print 'hice_p_3year shape ', hice_p_3year.shape
 
       #for n in range(1,13):
#		print n
#      		hice_p[n-1]=np.ma.masked_where(gmask==0, hice_p[n-1])
 #      lon2=np.ma.masked_where(gmask==0, lon2)
 #      lat2=np.ma.masked_where(gmask==0, lat2)
       #print 'calenderdays: ', calenderdays[:]
       #print 'XP: ', XP[:]
       
       ii=0
       while (ii <= 275):
               jj=0
               while (jj <= 359):
                       dum=np.interp(calenderdays_extend[15:caldayend],XP_extend[:],hice_p_extend[:,ii,jj])
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
	         
         #### check if aice input data exist
         aice_file = '/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/INPUT/%d/gmao-obs-%d.nc' %(year,yyyymmdd)
	 fileout_hice='OUTPUT/%d/hice/seaice_hice_%d.nc' % (year,yyyymmdd)
         if os.path.isfile(aice_file):
	 	yyyymmdd1=yyyymmdd
	 else:
		yyyymmdd=year1*10000+month1*100+day1        
          	print 'aice missing'	
		print 'aice_file=', aice_file
	        ###
	 filein1='/discover/nobackup/zli7/geos5/Sub_seasonal/Obs_to_Tri_interp/INPUT/%d/gmao-obs-%d.nc' %(year,yyyymmdd)
	 fileout_aice='OUTPUT/%d/aice/seaice_aice_%d.nc' % (year,yyyymmdd)
#	 ncfile=Dataset(filein1,'r')
#	 dum=ncfile.variables['lon'][:]
#	 lon1=np.zeros(np.shape(dum))
#	 lon1[:]=dum
#	 lon1[lon1>80.0]=lon1[lon1>80.0]-360.0
#	 #lon1[lon1<=0.0]=lon1[lon1<=0.0]+360.0
#	 print 'lon1 ',lon1.shape
#	
#	 dum=ncfile.variables['lat'][:]
#	 lat1=np.zeros(np.shape(dum))
#	 lat1[:]=dum
#	 
#	 dum=ncfile.variables['value'][:]
#	 aice_p=np.zeros(np.shape(dum))
#	 aice_p[:]=dum
#	 aice_p=np.squeeze(aice_p)
#	 ncfile.close()
#	 print aice_p.shape
#	
#		 ######get tripolar LON LAT
#	 LON, LAT, numlevels = get_grid()
#
#	 ####find mininum lat in obs to maskout artifactual seaice after interpo
#	 print 'AA ', lon1.min(), lon1.max()
#	 print 'AA ', lat1.min(), lat1.max()
#	 ## divide by lon zone and mask zone by zone
#	 lon_zone=np.linspace(-280,80,37)
#	 min_lat_ind=np.where(aice_p > 0)
#	 #print min_lat_ind
#	 min_lat_N=np.linspace(0,36,37)
#	 min_lat_S=np.linspace(0,36,37)
#	 for ii in min_lat_ind:
#	 	yy=lat1[ii]
#	 	xx=lon1[ii]
#	 	yy_N=yy[yy >= 0] #yy_N: non zero points' lat value in NH
#	 	yy_S=yy[yy < 0]
#	 	yy_S_abs=abs(yy_S)
#	 for mm in range(1,37):
#		Izone_N=np.where((xx>lon_zone[mm-1])& (xx<=lon_zone[mm]) & (yy >= 0))
#	 	min_lat_N[mm-1]=np.min(yy_N[Izone_N]) #min_lat_N:min lat value (mask threadshould) in each lon zone in NH
#	 	#min_lat_N[mm-1]=np.min(yy[Izone_N]) #min_lat_N:min lat value (mask threadshould) in each lon zone in NH
#	 	#min_lat_S[mm-1]=np.min(yy_S_abs[Izone])
#	 	min_lat_S[mm-1]=np.min(yy_S_abs)   #SH is masked out using one value(not by zones)
#	 print 'maskout min lat in NH='+str(min_lat_N[0:36])
#	 #print 'maskout min lat in SH='+str(-min_lat_S[0])
#	 #min_lat_N[:]=44.75
#	 #min_lat_S=yy_S_abs.min()
#	 
#	 ###Interpo and maskout by zones
#	 aice_out = nearest_interp(lon1, lat1, aice_p, LON, LAT, undef=0.)
#	 #aice_out = nearest_interp_new(lon1, lat1, aice_p, LON, LAT)
#	 aice_out=np.squeeze(aice_out)
#	 for mm in range(1,37):
#		I=np.where((LON>lon_zone[mm-1])& (LON<=lon_zone[mm]) & (LAT>-min_lat_S[mm-1]) & (LAT<min_lat_N[mm-1]))
#	        #print mm, lon_zone[mm-1], lon_zone[mm], -min_lat_S[mm-1], min_lat_N[mm-1]
#	 	#print LON.min(), LON.max()
#	 	#print LAT.min(), LAT.max()
#	 	#print I
#	 	aice_out[I]=0.
#	 	#print aice_out.max()
#	 	#print aice_out.min()
# 	 write_state_aice(fileout_aice, aice_out) #aice_out)
         
	 ######get tripolar LON LAT
 	 LON, LAT, numlevels = get_grid()
         hice_temp = hice_day[days,:,:].flatten()
         hice_in = hice_temp[hice_temp<1000]
         lon_in = lon2.flatten()[hice_temp<1000]
         lat_in = lat2.flatten()[hice_temp<1000]
         print 'lon_in  shape ', lon_in.shape 
         print 'lat_in  shape ', lat_in.shape 
         print 'hice_in  shape ', hice_in.shape 
         #gm=gmask.flatten()[hice_temp<1000]
         #hice_in = hice_temp[gm>0]
         #lon_in = lon_in[gm>0]
         #lat_in = lat_in[gm>0]
         #print 'lon_in  shape 1 ', lon_in.shape 
         #print 'lat_in  shape 1', lat_in.shape 
         #print 'hice_in  shape 1', hice_in.shape 
         hice_out = nearest_interp(lon_in, lat_in, hice_in, LON, LAT, undef=np.nan)
         #hice_out = nearest_interp_new(lon_in, lat_in, hice_in, LON, LAT)
        
         write_state_hice(fileout_hice, hice_out) #hice_out)
         days = days + 1
         day = day + 1
         
         ###write out nc file
       year = year +1
        

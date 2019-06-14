import matplotlib
matplotlib.use('agg')
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
import sys
import datetime
from scipy.io import netcdf
import os.path
import os
import scipy
import ocean_obs_utils


def haversine_np(lon1, lat1, lon2, lat2):
    """                                                                                                                                                                                                                     
    Calculate the great circle distance between two points                                                                                                                                                                  
    on the earth (specified in decimal degrees)                                                                                                                                                                             
                                                                                                                                                                                                                            
    All args must be of equal length.                                                                                                                                                                                       
                                                                                                                                                                                                                            
    """
    lon1, lat1, lon2, lat2 = map(np.radians, [lon1, lat1, lon2, lat2])

    dlon = lon2 - lon1
    dlat = lat2 - lat1

    a = np.sin(dlat/2.0)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2.0)**2

    c = 2 * np.arcsin(np.sqrt(a))
    km = 6367 * c
    return km

def nearest_interp(lon, lat, z, LON, LAT, undef=np.nan):

    lon[lon>80.0]=lon[lon>80.0]-360.0
    points=np.array( (lon.flatten(), lat.flatten()) ).swapaxes(0, 1)
    zout=interpolate.NearestNDInterpolator(points, z.flatten())(LON, LAT)
    
    return zout


def smooth(x,window_len=11,window='flat'):
    """smooth the data using a window with requested size.                                                                                                                                                                                       
                                                                                                                                                                                                                                                 
    This method is based on the convolution of a scaled window with the signal.                                                                                                                                                                  
    The signal is prepared by introducing reflected copies of the signal                                                                                                                                                                         
    (with the window size) in both ends so that transient parts are minimized                                                                                                                                                                    
    in the begining and end part of the output signal.                                                                                                                                                                                           
                                                                                                                                                                                                                                                 
    input:                                                                                                                                                                                                                                       
        x: the input signal                                                                                                                                                                                                                      
        window_len: the dimension of the smoothing window; should be an odd integer                                                                                                                                                              
        window: the type of window from 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'                                                                                                                                                     
            flat window will produce a moving average smoothing.                                                                                                                                                                                 
                                                                                                                                                                                                                                                 
    output:                                                                                                                                                                                                                                      
        the smoothed signal                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                 
    example:                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    t=linspace(-2,2,0.1)                                                                                                                                                                                                                         
    x=sin(t)+randn(len(t))*0.1                                                                                                                                                                                                                   
    y=smooth(x)                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                 
    see also:                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                 
    numpy.hanning, numpy.hamming, numpy.bartlett, numpy.blackman, numpy.convolve                                                                                                                                                                 
    scipy.signal.lfilter                                                                                                                                                                                                                         
                                                                                                                                                                                                                                                 
    TODO: the window parameter could be the window itself if an array instead of a string                                                                                                                                                        
    Stolen from: http://www.scipy.org/Cookbook/LinearRegression?highlight=%28regress%29                                                                                                                                                          
    """

    #if x.ndim != 1:                                                                                                                                                                                                                             
    #    raise ValueError, "smooth only accepts 1 dimension arrays."                                                                                                                                                                             

    #if x.size < window_len:                                                                                                                                                                                                                     
    #    raise ValueError, "Input vector needs to be bigger than window size."                                                                                                                                                                   


    if window_len<3:
        return x

    if not window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman','std']:
        raise ValueError, "Window is one of 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'"

    if window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman']:
        s=np.r_[2*x[0]-x[window_len:1:-1],x,2*x[-1]-x[-1:-window_len:-1]]
        if window == 'flat': #moving average
            w=np.ones(window_len,'d')
        else:
            w=eval('np.'+window+'(window_len)')

        tmpy=np.convolve(w/w.sum(),s,mode='same')
        y=tmpy[window_len-1:-window_len+1]

        print

    if window in ['std']:
        y = std_filter(x,n_std=float(window_len))

    y[-window_len+1:] = 99999.9#np.nan
    y[0:window_len]   = 99999.9#np.nan
    
    return y
                                  
def std_filter(x,n_std=3.):

    y=x
    std=scstats.nanstd(x)
    mean=scstats.nanmean(x)
    for iter in range(0,4):
        y[ np.where( (y>mean+n_std*std) | (y<mean-n_std*std) ) ] = np.nan

    return y

######################
# OBSERVATION READERS
######################

def cs2_reader(yyyy, mm, dd, hh, platform='CS2-HICE', NDAYS=3.0):

    GOBSDIR = '/gpfsm/dnb42/projects/p17/gvernier/OCEAN_DAS_RC/obs/'#cryosat2
    fname = GOBSDIR+'cryosat2/'+yyyy+mm+'_cs2thickness.nc'

    LON = []
    LAT = []
    VAR = []
    DATE_TIME = []
    INST_ID = []
    QC_FLAG = []
    QC_PRF = []
    NPTS = []
    DEPTH = []
    QC_LEV = []
    OBS_ERROR = []
    DATA_ID = []
    N_PROF = []
    N_LEVS=[]
    obs_cnt = 0
    cnt = 0



    if ((hh!='12') | (dd!='15') ):
        return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

    ncfile = Dataset(fname,'r') 
    #ncfile=netcdf.netcdf_file(fname)
    VAR       = ncfile.variables['sea_ice_thickness'][:]
    LON       = ncfile.variables['lon'][:]
    LAT       = ncfile.variables['lat'][:]
    ncfile.close()

    VAR[VAR<0.0]=np.nan
    VAR[VAR>10]=np.nan    
    VAR[LAT>88]=np.nan

    I=np.where(np.isfinite(VAR))
    VAR=VAR[I].flatten()
    LON=LON[I].flatten()
    LAT=LAT[I].flatten()
    
    VAR = np.reshape(VAR, (len(VAR),1))

    OBS_ERROR = 0.5*np.ones(np.shape(VAR))

    yyyymmddhh=yyyy+mm+dd+hh
    DATE_TIME = int(yyyymmddhh)*np.ones(np.shape(LON))
    DEPTH = 15.0*np.ones(np.shape(VAR))
    N_LEVS=1
    QC_LEV = 1.0*np.ones(np.shape(VAR))        
    QC_PRF = 1.0*np.ones(np.shape(LON))
        
    return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

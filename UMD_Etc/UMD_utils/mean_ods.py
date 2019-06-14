#! /usr/bin/env python
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
from datetime import date
import scipy.interpolate as interp
import scipy.optimize as optm
import subprocess
from utils import get_state, write_state, write_mom_restart
#sys.path.append('/usr/local/other/SLES11/mpi4py-1.3/lib/python/')
#from mpi4py import MPI
#Ne=int(sys.argv[1])

class Ods:
    def __init__(self, fname):
        ncfile = Dataset(fname, 'r')
        self.OBSID = ncfile.variables['OBSID'][:]
        self.lon = ncfile.variables['lon'][:]
        self.lat = ncfile.variables['lat'][:]
        self.lev = ncfile.variables['lev'][:]
        self.obs = ncfile.variables['obs'][:]
        self.sigo = ncfile.variables['sigo'][:]
        self.omf = ncfile.variables['omf'][:]
        self.qc = ncfile.variables['qc'][:]
        ncfile.close()

        self.std_omf = np.zeros(np.shape(self.omf))
        #self.oma = np.zeros(np.shape(self.omf))
        #self.std_oma = np.zeros(np.shape(self.omf))

def ana_stats(yyyy, mm, dd, hh, stats_type='omf'):
    #ods_flist=glob.glob('./oana-'+yyyy+mm+dd+'_'+seq+'/'+stats_type+'/*.dat.nc')
    #ods_flist=glob.glob('./ocean_observer_'+yyyy+mm+dd+'_'+hh+'/*.dat.nc')
    ods_flist=glob.glob('./*.dat.nc')
    ods_flist.sort()
    print ods_flist
    #Compute mean
    #============
    index=0.
    for ods_fname in ods_flist:
        print ods_fname
        if (index==0.):
            obs=Ods(ods_fname)
        else:
            obs_tmp = Ods(ods_fname)
            obs.omf = obs.omf + obs_tmp.omf
        index+=1.
    if (stats_type=='omf'):
        obs.omf = obs.omf/index
    if (stats_type=='oma'):
        obs.oma = obs.omf/index

    #Compute std
    #============
    index=0.
    for ods_fname in ods_flist:
        if index==0:
            std_omf = np.zeros(np.shape(obs.omf))
        obs_tmp = Ods(ods_fname)
        std_omf = std_omf + (obs.omf - obs_tmp.omf)**2
        index+=1.

    if (stats_type=='omf'):
        obs.std_omf = np.zeros(np.shape(obs.omf))    
        obs.std_omf = (std_omf/(index-1.))**0.5
    if (stats_type=='oma'):
        obs.std_oma = np.zeros(np.shape(obs.omf))    
        obs.std_oma = (std_omf/(index-1.))**0.5
    
    return obs

yyyy       = sys.argv[1]     # '2012'
mm         = sys.argv[2]     # '12'
dd         = sys.argv[3]     # '01'
hh        = sys.argv[4]     # DA sequence 
#seq        = sys.argv[5]     # DA sequence 
stats_type = sys.argv[5]     # omf or oma

outfile='obs-'+yyyy+mm+dd+'_'+hh+'.nc'
print outfile

#try:
obs=ana_stats(yyyy, mm, dd, hh, stats_type=stats_type)

if (stats_type=='omf'):
    
    ncfile = Dataset(outfile,'w') 
    ncfile.createDimension('nobs',len(obs.omf))
    
    tmp = ncfile.createVariable('OBSID',np.dtype('int32').char,('nobs'))
    tmp[:] = obs.OBSID

    tmp = ncfile.createVariable('lon',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.lon

    tmp = ncfile.createVariable('lat',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.lat

    tmp = ncfile.createVariable('lev',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.lev

    tmp = ncfile.createVariable('obs',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.obs

    tmp = ncfile.createVariable('sigo',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.sigo

    tmp = ncfile.createVariable('omf',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.omf

    tmp = ncfile.createVariable('std_omf',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.std_omf

if (stats_type=='oma'):
    #obs=ana_stats(stats_type='oma')
        
    ncfile = Dataset(outfile,'a') 
    tmp = ncfile.createVariable('oma',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.oma

    tmp = ncfile.createVariable('std_oma',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.std_oma

ncfile.close()
#except:
#    print 'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDddd'
#    print 'pass ...'
#    pass

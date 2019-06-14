#! /usr/bin/env python
import matplotlib
matplotlib.use('agg')
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
import utils
import scipy.stats as scstats

yyyy     = sys.argv[1]
mm       = sys.argv[2]
dd       = sys.argv[3]  

yyyymmdd=yyyy+mm+dd

ogrid=utils.ModelGrid(fname='./grid_spec.nc')

plt.figure(num=1, figsize=(12,12), facecolor='w')
#Static background spread
#========================
fname='./oana-'+yyyymmdd+'_1/recenter/moments3d-'+yyyymmdd+'.nc'
print fname
ncfile = Dataset(fname, 'r')
stdT = np.squeeze(ncfile.variables['std_T'][:])
stdS = np.squeeze(ncfile.variables['std_S'][:])
ncfile.close()

plt.subplot(321)
ch=utils.plt2d(ogrid.x, ogrid.y,stdT[0,:,:],minval=0, maxval=1.0)
plt.colorbar(ch,shrink=0.5)
titlestr='std(SST) Static bkg'
plt.title(titlestr)

plt.subplot(322)
ch=utils.plt2d(ogrid.x, ogrid.y,stdS[0,:,:],minval=0, maxval=.3)
plt.colorbar(ch,shrink=0.5)
titlestr='std(SSS) Static bkg'
plt.title(titlestr)

#Analysis or 2nd background spread
#=================================
fname='./oana-'+yyyymmdd+'_1/mean_ana_restart/std_ocean_temp_salt.res.nc'
ncfile = Dataset(fname, 'r')
stdT = np.squeeze(ncfile.variables['temp'][:])
stdS = np.squeeze(ncfile.variables['salt'][:])
ncfile.close()

plt.subplot(323)
ch=utils.plt2d(ogrid.x, ogrid.y,stdT[0,:,:],minval=0, maxval=1.0)
plt.colorbar(ch,shrink=0.5)
titlestr='std(SST) after analysis of In-situ T/S/ADT'
plt.title(titlestr)

plt.subplot(324)
ch=utils.plt2d(ogrid.x, ogrid.y,stdS[0,:,:],minval=0, maxval=.2)
plt.colorbar(ch,shrink=0.5)
titlestr='std(SSS) after analysis of In-situ T/S/ADT'
plt.title(titlestr)

#Last Analysis spread
#=================================
fname='./oana-'+yyyymmdd+'_2/mean_ana_restart/std_ocean_temp_salt.res.nc'
ncfile = Dataset(fname, 'r')
stdT = np.squeeze(ncfile.variables['temp'][:])
ncfile.close()

plt.subplot(325)
ch=utils.plt2d(ogrid.x, ogrid.y,stdT[0,:,:],minval=0, maxval=1.0)
plt.colorbar(ch,shrink=0.5)
titlestr='std(SST) after SST analysis'
plt.title(titlestr)

plt.suptitle(yyyy+'/'+mm+'/'+dd, fontsize=20)

plt.savefig('surface-spread-'+yyyymmdd)

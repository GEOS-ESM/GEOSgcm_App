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
import utils
import scipy.stats as scstats
#sys.path.append('/usr/local/other/SLES11/mpi4py-1.3/lib/python/')
#from mpi4py import MPI
#Ne=int(sys.argv[1])

bkg_flist=glob.glob('../wrkdir/oana-20121130_1/bkg/???/ocean_temp_salt.res.nc')
bkg_flist.sort()
ana_flist=glob.glob('../wrkdir/oana-20121130_1/ana/???/ocean_temp_salt.res.nc')
ana_flist.sort()

print bkg_flist

#dirname='../wrkdir/incr_adt/'
#subprocess.call(['mkdir',dirname])
index=0

for ana_fname in ana_flist:
    print ana_fname, bkg_flist[index]
    Ta, Sa, SSHa = utils.get_state(ana_fname)
    #Ta, Sa, SSHa, Pba = np.squeeze(utils.get_state(ana_fname))
    Tb, Sb, SSHb  = utils.get_state(bkg_flist[index])

    dT=np.squeeze(Ta-Tb)
    dS=np.squeeze(Sa-Sb)

    #dPb=Pbb
    #rho0=1025.0
    #g=9.81

    #dPb[dPb==0.0]=np.nan
    #dPb=dPb-scstats.nanmean(dPb.flatten())    
    #print np.max(dPb.flatten())
    #plt.sca(grid[])
    dsteric, dhalosteric, dthermosteric = utils.steric(Tb, Sb, dT, dS, SSHa-SSHb, SSHb)

    index+=1

    plt.clf()

    

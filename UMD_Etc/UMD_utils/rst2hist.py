#! /usr/bin/env python
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title: rst2hist.py
# *
# * Description: Change variable name inside of netcdf file <=== Look into NCO utilities as a replacement
# *              
# * Args:  
# *              fname2d  : Barotropic restart
# *              fname3d  : Temp Salt restart
# *              
# * Example:             
# *
# *             rst2hist.py ocean_temp_salt.nc ocean_barotropic.nc
# *              
# *              
# * @author: Guillaume Vernieres
# */
# 
# /////////////////////////////////////////////////////////////////////////
# Date: Dec 2015

from netCDF4 import Dataset
import sys

fname2d=sys.argv[1]
fname3d=sys.argv[2]

try:
    ncfile = Dataset(fname2d, 'r+')
    #ncfile.renameVariable('eta_t', 'SLV')
    ncfile.renameVariable('eta_t', 'SSH')
    ncfile.close()
except:
    print 'Did not rename eta_t to SLV in ',fname2d

try:
    ncfile = Dataset(fname3d, 'r+')
    ncfile.renameVariable('temp', 'T')
    ncfile.renameVariable('salt', 'S')
    ncfile.close()
except:
     print 'Did not rename vars in ',fname3d



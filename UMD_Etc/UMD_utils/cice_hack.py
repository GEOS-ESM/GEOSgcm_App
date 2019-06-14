#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import sys
import os
#
# !!!!!!!!!!!! HACK !!!!!!!!! This script copies the sea-ice state in the 4 first level of the ocean_velocity_restart 
# Need to clean up, one day ...
#                             

def transform(VAR, transtyp='logit'):
    '''
    '''

    if transtyp=='logit':
        VARout=np.zeros(np.shape(VAR))
        VARout[:]=VAR[:]
        #Deal with 0% and 100%
        tiny=1e-6
        VARout[VAR<tiny]=tiny
        VARout[VAR>1.0-tiny]=1.0-tiny
        VARout=np.log(VARout) - np.log(1.0 - VARout)

    return VARout

def zeropadvar(VAR, bignum = 999999.9):
    VARout = np.zeros(np.shape(VAR))
    VARout[:]=VAR[:]
    VARout[np.abs(VARout)>bignum]=0.0

    return VARout

class SeaIce:
    def __init__(self, fname, nx=720, ny=410, nz=40, trans=False):
        self.fname=fname
        self.trans = trans
        self.nx = nx
        self.ny = ny
        self.nz = nz

    def read(self):
        print 'fname=',self.fname
        ncfile=Dataset(self.fname, mode="r", clobber=True, format='NETCDF4')
        self.AICE = zeropadvar(ncfile.variables['AICE'][:])
        self.HICE = zeropadvar(ncfile.variables['HICE'][:])
        self.HSNO = zeropadvar(ncfile.variables['HSNO'][:])
        self.DRAFT = zeropadvar(ncfile.variables['DRAFT'][:])
        ncfile.close()

    def write_hack_cice(self, fname_out):

        nx=self.nx
        ny=self.ny
        nz=self.nz
        nt=1

        ncfile = Dataset(fname_out,'w') 
        ncfile.createDimension('xaxis_1',nx)
        ncfile.createDimension('yaxis_1',ny)
        ncfile.createDimension('zaxis_1',nz)
        ncfile.createDimension('Time',nt)

        xaxis_1 = ncfile.createVariable('xaxis_1',np.dtype('float64').char,('xaxis_1'))
        xaxis_1.long_name = 'xaxis_1'
        xaxis_1.units = 'none'
        xaxis_1.cartesian_axis = 'X'

        yaxis_1 = ncfile.createVariable('yaxis_1',np.dtype('float64').char,('yaxis_1'))
        yaxis_1.long_name = 'yaxis_1'
        yaxis_1.units = 'none'
        yaxis_1.cartesian_axis = 'Y'

        zaxis_1 = ncfile.createVariable('zaxis_1',np.dtype('float64').char,('zaxis_1'))
        zaxis_1.long_name = 'zaxis_1'
        zaxis_1.units = 'none'
        zaxis_1.cartesian_axis = 'Z'

        Time = ncfile.createVariable('Time',np.dtype('float64').char,('Time'))
        Time.long_name = 'Time'
        Time.units = 'time level'
        Time.cartesian_axis = 'T'

        seaice = ncfile.createVariable('u',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        seaice.long_name='u'  
        seaice.units='none'

        # More wasteful garbage below
        v = ncfile.createVariable('v',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        v.long_name='v'
        v.units='none'
        
        #Storing the sea-ice state in u
    
        if self.trans:
            seaice[0,0,:,:] = transform(self.AICE)
        else:
            seaice[0,0,:,:] = self.AICE            
        seaice[0,1,:,:] = self.HICE
        seaice[0,2,:,:] = self.HSNO
        seaice[0,3,:,:] = self.DRAFT
        seaice[0,4,:,:] = self.HICE-self.DRAFT # Free-board
        seaice[0,5:nz,:,:]=0.0

        v[:]=0.0
        #seaice[:]=0.0

        ncfile.close()

    def write_cice(self, fname_out):

        nx=self.nx
        ny=self.ny
        nt=1

        ncfile = Dataset(fname_out,'w') 
        ncfile.createDimension('xaxis_1',nx)
        ncfile.createDimension('yaxis_1',ny)
        ncfile.createDimension('Time',nt)

        xaxis_1 = ncfile.createVariable('xaxis_1',np.dtype('float64').char,('xaxis_1'))
        xaxis_1.long_name = 'xaxis_1'
        xaxis_1.units = 'none'
        xaxis_1.cartesian_axis = 'X'

        yaxis_1 = ncfile.createVariable('yaxis_1',np.dtype('float64').char,('yaxis_1'))
        yaxis_1.long_name = 'yaxis_1'
        yaxis_1.units = 'none'
        yaxis_1.cartesian_axis = 'Y'

        Time = ncfile.createVariable('Time',np.dtype('float64').char,('Time'))
        Time.long_name = 'Time'
        Time.units = 'time level'
        Time.cartesian_axis = 'T'

        aice = ncfile.createVariable('AICE',np.dtype('float64').char,('Time','yaxis_1','xaxis_1'))
        aice.long_name='AICE' 
        aice.units='none'
        if self.trans:
            aice[0,:,:] = transform(self.AICE)
        else:
            aice[0,:,:] = self.AICE

        hice = ncfile.createVariable('HICE',np.dtype('float64').char,('Time','yaxis_1','xaxis_1'))
        hice.long_name='HICE'  
        hice.units='none'
        hice[0,:,:] = self.HICE

        hsno = ncfile.createVariable('HSNO',np.dtype('float64').char,('Time','yaxis_1','xaxis_1'))
        hsno.long_name='HSNO'  
        hsno.units='none'
        hsno[0,:,:] = self.HSNO

        draft = ncfile.createVariable('DRAFT',np.dtype('float64').char,('Time','yaxis_1','xaxis_1'))
        draft.long_name='HSNO'  
        draft.units='none'
        draft[0,:,:] = self.DRAFT

        ncfile.close()

fname_in  = sys.argv[1] # File name of the recentered sea-ice state
fname_out = sys.argv[2] # Ensemble member
trans     = sys.argv[3] # Boolean, Transform AICE or not
hack      = sys.argv[4] # Boolean, hack = True: put sea-ice state in u

if (trans == 'True'):
    trans=True
else:
    trans=False

Nx = int(os.environ['ODAS_Nx'])
Ny = int(os.environ['ODAS_Ny'])
Nz = int(os.environ['ODAS_Nz'])

print Nx, Ny, Nz, fname_in, fname_out

bkg_si = SeaIce(fname_in, nx=Nx, ny=Ny, nz=Nz, trans=trans)
bkg_si.read()
if (hack=='True'):
    # Copy the sea-ice state in the velocity restart
    print 'in hack'
    bkg_si.write_hack_cice(fname_out)
else:
    #Save sea-ice state in fname_out
    bkg_si.write_cice(fname_out)


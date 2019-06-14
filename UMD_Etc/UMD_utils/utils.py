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
sys.path.append('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/GODAE/sea_water/')
import eos
import matplotlib.cm as cm
from mpl_toolkits.axes_grid1 import AxesGrid

def get_state(fname):
    print 'fname=',fname
    try:
        ncfile=Dataset(fname, mode="r", clobber=True, format='NETCDF4')
        T=ncfile.variables['temp'][:]
        S=ncfile.variables['salt'][:]
        ncfile.close()
    except:
        print 'No temp salt'
        T=[]
        S=[]

    try:
        fname_baro=fname[0:-16]+'barotropic.res.nc'
        print 'fname=',fname_baro    
        ncfile=Dataset(fname_baro, mode="r", clobber=True, format='NETCDF4')
        SSH=ncfile.variables['eta_t'][:]
        ncfile.close()
    except:
        print 'No barotropic file found.'
        SSH=[]

    return np.squeeze(T), np.squeeze(S), np.squeeze(SSH)

def write_state(fname, T, S, SSH):

    nx=T.shape[2]
    ny=T.shape[1]
    nz=T.shape[0]
    nt=1

    ncfile = Dataset(fname,'w') 
    ncfile.createDimension('xaxis_1',nx)
    ncfile.createDimension('yaxis_1',ny)
    ncfile.createDimension('zaxis_1',nz)
    ncfile.createDimension('Time',nz)

    temp = ncfile.createVariable('temp',np.dtype('float32').char,('zaxis_1','yaxis_1','xaxis_1'))
    salt = ncfile.createVariable('salt',np.dtype('float32').char,('zaxis_1','yaxis_1','xaxis_1'))
    eta_t = ncfile.createVariable('eta_t',np.dtype('float32').char,('yaxis_1','xaxis_1'))

    temp[:] = T
    salt[:] = S
    eta_t[:] = SSH
    ncfile.close()

def write_mom_restart(fname, T, S):

    nx=T.shape[2]
    ny=T.shape[1]
    nz=T.shape[0]
    nt=1

    ncfile = Dataset(fname,'w') 
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

    temp = ncfile.createVariable('temp',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
    temp.long_name='temp'
    temp.units='none'

    salt = ncfile.createVariable('salt',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
    salt.long_name='salt'
    salt.units='none'
    
    xaxis_1[:]=np.linspace(1, nx, num=nx, endpoint=True)
    yaxis_1[:]=np.linspace(1, ny, num=ny, endpoint=True)
    zaxis_1[:]=np.linspace(1, nz, num=nz, endpoint=True)
    Time[:]=1

    temp[0,:,:,:] = T
    salt[0,:,:,:] = S

    ncfile.close()

def write_mom_restart_barotropic(fname, SSH):

    nx=SSH.shape[1]
    ny=SSH.shape[0]
    nt=1

    ncfile = Dataset(fname,'w') 
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

    eta_t = ncfile.createVariable('eta_t',np.dtype('float64').char,('Time','yaxis_1','xaxis_1'))
    eta_t.long_name='eta_t'
    eta_t.units='none'
    eta_t.comments='Centered SLV + delta dynamic height'
    
    xaxis_1[:]=np.linspace(1, nx, num=nx, endpoint=True)
    yaxis_1[:]=np.linspace(1, ny, num=ny, endpoint=True)
    Time[:]=1

    eta_t[0,:,:] = SSH

    ncfile.close()

class ModelGrid():
    def __init__(self, fname='grid_spec.nc'):
        ncfile = Dataset(fname, 'r')
        self.zb = ncfile.variables['zb'][:]
        self.zt = ncfile.variables['zt'][:]
        self.x = ncfile.variables['x_T'][:]
        self.y = ncfile.variables['y_T'][:]
        self.depth_t=np.transpose(ncfile.variables['depth_t'][:])
        self.numlev=np.transpose(ncfile.variables['num_levels'][:])
        try:
            self.area=np.transpose(ncfile.variables['area_T'][:])
        except:
            print 'No area var in grid file'
        ncfile.close()

def steric(Tb, Sb, dT, dS, deta, eta):
    ogrid=ModelGrid(fname='../wrkdir/grid_spec.nc')
    p=np.ones(np.shape(Sb))
    h=np.ones(np.shape(Sb))
    for index in range(0,np.shape(p)[0]):
        p[index,:,:]=ogrid.zt[index]
        if (index==0):
            h[index,:,:]=ogrid.zb[index]#-eta
        else:
            h[index,:,:]=abs(ogrid.zb[index]-ogrid.zb[index-1])
    rho_b=eos.density(Sb, Tb, p)
    rho_a=eos.density(Sb+dS, Tb+dT, p)
    rho_a_halo=eos.density(Sb+dS, Tb, p)
    rho_a_thermo=eos.density(Sb, Tb+dT, p)

    dsteric=-np.nansum( h*(rho_a-rho_b)/rho_b, axis=0)
    dhalosteric=-np.nansum( h*(rho_a_halo-rho_b)/rho_b, axis=0)
    dthermosteric=-np.nansum( h*(rho_a_thermo-rho_b)/rho_b, axis=0)

    return dsteric, dhalosteric, dthermosteric


    '''
    print np.shape(dsteric), np.shape(Sb)

    dsteric[dsteric==0.0]=np.nan
    deta[deta==0.0]=np.nan

    print 'deta-dsteric=',np.nansum(np.abs(deta-dsteric))    

    fig = plt.figure(num=1, figsize=(19,12), facecolor='w')
    #grid = AxesGrid(fig, 111, nrows_ncols = (2, 3), axes_pad = 0.5,
    #                cbar_location="bottom",
    #                cbar_mode="each",
    #                cbar_size="7%",
    #                cbar_pad="2%")

    
    vmax=0.1
    vmin=-vmax

    #plt.sca(grid[0])

    plt.subplot(231)
    ch=plt2d(ogrid.x, ogrid.y, dsteric, minval=vmin, maxval=vmax, colmap=cm.jet)
    #grid.cbar_axes[0].colorbar(ch)
    plt.title('Steric height infered increment [m]')

    #plt.sca(grid[1])
    plt.subplot(232)
    incr_s=np.flipud(dhalosteric)
    incr_s[incr_s==0.0]=np.nan
    ch=plt2d(ogrid.x, ogrid.y, dhalosteric, minval=vmin, maxval=vmax, colmap=cm.jet)
    #grid.cbar_axes[1].colorbar(ch)
    plt.title('Halo Steric height infered increment [m]')

    #plt.sca(grid[2])
    plt.subplot(233)
    incr_t=np.flipud(dthermosteric)
    incr_t[incr_t==0.0]=np.nan
    ch=plt2d(ogrid.x, ogrid.y, dthermosteric, minval=vmin, maxval=vmax, colmap=cm.jet)
    #grid.cbar_axes[2].colorbar(ch)
    plt.title('Thermo Steric height infered increment [m]')

    #plt.sca(grid[3])
    plt.subplot(234)
    ch=plt2d(ogrid.x, ogrid.y, deta, minval=vmin, maxval=vmax, colmap=cm.jet)
    #grid.cbar_axes[3].colorbar(ch)
    plt.title('SSH increment [m]')

    vmin=-0.1
    vmax=0.1
    #plt.sca(grid[4])
    plt.subplot(235)
    err=deta-dsteric
    #err[np.abs(err)<0.05]=np.nan
    #err=np.abs(deta/dsteric)
    ch=plt2d(ogrid.x, ogrid.y, err, minval=vmin, maxval=vmax, colmap=cm.bwr)
    #grid.cbar_axes[4].colorbar(ch)
    plt.title('SSH - Steric [m]')

    #plt.sca(grid[5])
    plt.subplot(236)
    err=deta-dthermosteric
    #err=np.abs(deta/dthermosteric)
    #err[np.abs(err)<0.05]=np.nan
    ch=plt2d(ogrid.x, ogrid.y, err, minval=vmin, maxval=vmax, colmap=cm.bwr)
    #grid.cbar_axes[5].colorbar(ch)
    plt.title('SSH - Thermosteric [m]')


    '''
    plt.subplot(235)
    incr_t=np.flipud(dthermosteric)
    incr_t[incr_t==0.0]=np.nan
    plt.imshow(incr_t,vmin=vmin,vmax=vmax)
    #plt.imshow(np.flipud(dthermosteric),vmin=vmin,vmax=vmax)

    plt.colorbar()

    plt.subplot(231)
    plt.imshow(np.flipud(deta),vmin=vmin,vmax=vmax)
    plt.title('SSH increment [m]')
    plt.colorbar()

    plt.subplot(233)
    err=np.flipud(deta-dsteric)
    err[np.abs(err)<0.02]=np.nan
    plt.imshow(err,vmin=vmin,vmax=vmax)
    #plt.imshow(np.flipud(deta-dsteric),vmin=vmin,vmax=vmax)
    plt.title('SSH - Steric height [m]')
    plt.colorbar()
    '''
    plt.show()
    '''

def dynamic_height(Tb, Sb, dT, dS):
    #
    # INPUT: background Tb and Sb
    #        departure from background: dT, dS
    # OUTPUT: Dynamic height departure from background
    #
    ogrid=ModelGrid(fname='../wrkdir/grid_spec.nc')
    p=np.ones(np.shape(Sb))
    h=np.ones(np.shape(Sb))
    for index in range(0,np.shape(p)[0]):
        p[index,:,:]=ogrid.zt[index]
        if (index==0):
            h[index,:,:]=ogrid.zb[index]#-eta
        else:
            h[index,:,:]=abs(ogrid.zb[index]-ogrid.zb[index-1])
    print 'compute rho'
    rho_b        = eos.density(Sb, Tb, p)
    rho_a        = eos.density(Sb+dS, Tb+dT, p)
    #rho_a_halo   = eos.density(Sb+dS, Tb, p)
    #rho_a_thermo = eos.density(Sb, Tb+dT, p)
    
    dh           = - np.nansum( h*(rho_a-rho_b)/rho_b, axis=0)
    #dh_halo       = - np.nansum( h*(rho_a_halo-rho_b)/rho_b, axis=0)
    #dh_thermo     = - np.nansum( h*(rho_a_thermo-rho_b)/rho_b, axis=0)

    return dh #, dh_thermo, dh_halo

def dynamic_height_1d(Tb, Sb, dT, dS):
    #
    # INPUT: background Tb and Sb
    #        departure from background: dT, dS
    # OUTPUT: Dynamic height departure from background
    #
    ogrid=ModelGrid(fname='../wrkdir/grid_spec.nc')
    p=np.ones(np.shape(Sb))
    h=np.ones(np.shape(Sb))
    for index in range(0,np.shape(p)[0]):
        p[index]=ogrid.zt[index]
        if (index==0):
            h[index]=ogrid.zb[index]#-eta
        else:
            h[index]=abs(ogrid.zb[index]-ogrid.zb[index-1])
    print 'compute rho'
    rho_b        = eos.density(Sb, Tb, p)
    rho_a        = eos.density(Sb+dS, Tb+dT, p)
    rho_a_halo   = eos.density(Sb+dS, Tb, p)
    rho_a_thermo = eos.density(Sb, Tb+dT, p)

    dh           = - np.nansum( h*(rho_a-rho_b)/rho_b, axis=0)
    dh_halo       = - np.nansum( h*(rho_a_halo-rho_b)/rho_b, axis=0)
    dh_thermo     = - np.nansum( h*(rho_a_thermo-rho_b)/rho_b, axis=0)

    return dh, dh_thermo, dh_halo

def plt2d(lon,lat,stuff, minval=-.1, maxval=.1, colmap=cm.spectral):

    levels = np.arange(minval, maxval+(maxval-minval)/25, (maxval-minval)/25)
    map = Basemap(projection='mill', llcrnrlon=20, llcrnrlat=-80, urcrnrlon=380, urcrnrlat=90, resolution='l')
    map.drawcoastlines()
    map.drawcountries()
    map.fillcontinents(color='coral')
    map.drawmapboundary()
    for shift in [0, 360]:
        x, y =map(lon+shift,lat)
        ch=map.contourf(x,y,stuff, levels, origin='lower', extend='both',cmap=colmap)
    return ch



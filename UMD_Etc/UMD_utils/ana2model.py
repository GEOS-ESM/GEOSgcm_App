#! /usr/bin/env python
import glob
from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt
import os.path
import sys
import os
import f90nml

def readnc(fname, varname):
    ncfile=Dataset(fname)
    VAR=np.squeeze(ncfile.variables[varname][:])
    ncfile.close()
    #VAR[np.abs(VAR)>999.9]=0.0    
    return VAR

def check_salt(Salt):
    Salt[Salt<0.0]=0.0 
    return Salt

class TempSalt:
    def __init__(self, bkg_fname, ana_fname):
        #Read background
        ################
        self.Tb=readnc(bkg_fname,'temp')
        self.Sb=readnc(bkg_fname,'salt')

        mask = np.zeros(np.shape(self.Tb))
        mask[np.abs(self.Tb)>0.0]=1.0

        #Read increment
        ###############
        self.Tincr=readnc('temp_reg-tri_ana.nc4','temp')
        self.Sincr=readnc('salt_reg-tri_ana.nc4','salt')        

        Sa = np.zeros(np.shape(self.Sincr))
        Sa[:] = check_salt(self.Sb + self.Sincr*mask) 

        #Save temp_salt analysis restart
        ################################        
        ncfile = Dataset(ana_fname,'w')
        ncfile.createDimension('xaxis_1',self.Tb.shape[2])
        ncfile.createDimension('yaxis_1',self.Tb.shape[1])
        ncfile.createDimension('zaxis_1',self.Tb.shape[0])
        ncfile.createDimension('Time',1)

        tt=ncfile.createVariable('Time',np.dtype('float64').char,('Time'))
        tt=1

        temp=ncfile.createVariable('temp',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        temp[0,:,:,:] = self.Tb+self.Tincr*mask
        tempi=ncfile.createVariable('temp_incr',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        tempi[0,:,:,:]=self.Tincr*mask

        salt=ncfile.createVariable('salt',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        salt[0,:,:,:]=Sa #check_salt(self.Sb+self.Sincr*mask)

        salti=ncfile.createVariable('salt_incr',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        salti[0,:,:,:]=self.Sincr*mask

        ncfile.close()

class SeaIce:
    def __init__(self, ana_fname='ANA-seaice.nc'):
        #Read analysis
        ##############
        self.ICE=readnc('ice_reg-tri_ana.nc4', 'ice')

        #Save 
        ################################        
        ncfile = Dataset(ana_fname,'w')
        ncfile.createDimension('xaxis_1',self.ICE.shape[2])
        ncfile.createDimension('yaxis_1',self.ICE.shape[1])
        ncfile.createDimension('zaxis_1',self.ICE.shape[0])
        ncfile.createDimension('Time',1)
        temp=ncfile.createVariable('ice',np.dtype('float64').char,('Time','zaxis_1','yaxis_1','xaxis_1'))
        temp[0,:,:,:]=self.ICE
        ncfile.close()




ODAS_RC = os.environ['ODAS_RC']
ODAS_Nx = os.environ['ODAS_Nx']
ODAS_Ny = os.environ['ODAS_Ny']
ODAS_Nz = os.environ['ODAS_Nz']
ODAS_GRID_TYPE = os.environ['ODAS_GRID_TYPE']
UMD_LETKFUTILS = os.environ['UMD_LETKFUTILS']



# Command line arguments:
MODEL_Nx = sys.argv[1]
MODEL_Ny = sys.argv[2]
MODEL_Nz = sys.argv[3]

incr_fname = sys.argv[4]
bkg_fname = sys.argv[5]
ana_fname = sys.argv[6] #Output only
 
try:
    seaice_ana = sys.argv[7] 
    do_seaice = True
except:
    do_seaice = False
 
GRID_DIR = ODAS_RC+'GRIDS/'
output_grid = GRID_DIR+'/mom/grid_spec_'+MODEL_Nx+'x'+MODEL_Ny+'x'+MODEL_Nz+'.nc'
native_sosie = GRID_DIR+'/sosie/ogrid-tri-'+MODEL_Nx+'x'+MODEL_Ny+'x'+MODEL_Nz+'.nc'
ana_grid = GRID_DIR+'/oletkf/grid_spec_'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'.nc'
mapping = GRID_DIR+'maps/sosie_mapping_reg-'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'-tri-'+MODEL_Nx+'x'+MODEL_Ny+'x'+MODEL_Nz+'.nc'
command='ln -s '+mapping+' sosie_mapping_reg-tri.nc'
os.system(command)

for varname in ['temp','salt']:
    basedir = ODAS_RC+'BKGERR/anom-'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'-'+ODAS_GRID_TYPE+'/maps/'
    #namelist = GRID_DIR+'/sosie/namelist.tmpl'
    namelist =  GRID_DIR+'/sosie/namelist.'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'-TO-'+MODEL_Nx+'x'+MODEL_Ny+'x'+MODEL_Nz+'.tmpl'
    command = 'cp '+namelist+' sosie_input.nml'
    os.system(command)
    nml = f90nml.read('sosie_input.nml')
    nml['ninput']['cv_in']=varname
    nml['ninput']['cf_in']=incr_fname
    nml['ninput']['cf_x_in']=ana_grid
    nml['ninput']['cf_lsm_in']=ana_grid
    
    nml['n3d']['cf_z_in']=ana_grid
    nml['n3d']['cf_z_out']=native_sosie

    nml['noutput']['cf_x_out']=output_grid

    nml['nnetcdf']['cv_out']=varname

    nml.write('sosie_input.nml', force=True)
    command = UMD_LETKFUTILS+'/sosie-2.6.4/src/sosie.x -f sosie_input.nml > sosie.out'            
    os.system(command)

OCN = TempSalt(bkg_fname, ana_fname)

if do_seaice:
    varname = 'ice'
    basedir = ODAS_RC+'BKGERR/anom-'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'-'+ODAS_GRID_TYPE+'/maps/'
    namelist = GRID_DIR+'/sosie/namelist.tmpl'
    command = 'cp '+namelist+' sosie_input.nml'
    os.system(command)
    nml = f90nml.read('sosie_input.nml')
    nml['ninput']['cv_in']=varname
    nml['ninput']['cf_in']=seaice_ana
    nml['ninput']['cf_x_in']=ana_grid
    nml['ninput']['cf_lsm_in']=ana_grid
    
    nml['n3d']['cf_z_in']=ana_grid
    nml['n3d']['cf_z_out']=native_sosie

    nml['noutput']['cf_x_out']=output_grid

    nml['nnetcdf']['cv_out']=varname

    nml.write('sosie_input.nml', force=True)
    command = UMD_LETKFUTILS+'/sosie-2.6.4/src/sosie.x -f sosie_input.nml > sosie.out'            
    os.system(command)

    CICE = SeaIce()

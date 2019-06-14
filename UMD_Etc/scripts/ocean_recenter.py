#! /usr/bin/env python
import numpy as np
import sys
import os
import f90nml
import subprocess
import glob
from netCDF4 import Dataset

UMD_LETKFUTILS = os.environ['UMD_LETKFUTILS']
ODAS_Nx        = os.environ['ODAS_Nx']
ODAS_Ny        = os.environ['ODAS_Ny']
ODAS_Nz        = os.environ['ODAS_Nz']
ODAS_GRID_TYPE = os.environ['ODAS_GRID_TYPE']
ODAS_RC        = os.environ['ODAS_RC']

def read_cap(path=''):
    '''
    Read month from cap_restart to select ensemble of anomalies
    '''
    f = open(path+'/cap_restart_ana', 'r')
    MM = f.readline()[4:6]
    print 'Using static background for ', MM
    f.close()
    
    return MM

def get_static_ens(yyyy, mm, dd, Ne, type='fcst', trans='1'):
    # To make sure the bkg error are the same over the entire DA window,
    # get the month from the cap_restart
    SCRDIR = os.environ['SCRDIR']
    MM = read_cap(path=SCRDIR)    # Exctract month from cap_restart in the scratch dir of the experiment
    print 'Exctracting ',MM,' anomalies'

    pathname=ODAS_RC+'BKGERR/anom-'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'-'+ODAS_GRID_TYPE+'/'+MM+'/'
    flist=glob.glob(pathname+'*'+MM+'.nc')    
    flist.sort()

    # If not enough static members, use previous-next month.
    print len(flist)
    print Ne
    if (len(flist)<Ne): 
        print 'Loading adjacent months'
        MM0 = str(int(MM)-1).zfill(2)
        MM1 = str(int(MM)).zfill(2)
        MM2 = str(int(MM)+1).zfill(2)
    
        pathname=ODAS_RC+'BKGERR/anom-'+ODAS_Nx+'x'+ODAS_Ny+'x'+ODAS_Nz+'-'+ODAS_GRID_TYPE+'/ANOM/'
        flist=glob.glob(pathname+MM1+'/*.nc')+glob.glob(pathname+MM0+'/*.nc')+glob.glob(pathname+MM2+'/*.nc')

    n=0

    Ne=int(Ne)

    print 'IN RECENTER.PY =============================================='
    print 'Ne=',Ne,' out of ',len(flist),' forecast anomalies'

    for fname in flist[0:Ne]:        
        print fname
        print 'Copy static ensemble members #:',str(n+1),' out of ',Ne
        fname_out=' ./states/state.'+str(n+1)+'.'+yyyy+mm+dd+'.nc'
        command = 'cp '+fname+' '+fname_out        
        os.system(command)
        n+=1
    try:
        flist2=glob.glob('./states/*.nc')
        for fname in flist2:        
            print fname
            ncfile = Dataset(fname, 'r+')
            ncfile.renameVariable('SSH', 'SLV')
            ncfile.close()
    except:
        pass

def main():
    yyyy              = sys.argv[1] 
    mm                = sys.argv[2] 
    dd                = sys.argv[3] 
    center_fname      = sys.argv[4]  # 3D file containing T & S
    center_fname2d    = sys.argv[5]  # 2D   "      "      SLV
    center_fname_cice = sys.argv[6]  # 3D file     "      AICE, HICE, HSNO, DRAFT, FREEBOARD
    Ne                = sys.argv[7]  # Ensemble size
    Units             = sys.argv[8]  # Units of variable T in center_fname (K or C)

    inflation = 1.0
    #dyna_ens=False

    yyyymm=yyyy+mm
    yyyymmdd=yyyy+mm+dd

    #RECENTER AICE, HICE, HSNO, DRAFT
    #================================
    #Create symbolic links to ensemble members
    #---------------------------------------------
    
    os.system('mkdir states')
    print '==============================================='
    print 'Ensemble size:',Ne,' recentering sea-ice state'
    print '==============================================='

    print 'Getting static members for ',yyyy,'/',mm
    #get_static_ens(yyyy, mm, dd, Ne, type='cice', trans='logit')        
    get_static_ens(yyyy, mm, dd, Ne) #, trans='logit')        

    command     = 'cp recenter_tmp.nml recenter.nml'
    os.system(command)

    append_to_file='.false.'
    for X_varname in ['AICE','HICE','HSNO','DRAFT']: #['AICE','HICE','HSNO','DRAFT']:
        nml = f90nml.read('recenter.nml')
        nml['recenter_grid_size']['nx']=int(ODAS_Nx)
        nml['recenter_grid_size']['ny']=int(ODAS_Ny)
        nml['recenter_grid_size']['nz']=1
        
        nml['recenter_grid_size']['grid_fname']='grid_spec.nc'
        nml['recenter_grid_size']['lon_name']='x_T'
        nml['recenter_grid_size']['lat_name']='y_T'
        
        nml['recenter_x']['x_varname']=X_varname
        nml['recenter_x']['x_varshape']='2d'
        nml['recenter_x']['num_levels']=1
        nml.write('recenter.nml', force=True)

        scaling = 0.0
        command = 'mpirun -np '+str(Ne)+' ./ocean_recenter.x '+yyyymmdd+' '+append_to_file+' '+center_fname_cice+' '+str(scaling)+' '+str(inflation)
        print command
        os.system(command)

        append_to_file='.true.'

    #RECENTER SLV
    #=========================

    #os.system('mkdir states')
    print '==============================================='
    print 'Ensemble size:',Ne,' recentering SLV'
    print '==============================================='


    #print 'Linking static members for ',yyyy,'/',mm
    get_static_ens(yyyy, mm, dd, Ne, trans='notrans')

    command     = 'cp recenter_tmp.nml recenter.nml'
    os.system(command)
    for X_varname in ['SLV']:
    #for X_varname in ['SSH']:
    #for X_varname in ['eta_t']:
        nml = f90nml.read('recenter.nml')
        nml['recenter_grid_size']['nx']=int(ODAS_Nx)
        nml['recenter_grid_size']['ny']=int(ODAS_Ny)
        nml['recenter_grid_size']['nz']=1
        
        nml['recenter_grid_size']['grid_fname']='grid_spec.nc'
        nml['recenter_grid_size']['lon_name']='x_T'
        nml['recenter_grid_size']['lat_name']='y_T'
        
        nml['recenter_x']['x_varname']=X_varname
        nml['recenter_x']['x_varshape']='2d'
        nml['recenter_x']['num_levels']=1
        nml.write('recenter.nml', force=True)

        scaling = 0.0 
        command = 'mpirun -np '+str(Ne)+' ./ocean_recenter.x '+yyyymmdd+' '+append_to_file+' '+center_fname2d+' '+str(scaling)+' '+str(inflation)
        os.system(command)
        append_to_file='.true.'

    #sys.exit()

    #RECENTER T AND S
    #=========================
    #Create symbolic links to ensemble members
    #---------------------------------------------
    print '==============================================='
    print 'Ensemble size:',Ne,' recentering T and S'
    print '==============================================='
    
    command     = 'cp recenter_tmp.nml recenter.nml'
    os.system(command)
    append_to_file='.false.'
    for X_varname in ['T','S']:
        nml = f90nml.read('recenter.nml')
        nml['recenter_grid_size']['nx']=int(ODAS_Nx)
        nml['recenter_grid_size']['ny']=int(ODAS_Ny)
        nml['recenter_grid_size']['nz']=int(ODAS_Nz)
        
        nml['recenter_grid_size']['grid_fname']='grid_spec.nc'
        nml['recenter_grid_size']['lon_name']='x_T'
        nml['recenter_grid_size']['lat_name']='y_T'
        
        nml['recenter_x']['x_varname']=X_varname
        nml['recenter_x']['x_varshape']='3d'
        nml['recenter_x']['num_levels']=int(ODAS_Nz)
        nml.write('recenter.nml', force=True)

        if ((X_varname=='T') & (Units=='K')):
            print 'Convert K to C'
            scaling = -273.15
        else:
            scaling = 0.0
        command = 'mpirun -np '+str(Ne)+' ./ocean_recenter.x '+yyyymmdd+' '+append_to_file+' '+center_fname+' '+str(scaling)+' '+str(inflation)
        print command
        os.system(command)
        append_to_file='.true.'

    with open('EnsembleSize.txt', 'wb') as fh:
        fh.write(str(Ne)+'\n')

if __name__ == '__main__':
    Ne=main()
    print Ne


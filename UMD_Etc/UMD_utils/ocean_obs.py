#! /usr/bin/env python
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title: ocean_obs.py
# *
# * Description: Extract the ocean observations for a specific date/time window. 
# *              
# * Args:  
# *              yyyy     : 4 digit year
# *              mm       : 2 digit month
# *              dd       : 2 digit day
# *              hh       : 2 digit hour
# *              obs_type : Entered as a list    
# *                         Argo
# *                         CTD
# *                         XBT
# *                         TAO
# *                         PIRATA
# *                         RAMA
# *                         CryoSat-2
# *                         Jason-1
# *                         Jason-2
# *                         Jason-3
# *                         Saral
# *                         ERS-1
# *                         ERS-2
# *                         TOPEX
# *                         GEOSAT-2
# *                         Envisat
# *                         HY-2A
# *                         Reynolds
# *                         OSTIA
# *                         AVHRR-18
# *                         NOAA-16
# *                         METOP-A
# *                         NASA-TEAM-2
# *                         NOAA-AICE
# *                         OIB-HICE
# *              
# * Example:             
# *
# *             ocean_obs.py 2012 03 15 03 Argo CTD Jason-2 CryoSat-2
# *              
# *             Will create a netcdf file and a series of png figures containing/showing the observations
# *             that are within the assimilation window.
# *              
# * @author: Guillaume Vernieres
# */
# 
# /////////////////////////////////////////////////////////////////////////
# Date: Dec 2015

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
from ocean_obs_utils import cs2_reader

OBSDIR='/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/'
INSITUOBSDIR='/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS'
ADTOBSDIR='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ADT_TRK_7.0/YEARLY_FILES/'
SYNOBSDIR='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/SYN_7.0/'

# OIB data
OIBDIR='/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/sea_ice_da/ICE_BRIDGE/'

# NESDIS L2-SST
NESDISDIR='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
prefix_dict = {
    'NOAA16'  : '0000-STAR-L2P_GHRSST-SSTskin-AVHRR16_G-ACSPO_V2.40-v02.0-fv01.0.nc',
    'METOPA'  : '0000-STAR-L2P_GHRSST-SSTskin-AVHRRMTA_G-ACSPO_V2.40-v02.0-fv01.0.nc'
    }

# Note on sigos from ORAS4 from 2012 tech-memo "The NEMOVAR ocean data assimilation system ..."
# Mostly errors of representation, so same for XBT, CTD and Argo (while measurement errors is quite different)  
# T sigo: 0.18 deg C, single global surface value but depth dependent, lower bound is 0.07 deg C
# S sigo: 0.18 psu, single global surface value but depth dependent, lower bound is 0.02 psu
# SSH: 5 cm

T_prof_sigo = 0.25   # deg C
#S_prof_sigo = 0.18  # psu
S_prof_sigo = 0.025  # psu

xsigo_s    = 1.0 #1.0     
xsigo_t    = 1.0 #1.0
xsigo_sst  = 2.0
xsigo_ssh  = 0.25
xsigo_aice = 1.0
xsigo_hice = 0.01

try:
    EXP_NDAYS = float(os.environ['ODAS_NDAYS'])
except:
    EXP_NDAYS = 0.125 # Default value for the observation window [yyyymmdd_hh-EXP_NDAY
    #EXP_NDAYS = 20.125 # Default value for the observation window [yyyymmdd_hh-EXP_NDAY

logit_transform = False

try:
    T_prof_sigo = float(os.environ['ODAS_T_prof_sigo'])
    S_prof_sigo = float(os.environ['ODAS_S_prof_sigo'])
    ADT_sigo = float(os.environ['ODAS_ADT_sigo'])

    xsigo_s    = T_prof_sigo  
    xsigo_t    = S_prof_sigo
    xsigo_ssh  = ADT_sigo

    SCRDIR = os.environ['SCRDIR']
    #logit_transform = os.environ['ODAS_logit_transform']
    #if (logit_transform=='True'):
    #    logit_transform = True
    #else:
    #    logit_transform = False
except:
    print 'Environement variables not set, reverting to default:'


print 'NDAYS=',EXP_NDAYS
print 'T_prof_sigo=',T_prof_sigo
print 'S_prof_sigo=',S_prof_sigo

# Obs id as defined in the UMD_oletkf
obsid_dict = {
    'id_s_obs'   : 5521, # Salinity
    'id_t_obs'   : 3073, # Temperature
    'id_sst_obs' : 5525, # SST
    'id_ssh_obs' : 5526, # SSH (Not used ...)
    'id_eta_obs' : 5351, # SSH
    'id_aice_obs': 6000, # AICE
    'id_hice_obs': 6001  # HICE
    }

def inv_logit(p):
    return np.exp(p) / (1 + np.exp(p))

def da_window(yyyy, mm, dd, hh, NDAYS, OBSDATE, QCPRF):
    """ 
    This function exctract the observations that are in [yyyymmdd_hh-NDAYS, yyyymmdd_hh+NDAYS]
    Args:
        yyyy (int)     : 4 digit year
          mm (int)     : 2 digit month
          dd (int)     : 2 digit day
          hh (int)     : 2 digit hour
          NDAYS (float): Used to define the size of the assimilation window centered at yyyymmddhh [yyyymmddhh-NDAYS, yyyymmddhh+NDAYS].
          OBSDATE (int): Observation date in a YYYYMMDDHH format
          QCPRF (int)  : Quality control flag 1=good, 0=bad  

    Returns:
        Array of indices that corresponds to the observations that are within the window and a QCPRF flag of 1
    """

    #Lower bound for obs time
    obs_date_min=datetime.datetime(int(yyyy), int(mm), int(dd), int(hh))-datetime.timedelta(days=NDAYS)
    yyyyo=str(obs_date_min.year)
    mmo=str(obs_date_min.month).zfill(2)
    ddo=str(obs_date_min.day).zfill(2)
    hho=str(obs_date_min.hour).zfill(2)

    #Upper bound for obs time
    obs_date_max=datetime.datetime(int(yyyy), int(mm), int(dd), int(hh))+datetime.timedelta(days=NDAYS)
    yyyye=str(obs_date_max.year)
    mme=str(obs_date_max.month).zfill(2)
    dde=str(obs_date_max.day).zfill(2)
    hhe=str(obs_date_max.hour).zfill(2)
    
    try:
        return list(np.squeeze(np.where( (OBSDATE>=int(yyyyo+mmo+ddo+hho)) & (OBSDATE<=int(yyyye+mme+dde+hhe))  & (QCPRF==1) )))
    except:
        return []

def standard_obs_reader(fname, vartype):
    ncfile=netcdf.netcdf_file(fname)
    N_LEVS    = ncfile.dimensions['N_LEVS']
    N_LEVS    = min(N_LEVS, 40)                               # Assumes profiles have been superobed to 40 levels
    DEPTH     = ncfile.variables['DEPTH'][:]
    VAR       = ncfile.variables[vartype][:]
    QC_LEV    = ncfile.variables['QC_LEV'][:]
    QC_PRF    = ncfile.variables['QC_PRF'][:]
    LON       = ncfile.variables['LON'][:]
    LAT       = ncfile.variables['LAT'][:]
    DATE_TIME = ncfile.variables['DATE_TIME'][:]
    OBS_ERROR = ncfile.variables['OBS_ERROR'][:]        
    ncfile.close()

    print np.shape(LON), np.shape(VAR)

    return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

def M2_sst_reader(yyyy, mm, dd, hh, path2scratch):

    # Reads MERRA-2 sst from path_to_scratch
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

    
    fname=path2scratch+'/sst_'+yyyy+mm+dd+'_'+hh+'00z.nc'
    if (os.path.exists(fname)):
        ncf = Dataset(fname, 'r')
        sst = (np.squeeze(ncf.variables['sst'][:]))
        omask = (np.squeeze(ncf.variables['mask'][:]))
        ncf.close()

        #read mom's grid
        fname=path2scratch+'/grid_spec.nc'
        print 'grid file:',fname
        ncf = Dataset(fname, 'r')
        xt = ncf.variables['x_T'][:]
        yt = ncf.variables['y_T'][:]
        ncf.close()

        #Inline vars
        xt=xt[:]
        yt=yt[:]
        sst=sst[:]
        omask=omask[:]
        
        #Get rid of land values
        I=np.where(omask==1.0)
        xt=xt[I]
        yt=yt[I]
        sst=sst[I]

        xt[xt<-180.0]=xt[xt<-180.0]+360.0

        #subsample
        skip=4
        xt=xt[0::skip]
        yt=yt[0::skip]
        sst=sst[0::skip]

        N=len(sst)
        LON = xt
        LAT = yt
        VAR = sst
        yyyymmddhh = yyyy+mm+dd+hh
        DATE_TIME = int(yyyymmddhh)*np.ones(N) 
        INST_ID = 516*np.ones(N)
        QC_FLAG = np.ones(N) 
        QC_PRF = np.ones(N) 
        DATA_ID = np.ones(N) 
        N_PROF = np.ones(N)

        NPTS = np.ones(N)
        DEPTH = np.ones( (N,1) )
        QC_LEV = np.ones( (N,1) )
        OBS_ERROR = 0.5*np.ones( (N,1) )
    
        VAR = np.reshape(VAR, (len(VAR),1))

    N_LEVS = 1

    return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

def l2_gmi_reader(yyyy, mm, dd, hh, NDAYS=0.125):
    
    import read_l2_gmi_rss

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
    obs_cnt = 0
    cnt = 0

    lat, lon, time_, sst, qc, sigo_ = read_l2_gmi_rss.gather_l2_gmi_rss(int(yyyy), int(mm), int(dd), int(hh), NDAYS = NDAYS, plot = False )

    obs_cnt = obs_cnt + np.shape(sst)[0]

    N=len(sst)
    LON = np.append( LON, lon[:] )
    LAT = np.append( LAT, lat[:] )
    VAR = np.append( VAR, sst[:] )
    yyyymmddhh = yyyy+mm+dd+hh
    DATE_TIME = np.append( DATE_TIME, int(yyyymmddhh)*np.ones(N) )
    INST_ID = np.append( INST_ID, 516*np.ones(N) )
    QC_FLAG = np.append( QC_FLAG, np.ones(N) )
    QC_PRF = np.append( QC_PRF, np.ones(N) )
    DATA_ID = np.append( DATA_ID, np.ones(N) )
    N_PROF = np.append( N_PROF, np.ones(N) )

    NPTS = np.append( NPTS, np.ones(N) )
    DEPTH = np.append( DEPTH, np.ones( (N,1) ) )
    QC_LEV = np.append( QC_LEV, np.ones( (N,1) ) )
    OBS_ERROR = np.append( OBS_ERROR, 0.5*np.ones( (N,1) ) )

    cnt+=N
    N_LEVS = 1
    
    VAR = np.reshape(VAR, (len(VAR),1))

    return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

def l2_sst_reader(yyyy, mm, dd, hh, platform='NOAA16', NDAYS=0.125):
    
    # Select L2 files
    date_end   = datetime.datetime(int(yyyy), int(mm), int(dd), int(hh)) + datetime.timedelta(days=NDAYS)
    doy_end = (date_end - datetime.datetime(int(date_end.year), 1, 1)).days + 1

    date_start   = datetime.datetime(int(yyyy), int(mm), int(dd), int(hh)) - datetime.timedelta(days=NDAYS)
    doy_start = (date_start - datetime.datetime(int(date_start.year), 1, 1)).days + 1

    prefix = prefix_dict[platform] #='0000-STAR-L2P_GHRSST-SSTskin-AVHRR16_G-ACSPO_V2.40-v02.0-fv01.0.nc'

    Nhrs=int(NDAYS*24*2.0)
    print 'Nhrs:',Nhrs
    list_of_dates = range(Nhrs)
    list_of_files = range(Nhrs)    
    for n in range(Nhrs):
        if n == 0:
            list_of_dates[0] = date_start
        else:
            list_of_dates[n] = list_of_dates[n-1] + datetime.timedelta(hours=1)

        doy   = (list_of_dates[n] - datetime.datetime(int(list_of_dates[n].year), 1, 1)).days + 1
        year  = str(list_of_dates[n].year)
        month = str(list_of_dates[n].month).zfill(2)
        day   = str(list_of_dates[n].day).zfill(2)
        hour  = str(list_of_dates[n].hour).zfill(2)
        list_of_files[n] = NESDISDIR+'/'+platform+'/'+str(list_of_dates[n].year)+'/'+str(doy).zfill(3)+'/'+year+month+day+hour+prefix

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
    obs_cnt = 0
    cnt = 0
    for fname in list_of_files:
        ncf = Dataset(fname, 'r')
        #!!!!!!!!!!!!!!!!!!!!!!!!!
        #Need to also read bias and std
        #!!!!!!!!!!!!!!!!!!!!!!!!!
        sst = (np.squeeze(ncf.variables['sea_surface_temperature'][:]))-273.15
        qc  = (np.squeeze(ncf.variables['quality_level'][:]))
        lon = np.squeeze(ncf.variables['lon'][:])
        lat = np.squeeze(ncf.variables['lat'][:])
        yyyymmddhh = ncf.time_coverage_start[0:8]+ncf.time_coverage_start[9:11]
        ncf.close()

        sst[qc<5]=np.nan
        sst=sst.flatten()
        I=np.where(np.isfinite(sst))
        sst=sst[I]

        lon = lon.flatten()
        lon=lon[I]
    
        lat = lat.flatten()
        lat=lat[I]

        obs_cnt = obs_cnt + np.shape(sst)[0]

        N=len(sst)
        LON = np.append( LON, lon[:] )
        LAT = np.append( LAT, lat[:] )
        VAR = np.append( VAR, sst[:] )
        DATE_TIME = np.append( DATE_TIME, int(yyyymmddhh)*np.ones(N) )
        INST_ID = np.append( INST_ID, 516*np.ones(N) )
        QC_FLAG = np.append( QC_FLAG, np.ones(N) )
        QC_PRF = np.append( QC_PRF, np.ones(N) )
        DATA_ID = np.append( DATA_ID, np.ones(N) )
        N_PROF = np.append( N_PROF, np.ones(N) )

        NPTS = np.append( NPTS, np.ones(N) )
        DEPTH = np.append( DEPTH, np.ones( (N,1) ) )
        QC_LEV = np.append( QC_LEV, np.ones( (N,1) ) )
        OBS_ERROR = np.append( OBS_ERROR, 0.5*np.ones( (N,1) ) )

        cnt+=N
    N_LEVS = 1

    VAR = np.reshape(VAR, (len(VAR),1))

    return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

def oib_reader(yyyy, mm, dd, hh, platform='AIR-BORN', NDAYS=3.0):

    fname = OIBDIR+'IDCSI4_'+yyyy+mm+dd+'.txt'

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
    obs_cnt = 0
    cnt = 0

    if (hh!='12'):
        return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

    f = open(fname, 'r')
    for line in f:
        try:
            columns = line.split()
            lat=float(columns[0][0:-2])
            lon=float(columns[1][0:-2])
            sit=float(columns[2][0:-2])    # Thickness
            sigo=float(columns[3][0:-2])   # Thickness error
            LON=np.append(LON, lon)
            LAT=np.append(LAT, lat)
            VAR=np.append(VAR, sit)
            OBS_ERROR=np.append(OBS_ERROR, sigo)           
            
        except:
            pass
    
    #plt.plot(LON, ocean_obs_utils.smooth(VAR,window_len=200),'-r')    
    #plt.plot(LON, VAR,'--k',alpha=0.2)    
    #plt.savefig('OIB.png')
            
    VAR = ocean_obs_utils.smooth(VAR,window_len=200)

    f.close()
    VAR = np.reshape(VAR, (len(VAR),1))
    yyyymmddhh=yyyy+mm+dd+hh
    DATE_TIME = int(yyyymmddhh)*np.ones(np.shape(LON))
    DEPTH = 15.0*np.ones(np.shape(VAR))
    N_LEVS=1
    QC_LEV = 1.0*np.ones(np.shape(VAR))        
    QC_PRF = 1.0*np.ones(np.shape(LON))
        
    print np.shape(LON), np.shape(VAR)

    return N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR

class Obs:
    """
    """
    def __init__(self, yyyy, mm, dd, hh, fname, id_obs, vartype='TEMP', xsigo=1.0, descriptor='', platform = '', color='r', markersize=5, NDAYS=EXP_NDAYS):
        """ This function exctract the observations that are within the assimilation window
        Args:
            yyyy (int)     : 4 digit year
              mm (int)     : 2 digit month
              dd (int)     : 2 digit day
              hh (int)     : 2 digit hour
              fname        : Name of file containing the obs 
              id_obs       : Obs identifier
              vartype      : Variable type, one of ['TEMP','SALT','ADT','ICE'] Note that ICE is ice fraction
              xsigo        : Used to rescale the obserror. new_sigo = xsigo x old_sigo
              descriptor   : String describing the instance of the object (ex: T-Argo, Jason-1-ADT, ...)
              NDAYS (float): Used to define the size of the assimilation window centered at yyyymmddhh [yyyymmddhh-NDAYS, yyyymmddhh+NDAYS].
    """

        if ( (platform == 'NOAA16') | (platform == 'METOPA') ):
            print 'L2-SST'
            try:
                N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR = l2_sst_reader(yyyy, mm, dd, hh, platform=platform, NDAYS=EXP_NDAYS)
            except:
                self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
                print 'Failed looking up ',self.descriptor
                return
        elif (platform == 'GMI'):
            #try:
            N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR = l2_gmi_reader(yyyy, mm, dd, hh, NDAYS=EXP_NDAYS)
            #except:
            #    self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
            #    print 'Failed looking up ',self.descriptor
            #    return 
        elif (platform == 'AIR-BORN'):
            try:
                N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR = oib_reader(yyyy, mm, dd, hh, NDAYS=EXP_NDAYS)
            except:
                self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False) 
                print 'Failed looking up ',self.descriptor
                return
        elif (platform == 'CS2-HICE'):
            try:
                N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR = cs2_reader(yyyy, mm, dd, hh, NDAYS=EXP_NDAYS)
            except:
                self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False) 
                print 'Failed looking up ',self.descriptor
                return
        elif (platform == 'M2-SST'):            
            N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR = M2_sst_reader(yyyy, mm, dd, hh, path2scratch=SCRDIR)
        else:
            if not(os.path.exists(fname)):
                self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
                #self.typ   = []
                #self.lon   = []
                #self.lat   = []
                #self.depth = []
                #self.value = []
                #self.oerr  = []
                #self.descriptor = descriptor
                #self.platform = platform
                #self.color=color
                #self.size=markersize
                #self.present=False
                return
            N_LEVS, DEPTH, VAR, QC_LEV, QC_PRF, LON, LAT, DATE_TIME, OBS_ERROR = standard_obs_reader(fname, vartype) # Reads standard iodas observation format

        print ':::::::::::::::::::',descriptor
        I=da_window(yyyy, mm, dd, hh, NDAYS, DATE_TIME, QC_PRF)        
        if (descriptor=='AVHRR18 L2 SST'):
            print 'L2 SST'
            I=da_window(yyyy, mm, dd, hh, NDAYS, np.floor_divide(DATE_TIME,10000), QC_PRF)

        if (len(I)==0): # File present but no obs within window
            self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
            return

        LON=LON[I]
        LAT=LAT[I]        
        VAR=VAR[I,0:N_LEVS]
        DEPTH=DEPTH[I,0:N_LEVS]
        QC_LEV=QC_LEV[I,0:N_LEVS]
        OBS_ERROR=OBS_ERROR[I,0:N_LEVS]            

	lon=np.zeros(np.shape(VAR))
        lat=np.zeros(np.shape(VAR))
	for prof in range(np.shape(VAR)[0]):
            lon[prof,:]=LON[prof]
            lat[prof,:]=LAT[prof]
        lon=lon.flatten()
        lat=lat.flatten()
	depth=DEPTH.flatten()
        value=VAR.flatten()



        # Compute sigo's for insitu profiles
        if ( (id_obs==obsid_dict['id_t_obs']) | (id_obs==obsid_dict['id_s_obs']) ): # Profiles
            #print np.shape(VAR)
            dTdZ=np.zeros(np.shape(VAR))
            for indexp in range(np.shape(VAR)[0]):
                for indexz in range(N_LEVS-1):                
                    dTdZ[indexp, indexz] = np.abs(( VAR[indexp, indexz] - VAR[indexp, indexz+1] )/( DEPTH[indexp, indexz] - DEPTH[indexp, indexz+1] ))
                    if ( (np.isnan(dTdZ[indexp, indexz])) | (dTdZ[indexp, indexz]==1.0) ):
                        dTdZ[indexp, indexz]=dTdZ[indexp, indexz-1]
                        #if (QC_LEV[indexp, indexz]!=1):
                        #dTdZ[indexp, indexz-1] = dTdZ[indexp, indexz-2]
                        #dTdZ[indexp, indexz] = dTdZ[indexp, indexz-1]
                        #break
                    #print indexz, dTdZ[indexp, indexz],VAR[indexp, indexz]
                #print dTdZ[indexp,:]
                #raw_input('<>?')
            dTdZ[dTdZ<1e-3]=1e-3
            OBS_ERROR[:]=2.0*dTdZ[:]

        # Make sure bad obs are out
        I=np.where(QC_LEV.flatten()==1)

        lon=lon[I]
        lat=lat[I]
        depth=depth[I]
        value=value[I]
        typ=id_obs*np.ones(np.shape(value))
        sigo=np.squeeze(OBS_ERROR.flatten()[I])#[I])

        #print np.shape(sigo), np.shape(value)
        #raw_input('<>?')

        if (id_obs==obsid_dict['id_sst_obs']):    #'SST'
            oerr_min=1e-2       
            sigo[sigo<oerr_min]=oerr_min
            sigo[np.isnan(sigo)]=999.9

        if id_obs==obsid_dict['id_eta_obs']: #'ADT'
            oerr_min=0.1 # 10 cm
            sigo[sigo<oerr_min]=oerr_min
            sigo[np.isnan(sigo)]=999.9

            #Impose large sigos in the high latitudes
            sigo_scale=np.zeros(np.shape(sigo))
            sigo_scale=0.01*(LAT[I]/90.0)**4
            sigo_scale=0.1*(lat/90.0)**4
            sigo=(sigo_scale+sigo) #*sigo
            #oerr[LAT<-60]=99.9

        '''
        if ( (id_obs==obsid_dict['id_t_obs']) | (id_obs==obsid_dict['id_s_obs']) ): # Profiles
            # Assumes a exponential decay of obs error 
            if (id_obs==obsid_dict['id_t_obs']):
                prof_sigo = T_prof_sigo
            else:
                prof_sigo = S_prof_sigo
            #D0=200.0 #e-folding scale
            D0=2000000.0 #e-folding scale
            
            #print np.shape(value)
            #raw_input('<>?')

            sigo=prof_sigo*np.ones(np.shape(value))*np.exp(-depth/D0) 
            oerr_max=np.max(sigo)
            sigo[sigo<0.01*oerr_max]=0.01*oerr_max
        '''
        if (id_obs==obsid_dict['id_aice_obs']):
            sigo = 0.05*np.ones(np.shape(value)) #value*0.05

        if (id_obs==obsid_dict['id_hice_obs']):
            sigo = 0.3*np.ones(np.shape(value)) #value*0.05
            #sigo[sigo<0.01]=0.01

        self.typ   = typ
        self.lon   = lon
        self.lat   = lat
        self.depth = depth
        self.value = value
        self.oerr  = xsigo*sigo       # Rescaled sigos
        self.descriptor = descriptor
        self.platform = platform
        self.color=color
        self.size=markersize
        if (len(self.typ)>0):
            self.present=True
        else:
            self.present=False



    def no_obs(self, descriptor='', platform='', color='r',size=10,present=False):
        self.typ   = []
        self.lon   = []
        self.lat   = []
        self.depth = []
        self.value = []
        self.oerr  = []
        self.descriptor = descriptor
        self.platform = platform
        self.color=color
        self.size=size
        self.present=False
    
    def transform(self, transtyp='logit'):
        '''
        Transform the obs and sigo, currently only supports the logit transform ...
        '''
        if self.present:
            if transtyp=='logit':
            #Deal with 0% and 100%
                tiny=1e-3
                self.value[self.value<tiny]=tiny
                self.value[self.value>1.0-tiny]=1.0-tiny
                self.value=np.log(self.value) - np.log(1 - self.value)
            #print np.min(self.value[np.isfinite(self.value)].flatten())
            #print np.max(self.value[np.isfinite(self.value)].flatten())
            #self.oerr= ... sigos need to be transormed as well

            if transtyp=='invlogit':
                self.value = np.exp(self.value) / (1 + np.exp(self.value))

    def plot(self, pngname):
        '''
        
        '''
        if self.present:
            fig = plt.figure(num=1, figsize=(10,8), facecolor='w')
            fig.add_subplot(111)
            map = Basemap(projection='moll', llcrnrlat=-90, urcrnrlat=90,llcrnrlon=-180, urcrnrlon=180, resolution='c', lon_0=-80)
            x, y = map(self.lon, self.lat)            
            if ( (self.typ[0]==5351) | (self.typ[0]==5525) ):
                if (self.typ[0]==5351):
                    valmin=-1.5
                    valmax=1.5
                    errmax=0.25
                if (self.typ[0]==5525):
                    print 'plotting SST ....'
                    print 'sst max:',np.min(self.lat)
                    valmin=-2.0
                    valmax=31.0
                    errmax=1.0

                plt.subplot(211)
                map.drawcoastlines()
                map.drawcountries()
                #map.fillcontinents(color='coral')
                map.drawmapboundary()
                map.scatter(x, y, 1, c=self.value,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                plt.colorbar(shrink=0.5)

                plt.subplot(212)
                map.drawcoastlines()
                map.drawcountries()
                map.fillcontinents(color='coral')
                map.drawmapboundary()
                map.scatter(x, y, 1, c=self.oerr,cmap=cm.spectral,vmin=0,vmax=errmax,edgecolor=None,lw=0)
                plt.colorbar(shrink=0.5)
            elif ( (self.typ[0]==6000) | (self.typ[0]==6001)):                
                valmin=0.
                if (self.typ[0]==6000):
                    valmax=1.0
                if (self.typ[0]==6001):
                    valmax=4.0
                errmax=0.5           
                map = Basemap(projection='npstere',lon_0=0,boundinglat=55, resolution='c')
                x, y = map(self.lon, self.lat)               
                plt.subplot(211)
                map.drawcoastlines()
                map.drawcountries()
                map.fillcontinents(color='coral')
                map.drawmapboundary()
                if logit_transform:
                    map.scatter(x, y, 1, c=inv_logit(self.value),cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                else:
                    map.scatter(x, y, 1, c=self.value,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                #map.scatter(x, y, 1, c=self.value,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                #map.scatter(x, y, 10, c=self.value,cmap=cm.spectral,vmin=0.0,vmax=5.0,edgecolor=None,lw=0)
                #map.scatter(x, y, 1, c=self.oerr,cmap=cm.spectral,vmin=0,vmax=0.01,edgecolor=None,lw=0)
                plt.colorbar(shrink=0.5)  

                map = Basemap(projection='spstere',lon_0=0,boundinglat=-55, resolution='c')
                x, y = map(self.lon, self.lat)               
                plt.subplot(212)
                map.drawcoastlines()
                map.drawcountries()
                map.fillcontinents(color='coral')
                map.drawmapboundary()
                if logit_transform:
                    map.scatter(x, y, 1, c=inv_logit(self.value),cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                else:
                    map.scatter(x, y, 1, c=self.value,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                #map.scatter(x, y, 1, c=self.value,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                plt.colorbar(shrink=0.5)  
                
            else:
                map.drawcoastlines()
                map.drawcountries()
                map.fillcontinents(color='coral')
                map.drawmapboundary()
                map.plot(x, y, color=self.color, marker='.', markersize= self.size, linestyle='None',alpha=0.2)

            titlestr=str(len(self.value))+' Obs'
            plt.title(titlestr)
            plt.savefig(self.descriptor+pngname)
            plt.clf()

def update_list_of_obs(list_of_obs, obs):
    if obs.present:
        list_of_obs.append(obs)

# Profiling drifters, moorings, ...
#==================================
def argo(list_of_obs):
    argo_t  = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/ARGO_6.0/T_ARGO_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo-T', NDAYS=EXP_NDAYS)
    argo_s  = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/ARGO_6.0/S_ARGO_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t)
    update_list_of_obs(list_of_obs, argo_s)
    return list_of_obs

def ctd(list_of_obs):
    ctd_t   = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/CTD_6.0/T_CTD_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', descriptor='CTD-T', NDAYS=EXP_NDAYS)
    ctd_s   = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/CTD_6.0/S_CTD_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='CTD-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, ctd_t)
    update_list_of_obs(list_of_obs, ctd_s)
    return list_of_obs

def xbt(list_of_obs):
    xbt_t   = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/XBT_6.0/T_XBT_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', descriptor='XBT-T', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, xbt_t)
    return list_of_obs

def xbt_synS(list_of_obs):
    print SYNOBSDIR+'/SYN_XBT_'
    xbt_s   = Obs(yyyy, mm, dd, hh, fname=SYNOBSDIR+'/XBT/SYN_XBT_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='XBT-SYN-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, xbt_s)
    return list_of_obs

def tao(list_of_obs):
    tao_t   = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/TAO_6.0/T_TAO_'+yyyy+'.nc',   id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='TAO-T', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, tao_t)
    return list_of_obs

def pirata(list_of_obs):
    pir_t   = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/PIR_6.0/T_PIR_'+yyyy+'.nc',   id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='PIRATA-T', NDAYS=EXP_NDAYS)
    pir_s   = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/PIR_6.0/S_PIR_'+yyyy+'.nc',   id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=10.0*xsigo_s, color='r', descriptor='PIRATA-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, pir_t)
    update_list_of_obs(list_of_obs, pir_s)
    return list_of_obs

def rama(list_of_obs):
    rama_t  = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/RAMA_6.0/T_RAMA_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='RAMA-T', NDAYS=EXP_NDAYS)
    rama_s  = Obs(yyyy, mm, dd, hh, fname=INSITUOBSDIR+'/RAMA_6.0/S_RAMA_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=10.0*xsigo_s, color='r', descriptor='RAMA-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, rama_t)
    update_list_of_obs(list_of_obs, rama_s)
    return list_of_obs

# Altimeters
#===================
def cryosat2(list_of_obs):     #CryoSat-2
    fname=ADTOBSDIR+'ADT_TRK_C2_'+yyyy+'.nc'
    c2_adt  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='CryoSat-2-ADT', color='y')
    update_list_of_obs(list_of_obs, c2_adt)
    return list_of_obs

def jason1(list_of_obs):        #Jason-1
    fname=ADTOBSDIR+'ADT_TRK_J1_'+yyyy+'.nc'
    j1_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-1-ADT', color='m')
    update_list_of_obs(list_of_obs, j1_adt)
    return list_of_obs

def jason2(list_of_obs):        #Jason-2
    fname=ADTOBSDIR+'ADT_TRK_J2_'+yyyy+'.nc'
    j2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-2-ADT', color='m')
    update_list_of_obs(list_of_obs, j2_adt)
    return list_of_obs

def jason3(list_of_obs):        #Jason-3
    fname=ADTOBSDIR+'ADT_TRK_J3_'+yyyy+'.nc'
    j3_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-3-ADT', color='m')
    update_list_of_obs(list_of_obs, j3_adt)
    return list_of_obs

def saral(list_of_obs):        #Saral/Altica
    fname=ADTOBSDIR+'ADT_TRK_AL_'+yyyy+'.nc'
    al_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Saral-Altika-ADT', color='m')
    update_list_of_obs(list_of_obs, al_adt)
    return list_of_obs

def ers1(list_of_obs):        #ERS-1
    fname=ADTOBSDIR+'ADT_TRK_E1_'+yyyy+'.nc'
    e1_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='ERS-1-ADT', color='m')
    update_list_of_obs(list_of_obs, e1_adt)
    return list_of_obs

def ers2(list_of_obs):        #ERS-2
    fname=ADTOBSDIR+'ADT_TRK_E2_'+yyyy+'.nc'
    e2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='ERS-2-ADT', color='m')
    update_list_of_obs(list_of_obs, e2_adt)
    return list_of_obs

def topex_poseidon(list_of_obs):        #TOPEX/POSEIDON
    fname=ADTOBSDIR+'ADT_TRK_TP_'+yyyy+'.nc'
    tp_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='TOPEX-POSEIDON-ADT', color='m')
    update_list_of_obs(list_of_obs, tp_adt)
    return list_of_obs

def geosat_follow_on(list_of_obs):      #GEOSAT follow on
    fname=ADTOBSDIR+'ADT_TRK_G2_'+yyyy+'.nc'
    g2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='GEOSAT-follow-on-ADT', color='m')
    update_list_of_obs(list_of_obs, g2_adt)
    return list_of_obs

def envisat(list_of_obs):       #Envisat
    fname=ADTOBSDIR+'ADT_TRK_EN_'+yyyy+'.nc'
    en_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Envisat-ADT', color='m')
    update_list_of_obs(list_of_obs, en_adt)
    return list_of_obs

def hy2a(list_of_obs):        #HY-2A
    fname=ADTOBSDIR+'ADT_TRK_H2_'+yyyy+'.nc'
    h2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='HY-2A-ADT', color='m')
    update_list_of_obs(list_of_obs, h2_adt)
    return list_of_obs

# SST retrieval
#===================
# Need to choose between Reyn, OSTIA and Hadley depending on date <<<<<<<======= Should be on the to do list
def reyn_L3_sst(list_of_obs):
    fname='/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/SST_6.0/SST_REYN_'+yyyy+'.nc'
    reyn_t = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=xsigo_sst, descriptor='Reynolds-SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, reyn_t)
    return list_of_obs

def ostia_L3_sst(list_of_obs):
    fname   = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/OSTIA/ASSIM_QRT_SUB/'+yyyy+'/SST_OSTIA_'+yyyy+mm+dd+'.nc'
    ostia_t = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=xsigo_sst, descriptor='OSTIA SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, ostia_t)
    return list_of_obs

def avhrr18_L2_sst(list_of_obs):
    fname='/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/L2_SST_7.0/'+yyyy+'/AVHRR18_G_'+yyyy+mm+dd+'.nc'
    avhrr18 = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='AVHRR18 L2 SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, avhrr18)
    return list_of_obs

def noaa16_L2_sst(list_of_obs):
    noaa16 = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='NOAA-16', platform = 'NOAA16', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, noaa16)
    return list_of_obs

def metopa_L2_sst(list_of_obs):
    metopa = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='METOP-A', platform = 'METOPA', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, metopa)
    return list_of_obs

def gmi_L2_sst(list_of_obs):
    gmi = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='GMI-RSS', platform = 'GMI', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, gmi)
    return list_of_obs

def merra2_sst(list_of_obs):
    m2sst = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=1.0, descriptor='M2-SST', platform = 'M2-SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, m2sst)
    return list_of_obs

#def pathfinder(list_of_obs):


# Ice Fraction retrieval
#=======================
def nsidc_aice(list_of_obs):
    fname=OBSDIR+'/AICE_6.0/ICE_NSIDC_'+yyyy+'.nc'
    nsidc_a = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_aice_obs'], vartype='ICE',xsigo=xsigo_aice, descriptor='NASA-TEAM-2', NDAYS=EXP_NDAYS)
    if logit_transform:
        nsidc_a.transform() #logit transform for sea ice fraction
    update_list_of_obs(list_of_obs, nsidc_a)
    return list_of_obs

def reyn_aice(list_of_obs):
    fname=OBSDIR+'/AICE_6.0/ICE_REYN_'+yyyy+'.nc'
    reyn_a = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_aice_obs'], vartype='ICE',xsigo=xsigo_aice, descriptor='NOAA-AICE', NDAYS=EXP_NDAYS)
    if logit_transform:
        reyn_a.transform() #logit transform for sea ice fraction
    update_list_of_obs(list_of_obs, reyn_a)
    return list_of_obs

def ostia_aice(list_of_obs):
    fname   = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/OSTIA/ASSIM_QRT_SUB/'+yyyy+'/ICE_OSTIA_'+yyyy+mm+dd+'.nc'
    ostia_a = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_aice_obs'], vartype='ICE',xsigo=xsigo_aice, descriptor='OSTIA-AICE', NDAYS=EXP_NDAYS)
    if logit_transform:        
        ostia_a.transform() #logit transform for sea ice fraction
    update_list_of_obs(list_of_obs, ostia_a)
    return list_of_obs

# Ice Thickness/Freeboard/Snow depth
#===================================
def oib_hice(list_of_obs):
    oib_hi = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_hice_obs'], platform='AIR-BORN', xsigo=xsigo_hice, descriptor='OIB', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, oib_hi)
    return list_of_obs

def cs2_hice(list_of_obs):
    cs2_hi = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_hice_obs'], platform='CS2-HICE', xsigo=xsigo_hice, descriptor='CS2-HICE', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, cs2_hi)
    return list_of_obs

switch_obs = {
    "Argo"        : argo,
    "CTD"         : ctd,
    "XBT"         : xbt,
    "XBT-SYN-S"   : xbt_synS,
    "TAO"         : tao,
    "PIRATA"      : pirata,
    "RAMA"        : rama, 
    "CryoSat-2"   : cryosat2,
    "Jason-1"     : jason1,
    "Jason-2"     : jason2,
    "Jason-3"     : jason3,
    "Saral"       : saral,
    "ERS-1"       : ers1,
    "ERS-2"       : ers2,
    "TOPEX"       : topex_poseidon,
    "GEOSAT-2"    : geosat_follow_on,
    "Envisat"     : envisat,
    "HY-2A"       : hy2a,
    "Reynolds"    : reyn_L3_sst,
    "OSTIA"       : ostia_L3_sst,
    "M2-SST"      : merra2_sst,
    "AVHRR-18"    : avhrr18_L2_sst,
    "NASA-TEAM-2" : nsidc_aice,
    "NOAA-AICE"   : reyn_aice,
    "OSTIA-AICE"  : ostia_aice,
    "NOAA-16"     : noaa16_L2_sst,
    "METOP-A"     : metopa_L2_sst,
    "GMI-RSS"     : gmi_L2_sst,
    "OIB-HICE"    : oib_hice,
    "CS2-HICE"    : cs2_hice
 }

yyyy     = sys.argv[1]      # '2012'
mm       = sys.argv[2]      # '12'
dd       = sys.argv[3]      # '01'
hh       = sys.argv[4]      # '01'
obs_type = sys.argv[5:]     # 

list_of_obs = []
for doobs in obs_type:
    list_of_obs = switch_obs[doobs](list_of_obs)
    
print '============== Extracting obs ============'
if list_of_obs: 
    cnt=0
    for obs in list_of_obs:
        print obs.descriptor
        pngname='obs-'+mm+'-'+dd+'-'+yyyy
        obs.plot(pngname)
        if (cnt==0):
            typ=obs.typ
            lon=obs.lon
            lat=obs.lat
            depth=obs.depth
            value=obs.value
            oerr=obs.oerr
        else:
            typ=np.concatenate( (typ, obs.typ) )
            lon=np.concatenate( (lon,obs.lon) )
            lat=np.concatenate( (lat,obs.lat) )
            depth=np.concatenate( (depth,obs.depth) )
            value=np.concatenate( (value,obs.value) )

            print np.shape(oerr), np.shape(obs.oerr)

            oerr=np.concatenate( (oerr,obs.oerr) )

        cnt+=1

    nobs=len(typ)
    print 'nobs=',nobs
    with open('Nobs', 'wb') as fh:
        fh.write(str(nobs)+'\n')

    fnameout='gmao-obs-'+yyyy+mm+dd+'.nc'
    ncfile = Dataset(fnameout,'w') 
    ncfile.createDimension('nobs',nobs)

    tmp = ncfile.createVariable('typ',np.dtype('int32').char,('nobs'))
    tmp[:] = typ

    tmp = ncfile.createVariable('lon',np.dtype('float32').char,('nobs'))
    tmp[:] = lon

    tmp = ncfile.createVariable('lat',np.dtype('float32').char,('nobs'))
    tmp[:] = lat

    tmp = ncfile.createVariable('depth',np.dtype('float32').char,('nobs'))
    tmp[:] = depth

    tmp = ncfile.createVariable('value',np.dtype('float32').char,('nobs'))
    tmp[:] = value

    tmp = ncfile.createVariable('oerr',np.dtype('float32').char,('nobs'))
    tmp[:] = oerr

    ncfile.close()
    print 'Saved ',str(nobs),'obs in ',fnameout

else:
    with open('Nobs', 'wb') as fh:
        fh.write('0'+'\n')    
try:
    command = './oceanobs_nc2bin.x -y '+yyyy+' -m '+mm+' -d '+dd+' -indir1 gmao-obs- -outdir .' 
    print command
    os.system(command)
except:
    print 'Could not convert to NCEP binary format'

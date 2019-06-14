#! /usr/bin/env python
import matplotlib.pyplot as plt
import sys 
import glob
import numpy as np
from netCDF4 import Dataset
from datetime import date
import datetime

#obsid_dict = {5525:'SST', 5522:'SSS', 5351:'ADT', 3073: 'T prof', 5521: 'S prof', 6000:'Ice Fraction'} 
obsid_dict = {'SST':5525, 'SSS':5522, 'ADT': 5351,'T prof': 3073, 'S prof':5521, 'Ice Fraction': 6000} 

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

    y[-window_len+1:]=np.nan                                                                                                                                                                        
    y[0:window_len]=np.nan                                                                                                                                                                          

    return y

def std_filter(x,n_std=3.):

    y=x
    std=scstats.nanstd(x)
    mean=scstats.nanmean(x)
    for iter in range(0,4):
        y[ np.where( (y>mean+n_std*std) | (y<mean-n_std*std) ) ] = np.nan

    return y


class ExpHist():
    def __init__(self):
        self.date=[]
        self.mae_omf=[]
        self.mae_oma=[]
        self.bias_omf=[]
        self.bias_oma=[]

class OdaStats():
    def __init__(self, fname='obs01020.dat.nc',alpha=1.0, marker='o'):

        self.alpha=alpha
        self.marker=marker
        ncfile = Dataset(fname, 'r')
        self.fname=fname
        self.OBSID=ncfile.variables['OBSID'][:]
        self.lon=ncfile.variables['lon'][:]
        self.lat=ncfile.variables['lat'][:]
        self.lev=ncfile.variables['lev'][:]
        self.obs=ncfile.variables['obs'][:]
        self.sigo=ncfile.variables['sigo'][:]
        self.omf=ncfile.variables['omf'][:]  
        self.fcst=self.obs-self.omf
        try:
            self.oma=ncfile.variables['oma'][:]  
        except:
            self.oma=np.nan*self.omf
        self.std_omf=ncfile.variables['std_omf'][:]  
        self.yyyymmdd=fname[-30:-26]+'-'+fname[-26:-24]+'-'+fname[-24:-22] #fname[-27:-23]+'-'+fname[-23:-21]+'-'+fname[-21:-19]
        self.time=int(fname[-30:-22])
        self.date=datetime.datetime(int(fname[-30:-26]), int(fname[-26:-24]), int(fname[-24:-22]), int(fname[-21:-19]))
        ncfile.close()
    def mae(self, obstype=3073, region='glb'):
        self.IDS=np.unique(self.OBSID)
        ids = obstype
        
        if region=='glb':
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<300.0) & (self.fcst!=0.0) & (np.abs(self.lat)<90.0) & (np.abs(self.omf)<100.0))
        if region=='trp':
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<300.0) & (self.fcst!=0.0) & (np.abs(self.lat)<30.0) & (np.abs(self.omf)<100.0))
        if region=='north':        
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<300.0) & (self.fcst!=0.0) & (self.lat>0.0) & (np.abs(self.omf)<100.0) )
        if region=='south':        
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<300.0) & (self.fcst!=0.0) & (self.lat<0.0) & (np.abs(self.omf)<100.0) )

        self.mae_omf = np.mean(np.abs(self.omf[I]))
        self.mae_oma = np.mean(np.abs(self.oma[I]))
        self.bias_omf = np.mean(self.omf[I])
        self.bias_oma = np.mean(self.oma[I])
        self.nobs=np.shape(I)[1]

        return self.date, self.mae_omf, self.mae_oma, self.bias_omf, self.bias_oma, self.nobs

yyyy = '2014'
mm   = '10'
dd   = '??'
hh   = '??'

#obsid_dict = {5525:'SST', 5522:'SSS', 5351:'ADT', 3073: 'T prof', 5521: 'S prof', 6000:'Ice Fraction'} 
obstype = obsid_dict['S prof']
#obstype = obsid_dict['Ice Fraction']
plot_type='time_series'
region='glb'
COLORS=['b','r','g','k']
window_len=5
#PATH = ['../../../','/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das/']#,'/gpfsm/dnb42/projects/p17/aborovik/geos5/exp/ab007/ocean_das/']#,'/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk015/ocean_das/']

PATH=['../../../../../gv011/ocean_das/oana-*/',
      '/gpfsm/dnb42/projects/p17/aborovik/geos5/exp/ab007/ocean_das/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das/oana-*']
#      '/gpfsm/dnb42/projects/p17/aborovik/geos5/exp/ab007/ocean_das/oana-*']

#PATH=[ '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das/oana-*']#,
#      '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das_1moment/oana-*']
#PATH = #['/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk015/ocean_das/oana-*_1',

#PATH = ['/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das/oana-*',
#        '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das_1moment/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/aborovik/geos5/exp/ab007/ocean_das/oana-*']

cnt_exp=0
for path in PATH:

    path2files = path+'/ocean_observer_*/obs-'+yyyy+mm+dd+'_'+hh+'.nc'
    flist = glob.glob(path2files)
    flist.sort()

    ymdh=[]
    mae_omf=[]
    mae_oma=[]
    bias_omf=[]
    bias_oma=[]
    nobs=[]

    for fname in flist:
        exp_stats = OdaStats(fname=fname)
        if (plot_type=='time_series'):
            d, omf_tmp, oma_tmp, bomf, boma, n = exp_stats.mae(obstype=obstype,region=region)
            if np.isfinite(omf_tmp):
                ymdh.append(d)
                mae_omf.append(omf_tmp)
                mae_oma.append(oma_tmp)
                bias_omf.append(bomf)
                bias_oma.append(boma)
                nobs.append(n)
            print exp_stats.date

    fig = plt.figure(num=1, figsize=(14,8), facecolor='w')
    if (plot_type=='time_series'):

        ax1 = fig.add_subplot(211)
        ax2=ax1.twinx()
        ax3 = fig.add_subplot(212)
        ax4=ax3.twinx()

        print mae_omf

        #MAE        
        ax1.plot_date(ymdh, smooth(mae_omf,window_len=window_len),'o-',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        ax1.plot_date(ymdh, smooth(mae_oma,window_len=window_len),'o--',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        ax2.plot_date(ymdh, nobs,'-',color=COLORS[cnt_exp],lw=1,alpha=0.1)
        ax1.grid(True)

        #BIAS
        ax3.plot_date(ymdh, smooth(bias_omf,window_len=window_len),'-',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        ax3.plot_date(ymdh, smooth(bias_oma,window_len=window_len),'--',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        ax3.grid(True)
        ax4.plot_date(ymdh, nobs,'-',color=COLORS[cnt_exp],lw=1,alpha=0.01)

    else:
        plt.subplot(2,2,cnt_exp+1)
        plt.plot(exp_stats.obs,exp_stats.obs-exp_stats.omf,'.',color=COLORS[cnt_exp],alpha=0.1,markeredgecolor = 'none')
        plt.grid(True)
    #ax1.set_title()
    cnt_exp+=1

fig.autofmt_xdate()
plt.show()

'''
lon(nobs) ;
float lat(nobs) ;
float lev(nobs) ;
float obs(nobs) ;
float sigo(nobs) ;
float omf(nobs) ;
float std_omf(nobs) ;
float oma(nobs) ;
float std_oma(nobs) ;
'''

from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
import glob
import struct
import time
import sys
import getopt
import string
from datetime import date
import datetime

obsid_dict = {5525:'SST', 5522:'SSS', 5351:'ADT', 3073: 'T prof', 5521: 'S prof', 6000:'Ice Fraction'} 

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

        #print self.fcst[self.fcst==0.0]
        #raw_input('<>?')

    def plot(self, obstype='ADT'):
        IDS=np.unique(self.OBSID)
        plt.interactive(True)
        fig = plt.figure(num=1, figsize=(16,14), facecolor='w')

        for ids in IDS:
            #I = np.where( (self.OBSID==ids) & (self.qc==1) & (np.abs(self.lat)<90.0) & (np.abs(self.lev)<300.0) )            
            #I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<2000.0) & (np.abs(self.lev)>0.0) & (np.abs(self.omf)<100.0) & (np.abs(self.lat)<90.0) )
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<300.0) & (self.fcst!=0.0) & (np.abs(self.lat)<90.0) & (np.abs(self.omf)<10.0))
            if ( (ids == 3073) | (ids == 5521) ):
                if ids==3073:
                    vmin=-3.
                    vmax=3.            

                    #ax = fig.add_subplot(411)
                    #ax2=ax.twinx()
                    plt.subplot(611)
                    
                    plt.plot_date(self.date, np.mean(np.abs(self.omf[I])),'ok',markersize=8, alpha=self.alpha, marker=self.marker)
                    plt.plot_date(self.date, np.mean(np.abs(self.oma[I])),'or',markersize=8, alpha=self.alpha, marker=self.marker)

                    #plt.plot_date(self.date, np.sqrt(np.mean((self.omf[I])**2)),'k',markersize=8, alpha=self.alpha, marker=self.marker)
                    #plt.plot_date(self.date, np.mean(np.abs(self.oma[I])),'r',markersize=8, alpha=self.alpha, marker=self.marker)
                    #ax2.plot_date(self.date, len(I[0]),marker='.',color='gray',markersize=8)

                    plt.grid(True)
                    #ax2.grid(False)
                    #ax2.locator_params(axis='y',nbins=6)

                    print 'Nobs=',len(I[0])

                    #plt.grid(True)

                    #plt.subplot(812)
                    #plt.plot_date(self.date, np.mean(self.omf[I]),'ok',markersize=8)
                    #plt.grid(True)

                if ids==5521:
                    vmin=-1
                    vmax=1

                    plt.subplot(612)
                    plt.plot_date(self.date, np.mean(np.abs(self.omf[I])),'ok',markersize=8, alpha=self.alpha, marker=self.marker)
                    plt.plot_date(self.date, np.mean(np.abs(self.oma[I])),'or',markersize=8, alpha=self.alpha, marker=self.marker)
                    plt.grid(True)

                    #plt.subplot(814)
                    #plt.plot_date(self.date, np.mean(self.omf[I]),'ok',markersize=8)
                    #plt.grid(True)

                #plt.plot(self.omf[I], -self.lev[I],'.k',alpha=0.2)
                #plt.xlim((vmin,vmax))
                #plt.ylim((-3000,0))

            if (ids == 5525):
                
                if ids==5525:
                    vmin=-1.
                    vmax=1.
                if ids==5351:
                    vmin=-.2
                    vmax=.2

                plt.subplot(613)
                plt.plot_date(self.date, np.mean(np.abs(self.omf[I])),'ok',markersize=8, alpha=self.alpha, marker=self.marker)
                plt.plot_date(self.date, np.mean(np.abs(self.oma[I])),'or',markersize=8, alpha=self.alpha, marker=self.marker)
                plt.grid(True)
                plt.title(obsid_dict[ids],fontweight='bold')
                #plt.subplot(816)
                #plt.plot_date(self.date, np.mean(self.omf[I]),'ok',markersize=8)
                #plt.grid(True)


            if (ids == 5351):   
                

                plt.subplot(614)
                plt.plot_date(self.date, np.mean(np.abs(self.omf[I])),'ok',markersize=8, alpha=self.alpha, marker=self.marker)
                plt.plot_date(self.date, np.mean(np.abs(self.oma[I])),'or',markersize=8, alpha=self.alpha, marker=self.marker)
                plt.grid(True)
                plt.title(obsid_dict[ids],fontweight='bold')

            if (ids == 6000):   
                IN = np.where( (self.OBSID==ids) & (self.lat>55.0) )#& (np.abs(self.lev)<2000.0) & (self.fcst!=0.0) & (self.lat>55.0) & (np.abs(self.omf)<40.0))
                plt.subplot(615)
                plt.plot_date(self.date, np.mean(np.abs(self.omf[IN])),'ok',markersize=8, alpha=self.alpha, marker=self.marker)
                plt.plot_date(self.date, np.mean(np.abs(self.oma[IN])),'or',markersize=8, alpha=self.alpha, marker=self.marker)

                plt.grid(True)
                plt.title('Arctic '+obsid_dict[ids],fontweight='bold')                

                IS = np.where( (self.OBSID==ids) & (self.lat<-55.0) )#& (np.abs(self.lev)<2000.0) & (self.fcst!=0.0) & (self.lat<-55.0) & (np.abs(self.omf)<40.0))
                plt.subplot(616)
                plt.plot_date(self.date, np.mean(np.abs(self.omf[IS])),'ok',markersize=8, alpha=self.alpha, marker=self.marker)
                plt.plot_date(self.date, np.mean(np.abs(self.oma[IS])),'or',markersize=8, alpha=self.alpha, marker=self.marker)


                plt.grid(True)
                plt.title('Antarctic '+obsid_dict[ids],fontweight='bold')

                #plt.subplot(818)
                #plt.plot_date(self.date, np.mean(self.omf[I]),'ok',markersize=8)
                #plt.grid(True)
                '''
                map = Basemap(projection='mill', llcrnrlon=20, llcrnrlat=-70, urcrnrlon=380, urcrnrlat=80, resolution='l')
                map.drawcoastlines()
                map.drawcountries()
                #map.bluemarble()
                map.fillcontinents(color='coral')
                map.drawmapboundary()
                x, y = map(self.lon[I],self.lat[I])
                map.scatter(x,y,5,c=self.omf[I],cmap=cm.spectral,vmin=vmin,vmax=vmax,edgecolor=None,lw=0)                
                x, y = map(self.lon[I]+360,self.lat[I])
                map.scatter(x,y,5,c=self.omf[I],cmap=cm.spectral,vmin=vmin,vmax=vmax,edgecolor=None,lw=0)                
                plt.colorbar(shrink=.5, pad=0.05)
                '''
            #print obsid_dict[ids],' mean omf=',np.mean(self.omf[I]),' mean |omf|=',np.mean(np.abs(self.omf[I])),' mean std omf=',np.mean(np.abs(self.stdomf[I]))
            #plt.plot(self.lat[I],self.omf[I],'.k')
            #plt.title(self.yyyymmdd+' '+obsid_dict[ids])
            #fig.autofmt_xdate()                    
            
            #plt.savefig(self.yyyymmdd+'-'+obsid_dict[ids])
            #plt.draw()
            #raw_input('<>?')
            #plt.clf()

    def plot2dpolar(self, obstype='Ice Fraction', obsid=2819):
        I = np.where( (self.OBSID==obsid) )
        valmin=.0
        valmax=1.0
        errmax=0.5
        fig = plt.figure(num=1, figsize=(12,12), facecolor='w')        
        cnt=1
        for proj in ['npstere','spstere']:

            map = Basemap(projection=proj,lon_0=0,boundinglat=55, resolution='c')
            if (proj=='spstere'):
                map = Basemap(projection=proj,lon_0=0,boundinglat=-55, resolution='c')


            fcst=np.exp(self.obs[I]-self.omf[I])/(1.0+np.exp(self.obs[I]-self.omf[I]))
            obs=np.exp(self.obs[I])/(1.0+np.exp(self.obs[I]))

            #fcst[fcst<0.15]=np.nan
            #obs[obs<0.15]=np.nan

            plt.subplot(2,2,cnt)
            map.drawcoastlines()
            map.drawcountries()
            map.fillcontinents(color='coral')
            map.drawmapboundary()
            x, y = map(self.lon[I], self.lat[I])               
            map.scatter(x, y, 1, c=fcst,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
            x, y = map(self.lon[I]+360, self.lat[I])               
            map.scatter(x, y, 1, c=fcst,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)

            plt.subplot(2,2,cnt+1)
            map.drawcoastlines()
            map.drawcountries()
            map.fillcontinents(color='coral')
            map.drawmapboundary()
            x, y = map(self.lon[I], self.lat[I])               
            map.scatter(x, y, 1, c=obs,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
            x, y = map(self.lon[I]+360, self.lat[I])               
            map.scatter(x, y, 1, c=obs,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
            cnt+=2

        plt.subplots_adjust(left=None, bottom=None, right=None, top=None,wspace=0.0, hspace=0.0)


        plt.suptitle(self.yyyymmdd)
        plt.draw()
        fnameout='AICE_'+self.yyyymmdd
        plt.savefig(fnameout)
        plt.clf()

    def plot2d(self, obstype='Ice Fraction', obsid=5525):
        I = np.where( (self.OBSID==obsid) )
        valmin=-1.0
        valmax=1.0
        errmax=0.5
        fig = plt.figure(num=1, figsize=(16,8), facecolor='w')        
        cnt=1

        map = Basemap(projection='mill', llcrnrlon=20, llcrnrlat=-80, urcrnrlon=380, urcrnrlat=90, resolution='l')

        #fcst=self.obs[I]-self.omf[I]
        #obs=self.obs[I]

        #fcst[fcst<0.15]=np.nan
        #obs[obs<0.15]=np.nan

        #plt.subplot(2,1,1)
        map.drawcoastlines()
        map.drawcountries()
        map.fillcontinents(color='coral')
        map.drawmapboundary()
        x, y = map(self.lon[I], self.lat[I])               
        map.scatter(x, y, 2, c=self.omf[I],cmap=cm.bwr,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
        x, y = map(self.lon[I]+360, self.lat[I])               
        map.scatter(x, y, 2, c=self.omf[I],cmap=cm.bwr,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)

        #plt.subplot(2,1,2)
        #map.drawcoastlines()
        #map.drawcountries()
        #map.fillcontinents(color='coral')
        #map.drawmapboundary()
        #x, y = map(self.lon[I], self.lat[I])               
        #map.scatter(x, y, 1, c=obs,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
        #x, y = map(self.lon[I]+360, self.lat[I])               
        #map.scatter(x, y, 1, c=obs,cmap=cm.spectral,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)

        plt.colorbar(shrink=0.15)  

        plt.suptitle(self.yyyymmdd)
        plt.draw()
        fnameout='SST_'+self.yyyymmdd
        plt.savefig(fnameout)
        plt.clf()

        





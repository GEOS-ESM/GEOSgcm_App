#! /usr/bin/env python
# Python version of the obsview GUI

import matplotlib as mpl
mpl.use('WXAgg')
mpl.interactive(False)
import pylab as pl
from pylab import get_current_fig_manager as gcfm
import wx
import numpy as np
import random
from mpl_toolkits.basemap import Basemap
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import sys
#from omf_from_ods import dictionaries
import matplotlib.cm as cm
#import utils
#import analysis
import scipy.interpolate as interp
import numpy.linalg as linalg

class INSTRUMENT:
    def __init__(self, name, kx, kt, zmin, zmax, color='k'):
        self.name = name
        self.kx = kx
        self.kt = kt
        self.zmin = zmin
        self.zmax = zmax
        self.color = color
    def display(self):
        print 'Name: ',self.name,
        print 'kx: ',self.kx
        print 'kt:',self.kt
        print 'zmin:',self.zmin
        print 'zmax:',self.zmax

class VAR_SPECS:
    def __init__(self, varid, cutoff, fignum, units):
        self.varid  = varid
        self.cutoff = cutoff
        self.fignum = fignum
        self.units  = units



def dictionaries():
    dict_inst = {'ADCP'         : INSTRUMENT(name='ADCP', kx=505, kt=np.array([103, 104]), zmin=0, zmax=3000),
                 'CURRENT_METER': INSTRUMENT(name='CURRENT_METER', kx=506, kt=np.array([103, 104]), zmin=0, zmax=3000),
                 'TAO'          : INSTRUMENT(name='TAO', kx=501, kt=np.array([101, 102]), zmin=0, zmax=700, color='r'),
                 'PIR'          : INSTRUMENT(name='PIR', kx=502, kt=np.array([101, 102]), zmin=0, zmax=700, color='r'),
                 'XBT'          : INSTRUMENT(name='XBT', kx=503, kt=np.array([101, 102]), zmin=0, zmax=2000),
                 'CTD'          : INSTRUMENT(name='CTD', kx=513, kt=np.array([101, 102]), zmin=0, zmax=2000),
                 'CTDm'         : INSTRUMENT(name='CTDm', kx=600, kt=np.array([101, 102]), zmin=0, zmax=2000),
                 'ARGO'         : INSTRUMENT(name='ARGO', kx=508, kt=np.array([101, 102]), zmin=0, zmax=2000, color='k'),
                 'AQUARIUS'     : INSTRUMENT(name='AQUARIUS', kx=522, kt=np.array([102]), zmin=0, zmax=10),
                 'AQUARIUS_D'   : INSTRUMENT(name='AQUARIUS', kx=522, kt=np.array([102]), zmin=0, zmax=10),
                 'RAMA'         : INSTRUMENT(name='RAMA', kx=504, kt=np.array([101, 102]), zmin=0, zmax=1000, color='r'),
                 'LEV'          : INSTRUMENT(name='Levitus', kx=509, kt=np.array([101, 102]), zmin=0, zmax=4000),
                 'LEV_SSS'      : INSTRUMENT(name='Levitus surface',kx=521, kt=np.array([102]), zmin=0, zmax=10),
                 'TP'           : INSTRUMENT(name='TopexPoseidon', kx=514, kt=np.array([105]), zmin=0, zmax=0),
                 'JS1'          : INSTRUMENT(name='Jason1', kx=515, kt=np.array([105]), zmin=0, zmax=0 ),
                 'JS2'          : INSTRUMENT(name='Jason2', kx=517, kt=np.array([105]), zmin=0, zmax=0 ),
                 'SST'          : INSTRUMENT(name='Reynolds', kx=516, kt=np.array([101]), zmin=0, zmax=10),
                 'OSTIA_SST'    : INSTRUMENT(name='OSTIA SST', kx=525, kt=np.array([101]), zmin=0, zmax=10),
                 'REYN_AICE'    : INSTRUMENT(name='Reynolds aice', kx=523, kt=np.array([106]), zmin=0, zmax=10),
                 'CMIP5SST'     : INSTRUMENT(name='CMIP5 SST', kx=520, kt=np.array([101]), zmin=0, zmax=10),
                 'NSIDC'        : INSTRUMENT(name='NSIDC', kx=518,kt=np.array([106]), zmin=0, zmax=0),
                 'PIOMAS'       : INSTRUMENT(name='NSIDC', kx=522, kt=np.array([107]), zmin=0, zmax=0),
                 'MODIS'        : INSTRUMENT(name='MODIS', kx=524, kt=np.array([108]), zmin=0, zmax=0 ) }

    dict_varspecs = {'T'           : VAR_SPECS(varid=101, cutoff= 999., fignum=1, units='^oC'),
                     'S'           : VAR_SPECS(varid=102, cutoff= 999., fignum=2, units='psu'),
                     'U'           : VAR_SPECS(varid=103, cutoff= 999., fignum=3, units='m/s'),
                     'V'           : VAR_SPECS(varid=104, cutoff= 999., fignum=4, units='m/s'),
                     'SSH'         : VAR_SPECS(varid=105, cutoff= 5., fignum=5, units='m'),
                     'aice'        : VAR_SPECS(varid=106, cutoff= 999., fignum=6, units=''),
                     'hice'        : VAR_SPECS(varid=107, cutoff= 999., fignum=7, units='m'),
                     'chlorophyll' : VAR_SPECS(varid=108, cutoff= 999., fignum=8, units='') }

    COLORS=['k','r','g','b','m','y']

    return dict_inst, dict_varspecs, COLORS

dict_inst, dict_varspecs, COLORS = dictionaries()

class ods:
    #Hard coded for Steve Penny's letkf output
    def __init__(self, odsfname):
        print 'name:',odsfname
        ncfile = Dataset(odsfname, 'r')
        omf = ncfile.variables['omf'][:]
        try:
            oma = ncfile.variables['oma'][:]
        except:
            oma = ncfile.variables['omf'][:]            
        I=np.where(abs(omf)<9999.9)
        self.omf=omf[I]        
        self.oma=oma[I]
        try:
            self.std_omf=ncfile.variables['std_omf'][:]
        except:
            self.std_omf=0.0*self.omf
        self.lon=ncfile.variables['lon'][I]
        self.lat=ncfile.variables['lat'][I]
        self.obs=ncfile.variables['obs'][I]
        self.obserror=ncfile.variables['sigo'][I]
        self.lev=-ncfile.variables['lev'][I]
        self.kt=ncfile.variables['OBSID'][I]

        self.kx=np.ones(np.shape(self.kt))
        self.kx[self.kt==3073]=508
        self.kx[self.kt==5521]=508
        self.kx[self.kt==5351]=515
        self.kx[self.kt==5525]=525
        self.kx[self.kt==2819]=518 #NSIDC ice fraction

        self.kt[self.kt==3073]=101 #T
        self.kt[self.kt==5521]=102 #S
        self.kt[self.kt==5351]=105 #SSH
        self.kt[self.kt==5525]=101 #SST
        self.kt[self.kt==2819]=106 #AICE
        
        self.tt=np.zeros(np.shape(self.kt))
        ncfile.close()

#def draw_map(lonl=20,lonr=380):
def draw_map(lonl=-280,lonr=80):

    map = Basemap(projection='moll', llcrnrlat=-90, urcrnrlat=90,llcrnrlon=-180, urcrnrlon=180, resolution='c', lon_0=-80)
    #proj='mill'
    #map = Basemap(projection=proj, llcrnrlon=lonl, llcrnrlat=-80, urcrnrlon=lonr, urcrnrlat=90, resolution='l')
    map.drawcoastlines()
    map.drawcountries()
    map.fillcontinents(color='coral')
    map.drawmapboundary(fill_color='c')
    dlon=40
    dlat=20
    map.drawmeridians(np.arange(lonl,lonr,dlon),labels=[0,0,0,1])
    map.drawparallels(np.arange(-90,90,dlat),labels=[1,0,0,0])

    return map

def find_inst(KX):
    for instrument in dict_inst:
        if KX==dict_inst[instrument].kx:
            inst_name=dict_inst[instrument].name
    return inst_name

def find_var(KT):
    for var in dict_varspecs:
        if KT==dict_varspecs[var].varid:
            var_name=var
    return var_name

class ObsSpace(object):
    def __init__(self,odsfname,i=[]):

        print '============================================'
        print '=== Mouse left click: Profiles '
        print '=== Mouse right click: Horizontal scatter '
        print '============================================'

        self.ods=ods(odsfname)
        self.odsfname=odsfname
        self.fignum = 2
        self.figure = pl.figure(num=1, figsize=(18, 10), facecolor='c')   
        self.axis = self.figure.add_subplot(111)
        self.tooltip = wx.ToolTip(tip='tip with a long %s line and a newline\n' % (' '*100))
        gcfm().canvas.SetToolTip(self.tooltip)
        self.tooltip.Enable(False)
        self.tooltip.SetDelay(0)
        self.figure.canvas.mpl_connect('motion_notify_event', self._onMotion)
        self.figure.canvas.mpl_connect('button_press_event', self._onClick)
        self.dataX = np.squeeze(self.ods.lon)
        self.dataY = np.squeeze(self.ods.lat)

        map0=draw_map()
        x, y =map0(self.dataX,self.dataY) 
        self.X=x
        self.Y=y

        kxi=0
        for KX in np.unique(self.ods.kx):
            I=np.where(self.ods.kx==KX)
            if ( (KX==507) | (KX==522) | (KX==523)| (KX==507) |  (KX==514) | (KX==515) | (KX==517) | (KX==516) | (KX==520) | (KX==518) | (KX==521)| (KX==509) |(KX==525) ):
                STD=np.std(self.ods.omf[I])
                #self.figure.add_subplot(211)
                sh=self.axis.scatter(x[I], y[I], .5, c=self.ods.omf[I], cmap=cm.bwr,vmin=-STD,vmax=STD,edgecolor=None,lw=0)
                #self.figure.add_subplot(212)
                #self.axis.scatter(x[kx==KX], y[kx==KX], 5, c=oma[kx==KX], cmap=cm.bwr,vmin=-2*STD,vmax=2*STD,edgecolor=None,lw=0)
                #plt.colorbar(sh,orientation='horizontal',extend='both',shrink=0.3)
            else:
                self.axis.plot(x[I], y[I], linestyle='None', marker='.', markersize=10, label='myplot',color=COLORS[kxi])
            kxi+=1

    def _onMotion(self, event):
        collisionFound = False
        if event.xdata != None and event.ydata != None: # mouse is inside the axes
            for i in xrange(len(self.X)):
                radius = 200000
                if (abs(event.xdata - self.X[i]) < radius) and (abs(event.ydata - self.Y[i]) < radius):
                    inst_name = find_inst(self.ods.kx[i])
                    var_name = find_var(self.ods.kt[i])
                    top = tip='Lon=%f\nLat=%f\nInstrument: %s\nVar: %s' % (self.dataX[i], self.dataY[i], inst_name, var_name)
                    self.tooltip.SetTip(tip) 
                    self.tooltip.Enable(True)
                    self.i=i
                    collisionFound = True    
                    break
        if not collisionFound:
            self.tooltip.Enable(False)

    def _onClick(self, event):
        print 'click ',self.i 

        if event.button == 1:
            print 'Left click'
            self.figure2 = plt.figure(num=self.fignum, figsize=(16, 12), facecolor='c')   
            self.axis2 = self.figure2.add_axes([0.3,0.69,0.4,0.3])
            map=draw_map()
            I=np.where( (self.ods.lon==self.dataX[self.i]) & (self.ods.lat==self.dataY[self.i]) )
            ikx=0
            for KX in np.unique(self.ods.kx):
                Ikx=np.where(self.ods.kx==KX)
                for shift in [0, 360]:
                    x, y =map(self.dataX+shift,self.dataY)
                    self.axis2.plot(x[Ikx], y[Ikx], linestyle='None', marker='.', markersize=2, label='myplot',color=COLORS[ikx])
                    self.axis2.plot(x[self.i], y[self.i], linestyle='None', marker='.', markersize=10, label='myplot', color='k')
                ikx+=1

            inst_name = find_inst(self.ods.kx[self.i])
            var_name = find_var(self.ods.kt[self.i])
            self.axis3 = self.figure2.add_axes([0.1,0.05,0.35,0.6])
            self.axis3.set_ylabel('Depth [m]') 
            #self.axis5 = self.axis3.twiny()
            self.axis4 = self.figure2.add_axes([0.55,0.05,0.35,0.6])
            z=self.ods.lev[I]
            time=self.ods.tt[I]
            fcst=self.ods.obs[I]-self.ods.omf[I]
            std_fcst=self.ods.std_omf[I]
            
            ana=self.ods.obs[I]-self.ods.oma[I]
            obsi=self.ods.obs[I]
            obserrori=self.ods.obserror[I]
            vartype=self.ods.kt[I]
            print 'time:',time

            fcst[np.abs(fcst)<1e-5]=np.nan

            for utime in np.unique(time):
                vars=np.unique(vartype)
                cntvars=0
                for uvar in vars: 
                    cntvars+=1
                    print 'uvar=',uvar
                                
                    II=np.where( (time == utime) & (vartype==uvar) )
                    uz=z[II]
                    ufcst=fcst[II]
                    uana=ana[II]
                    uobs=obsi[II]
                    uobserror=obserrori[II]
                
                    III = np.argsort(uz)
                    
                    if cntvars==1:
                        self.axis3.plot(ufcst[III], uz[III], '--g',lw=2)
                        self.axis3.plot(uana[III], uz[III], '--r',lw=2)
                        self.axis3.errorbar(uobs[III], uz[III], xerr=uobserror[III],fmt='--ob',markersize=5)
                        #self.axis3.plot(uobs[III], uz[III], '--ob',markersize=5)
                        self.axis3.set_xlabel(var_name) 
                        #self.axis4.errorbar(0*uobs[III], uz[III], xerr=uobserror[III],fmt='*b')
                        #pl.figure(num=5, figsize=(12, 10), facecolor='c')                           
                        #pl.plot(ufcst[III], uz[III], '--r',lw=2)
                        #.plot(uobs[III], uz[III], 'ob')
                    else:
                        self.axis4.plot(ufcst[III], uz[III], '--g',lw=2)
                        self.axis4.plot(uana[III], uz[III], '--r',lw=2)
                        #self.axis4.plot(uobs[III], uz[III], '--ob',markersize=5)
                        self.axis4.errorbar(uobs[III], uz[III], xerr=uobserror[III],fmt='--ob',markersize=5)
                        self.axis4.set_xlabel(var_name) 
                        #self.axis4.errorbar(0*uobs[III], uz[III], xerr=uobserror[III],fmt='*b')
                        #self.axis5.plot(ufcst[III], uz[III], '--g',lw=2)
                        #self.axis5.errorbar(uobs[III], uz[III], xerr=uobserror[III],fmt='og',markersize=5)


                self.axis3.grid(True)
                self.axis4.grid(True)
                #self.axis5 = self.figure2.add_axes([0.75,0.75,0.2,0.2],frameon=False)#, axisbg='g')
                #self.axis5.axis('off')
                strlon='Lon = '+str(self.dataX[self.i])
                strlat='Lat = '+str(self.dataY[self.i])
                self.axis3.text(0.05,0.95,inst_name+' '+strlon+' '+strlat,fontsize=24, fontweight='bold')
                #self.axis3.text(0.05,0.75,strlon,fontsize=24, fontweight='bold')
                #self.axis3.text(0.05,0.55,strlat,fontsize=24, fontweight='bold')
                self.fignum +=1

                plt.show()

        if event.button == 3:
            print 'Right click'
            for KX in np.unique(self.ods.kx):
                for instrument in dict_inst:
                    if KX==dict_inst[instrument].kx:
                        inst_name=dict_inst[instrument].name
                figure2 = plt.figure(num=self.fignum, figsize=(16, 12), facecolor='c')
                axis2 = figure2.add_subplot(211)
                map=draw_map()
                I=np.where(self.ods.kx==KX)
                STD=np.std(self.ods.omf[I])
                for shift in [0, 360]:
                    x, y =map(self.dataX[I]+shift,self.dataY[I])
                    sch = axis2.scatter(x, y, 1, c=self.ods.omf[I], cmap=cm.bwr,vmin=-1.0*STD,vmax=1.0*STD,edgecolor=None,lw=0)
                plt.colorbar(sch,orientation='vertical',extend='both',shrink=0.5)
                    #mpl.colorbar(sch)
                titlestr=inst_name+' OMF'
                plt.title(titlestr,fontsize=24,fontweight='bold')
                #self.fignum+=1

                axis2 = figure2.add_subplot(212)
                map=draw_map()
                I=np.where(self.ods.kx==KX)
                STD=np.std(self.ods.omf[I])
                for shift in [0, 360]:
                    x, y =map(self.dataX[I]+shift,self.dataY[I])
                    sch = axis2.scatter(x, y, 1, c=self.ods.oma[I], cmap=cm.bwr,vmin=-1.0*STD,vmax=1.0*STD,edgecolor=None,lw=0)
                plt.colorbar(sch,orientation='vertical',extend='both',shrink=0.5)
                    #mpl.colorbar(sch)
                titlestr=inst_name+' OMA'
                plt.title(titlestr,fontsize=24,fontweight='bold')
                #self.fignum+=1
                
                '''
                axis3 = figure2.add_subplot(212)
                map=draw_map()
                for shift in [0, 360]:
                    x, y =map(self.dataX[I]+shift,self.dataY[I])
                    #axis3.scatter(x, y, 5, c=self.ods.oma[I], cmap=cm.bwr,vmin=-2*STD,vmax=2*STD,edgecolor=None,lw=0)
                    #sch=axis3.scatter(x, y, 1, c=np.log(self.ods.std_omf[I]), cmap=cm.bwr,vmin=0,vmax=0.5*STD,edgecolor=None,lw=0)
                    sch=axis3.scatter(x, y, 1, c=np.log(self.ods.std_omf[I]), cmap=cm.jet,vmin=-6.,vmax=0.,edgecolor=None,lw=0)
                plt.colorbar(sch,orientation='vertical',extend='both',shrink=0.5)
                titlestr=inst_name+' STD OMF'
                plt.title(titlestr,fontsize=24,fontweight='bold')
                '''
                self.fignum+=1

            #ax4 = figure2.add_axes([0.15, 0.25, 0.025, 0.5])
            #norm = mpl.colors.Normalize(vmin=-2*STD,vmax=2*STD)
            #mpl.colorbar.ColorbarBase(ax4, cmap=cm.bwr,norm=norm,orientation='vertical',extend='both')
            plt.show()

fname=sys.argv[1]
example = ObsSpace(fname)
pl.show()


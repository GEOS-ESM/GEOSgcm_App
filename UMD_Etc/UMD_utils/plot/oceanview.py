#! /usr/bin/env python

import matplotlib as mpl
mpl.use('WXAgg')
mpl.interactive(False)
import pylab as pl
from pylab import get_current_fig_manager as gcfm
import wx
import numpy as np
from mpl_toolkits.basemap import Basemap
from netCDF4 import Dataset, MFDataset
import matplotlib.pyplot as plt
import sys
import glob
import matplotlib.cm as cm


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
                 'ARGO'         : INSTRUMENT(name='ARGO', kx=508, kt=np.array([101, 102]), zmin=0, zmax=2000),
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

    COLORS=['b','r','g','k','m','y']

    return dict_inst, dict_varspecs, COLORS




dict_inst, dict_varspecs, COLORS = dictionaries()

def obsid2kxkt(obsid):
    kx=np.zeros(np.shape(obsid))
    kt=np.zeros(np.shape(obsid))
    kt[obsid==3073] = 101 # T
    kt[obsid==5521] = 102 # S
    kt[obsid==5525] = 101 # SST
    kt[obsid==5351] = 105 # SSH
    kt[obsid==6000] = 106 # AICE

    kx[obsid==3073] = 508 # T
    kx[obsid==5521] = 508 # S
    kx[obsid==5525] = 520 # SST
    kx[obsid==5351] = 517 # SSH
    kx[obsid==6000] = 518 # AICE
    
    return kx, kt


def get_from_ods(ncfile, varname):
    varout=ncfile.variables[varname][:]
    return varout#np.reshape(varout, (np.shape(varout)[0]*np.shape(varout)[1]))

class ods:
    def __init__(self, odsfnames):
        flist=glob.glob(odsfnames)
        print odsfnames
        print flist
        self.lon=[]
        self.lat=[]
        self.oma=[]
        self.omf=[]
        self.obs=[]
        self.obserror=[]
        self.lev=[]
        self.kt=[]        
        self.kx=[]
        self.tt=[]
        for odsfname in flist:
            print 'name:',odsfname
        
            ncfile = Dataset(odsfname)
            omf = get_from_ods(ncfile,'omf')
            I=np.where(abs(omf)<9999.9)
            self.omf=np.append(omf[I],self.omf)
        
            dum=get_from_ods(ncfile,'lon'); self.lon = np.append(dum[I],self.lon)
            dum=get_from_ods(ncfile,'lat'); self.lat = np.append(dum[I],self.lat)
            dum=get_from_ods(ncfile,'oma'); self.oma = np.append(dum[I],self.oma)
            dum=get_from_ods(ncfile,'obs'); self.obs = np.append(dum[I],self.obs)
            dum=get_from_ods(ncfile,'sigo'); self.obserror = np.append(dum[I],self.obserror)
            dum=get_from_ods(ncfile,'lev'); self.lev = np.append(-dum[I],self.lev)
            dum=get_from_ods(ncfile,'OBSID'); 
        
            kx, kt = obsid2kxkt(dum)
            self.kx = np.append(kx, self.kx)
            self.kt = np.append(kt, self.kt)

            ncfile.close()
        self.tt = np.zeros(np.shape(self.kt))
                           
def draw_map(lonl=20,lonr=380):    
    LAT0=90
    proj='mill'
    map = Basemap(projection=proj, llcrnrlon=lonl, llcrnrlat=-LAT0, urcrnrlon=lonr, urcrnrlat=LAT0, resolution='l')
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

class observation_space(object):
    def __init__(self,odsfname,i=[]):
        print '============================================'
        print '=== Mouse left click: Profiles '
        print '=== Mouse right click: Horizontal scatter '
        print '============================================'

        self.ods=ods(odsfname)
        self.odsfname=odsfname
        self.fignum = 2
        self.figure = pl.figure(num=1, figsize=(18, 10))
        self.axis = self.figure.add_subplot(111)
        self.tooltip = wx.ToolTip(tip='tip with a long %s line and a newline\n' % (' '*100))
        gcfm().canvas.SetToolTip(self.tooltip)
        self.tooltip.Enable(False)
        self.tooltip.SetDelay(0)
        self.figure.canvas.mpl_connect('motion_notify_event', self._onMotion)
        self.figure.canvas.mpl_connect('button_press_event', self._onClick)
        self.dataX = np.squeeze(self.ods.lon)
        self.dataY = np.squeeze(self.ods.lat)

        map0=draw_map(lonl=-180,lonr=180)
        x, y =map0(self.dataX,self.dataY) 
        self.X=x
        self.Y=y

        kxi=0
        for KX in np.unique(self.ods.kx):
            I=np.where(self.ods.kx==KX)
            #if ( (KX==507) | (KX==522) | (KX==523)| (KX==507) |  (KX==514) | (KX==515) | (KX==517) | (KX==516) | (KX==520) | (KX==518) | (KX==521)| (KX==509) |(KX==525) ):
            if ( (KX==507) | (KX==522) | (KX==523)| (KX==507) |  (KX==514) | (KX==515) | (KX==517) | (KX==516) | (KX==518) | (KX==521)| (KX==509) |(KX==525) ):
                STD=np.std(self.ods.omf[I])
                #self.figure.add_subplot(211)

                self.axis.scatter(x[I], y[I], 5, c=self.ods.omf[I], cmap=cm.bwr,vmin=-2*STD,vmax=2*STD,edgecolor=None,lw=0)
                #self.figure.add_subplot(212)
                #self.axis.scatter(x[kx==KX], y[kx==KX], 5, c=oma[kx==KX], cmap=cm.bwr,vmin=-2*STD,vmax=2*STD,edgecolor=None,lw=0)
                #plt.colorbar(orientation='vertical',extend='both',shrink=0.7)
            elif (KX==520):
                pass
            else:
                self.axis.plot(x[I], y[I], linestyle='None', marker='.', markersize=3, label='myplot',color=COLORS[kxi])
            kxi+=1

    def _onMotion(self, event):
        collisionFound = False
        if event.xdata != None and event.ydata != None: # mouse is inside the axes            
            I=np.where( (self.ods.kx!=520)|(self.ods.kx!=517) )
            for i in I[0]:#xrange(len(self.X)):
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
            self.axis4 = self.figure2.add_axes([0.55,0.05,0.35,0.6])
            z=self.ods.lev[I]
            time=self.ods.tt[I]
            fcst=self.ods.obs[I]-self.ods.omf[I]
            ana=self.ods.obs[I]-self.ods.oma[I]
            obsi=self.ods.obs[I]
            obskt=self.ods.kt[I]
            obserrori=self.ods.obserror[I]

            print 'time:',time

            for ukt in np.unique(self.ods.kt):
                II=np.where( obskt == ukt )
                uz=z[II]
                ufcst=fcst[II]
                uana=ana[II]
                uobs=obsi[II]
                uobserror=obserrori[II]
                III = np.argsort(uz)
                self.axis3.plot(ufcst[III], uz[III], '-g',lw=2)
                self.axis3.plot(uana[III], uz[III], '-r',lw=2)
                self.axis3.errorbar(uobs[III], uz[III], xerr=uobserror[III],fmt='-b')

                self.axis4.plot(uobs[III]-ufcst[III], uz[III], '-g',lw=2)
                self.axis4.plot(uobs[III]-uana[III], uz[III], '-r',lw=2)
                self.axis4.plot(ufcst[III]-uana[III], uz[III], '-b',lw=2)
                self.axis4.errorbar(0*uobs[III], uz[III], xerr=uobserror[III],fmt='*b')
                
                self.axis3.set_xlabel(var_name) 
                self.axis4.set_xlabel(var_name) 
                self.axis3.grid(True)
                self.axis4.grid(True)
                self.axis5 = self.figure2.add_axes([0.75,0.75,0.2,0.2],frameon=False)#, axisbg='g')
                self.axis5.axis('off')
                strlon='Lon = '+str(self.dataX[self.i])
                strlat='Lat = '+str(self.dataY[self.i])
                self.axis5.text(0.05,0.95,inst_name,fontsize=24, fontweight='bold')
                self.axis5.text(0.05,0.75,strlon,fontsize=24, fontweight='bold')
                self.axis5.text(0.05,0.55,strlat,fontsize=24, fontweight='bold')
                self.fignum +=1

            plt.show()
        if event.button == 3:
            for KX in np.unique(self.ods.kx):
                for instrument in dict_inst:
                    if KX==dict_inst[instrument].kx:
                        inst_name=dict_inst[instrument].name
                if (KX!=520):
                    figure2 = plt.figure(num=self.fignum, figsize=(16, 12), facecolor='c')   
                    axis2 = figure2.add_subplot(211)
                    map=draw_map()
                    I=np.where(self.ods.kx==KX)
                    STD=np.std(self.ods.omf[I])
                    for shift in [0, 360]:
                        x, y =map(self.dataX[I]+shift,self.dataY[I])
                        axis2.scatter(x, y, 5, c=self.ods.omf[I], cmap=cm.bwr,vmin=-2*STD,vmax=2*STD,edgecolor=None,lw=0)
                    titlestr=inst_name+' OMF'
                    plt.title(titlestr,fontsize=24,fontweight='bold')
                #self.fignum+=1
                    axis3 = figure2.add_subplot(212)
                    map=draw_map()
                    for shift in [0, 360]:
                        x, y =map(self.dataX[I]+shift,self.dataY[I])
                        axis3.scatter(x, y, 5, c=self.ods.oma[I], cmap=cm.bwr,vmin=-2*STD,vmax=2*STD,edgecolor=None,lw=0)
                    titlestr=inst_name+' OMA'
                    plt.title(titlestr,fontsize=24,fontweight='bold')
                    self.fignum+=1

            ax4 = figure2.add_axes([0.15, 0.25, 0.025, 0.5])
            norm = mpl.colors.Normalize(vmin=-2*STD,vmax=2*STD)
            mpl.colorbar.ColorbarBase(ax4, cmap=cm.bwr,norm=norm,orientation='vertical',extend='both')
            plt.show()

fname=sys.argv[1]
example = observation_space(fname)
pl.show()


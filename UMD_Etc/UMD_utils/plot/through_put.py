#! /usr/bin/env python
# Ex:./compare_omf.py 3 control_1992 mvoi_1992_nosla mvoi_1992_all TAO 006 S 0 700
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset
from netCDF4 import MFDataset
from numpy import *
import glob
import datetime
import string
import time
import os
import subprocess
import operator
import sys

TODAY=datetime.date.today()    #(2014,01,29)

fig = plt.figure(num=1, figsize=(20,10), facecolor='w')
fig.autofmt_xdate()

for stream in ['letkf4']:

    FILELIST=glob.glob('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/GEOS-Yuri/exp_a1_o05/test-letkf4/gmao_letkf/wrkdir/oana-*_2/oda.out')
    color='k'
    FILELIST.sort()
    Nfile=len(FILELIST)

    Walltime=range(1200*Nfile+1)
    Modeltime=range(1200*Nfile+1)
    index=0
    for filename in FILELIST:
        #print filename
        '''
        if (stream=='A003' or stream=='A001' or stream=='B001' or stream=='B001_j' or stream=='T001' or stream=='A004'):
            #year=filename[-14:-10]
            #month=filename[-9:-7]
            #day=filename[-6:-4]

            year=filename[-14:-10]
            month=filename[-10:-8]
            day=filename[-8:-6]    
        else:
            try:
                year=filename[-8:-4]
                month=filename[-11:-9]
                day=str(1)
                test=datetime.date(int(year),int(month),int(day))
            except:
          '''
        year=filename[-18:-14]
        month=filename[-14:-12]
        day=filename[-12:-10]
        print filename
        print year,month,day

        filestats=os.stat(filename)
        temp=time.localtime(filestats.st_ctime)

        Walltime[index]=datetime.datetime(int(temp[0]),int(temp[1]),int(temp[2]),int(temp[3]),int(temp[4]),int(temp[5]))
        Modeltime[index]=datetime.date(int(year),int(month),int(day))
        index=index+1    

    index_run=index
    index0=index
    for index_dt in range(5,10):
        index=index0
        DT_model = Modeltime[index-1]-Modeltime[index-index_dt]
        DT_wall  = Walltime[index-1]-Walltime[index-index_dt]

        print stream,"    : ",DT_model," per ",DT_wall
        print Walltime[index-1]
        index=index-1

        test=True

        while (test):
            print "==================="
            print index," wall=",Walltime[index]," DT_wall=",DT_wall
            print Walltime[index]
            print DT_wall
            print "==================="

            Walltime[index+1] = Walltime[index] + DT_wall
            Modeltime[index+1] = Modeltime[index] + DT_model
            test=Modeltime[index]<TODAY
            index=index+1

        plt.plot_date(Modeltime[index_run+1:index-1],Walltime[index_run+1:index-1],linestyle='-',color=color,marker='',linewidth=0.5)
    print "+++++++++++++++"
    print Modeltime[0:index_run]


    #Walltime[index]=datetime.datetime(2011,4,15,0,0,0)
    #Modeltime[index]=datetime.date(2011,1,1)
    

    plt.plot_date(Modeltime[0:index_run],Walltime[0:index_run],linestyle='',color=color,marker='.')




fig.autofmt_xdate()        
plt.grid()
plt.xlabel('Simulation Date')
plt.ylabel('Wall Date')
plt.show()

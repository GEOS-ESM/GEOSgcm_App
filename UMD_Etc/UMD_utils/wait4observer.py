#! /usr/bin/env python
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title:
# *
# * Description: wait until all observers are done. Assumes history output of *.geosgcm_ocn2dT.*.nc4
# *              
# *              
# *              
# * @author: Guillaume Vernieres
# */
# 
# /////////////////////////////////////////////////////////////////////////
# Date: May 2015

import glob
import sys
import time
import os

JOB_NDAYS     = float(os.environ['JOB_NDAYS'])*24.0
OBSERVER_FREQ = float(os.environ['OBSERVER_FREQ'])
#Nobservers    = int(JOB_NDAYS/OBSERVER_FREQ)

flist = glob.glob('*.oletkf_ocn2d.*.nc4')
flist.sort()

Nobservers  = len(flist)

#yyyy = flist[-1][-18:-14]
#mm   = flist[-1][-14:-12]
#dd   = flist[-1][-12:-10]
#hh   = flist[-1][-9:-7]

#file2wait4 = 'OCN_OBS_'+yyyy+mm+dd+'_'+hh
file2wait4 = 'OCN_OBS_*'

Nobservers = len(flist)

print 'Waiting for ',file2wait4

print 'JOB_NDAYS     : '+str(JOB_NDAYS)
print 'OBSERVER_FREQ : '+str(OBSERVER_FREQ)
print 'Nobservers    :'+str(Nobservers)

f1=open('./wait4observer.out', 'w+')
f1.write('JOB_NDAYS     : '+str(JOB_NDAYS)+'\r')
f1.write('OBSERVER_FREQ : '+str(OBSERVER_FREQ)+'\r')
f1.write('Nobservers    :'+str(Nobservers)+'\r')

old_list = []
while True:
    flist = glob.glob(file2wait4)    
    #if (len(flist)==1):
    if (len(flist)==Nobservers):
        #f1.write('Done waiting for '+file2wait4+'\r')
        f1.write('Done waiting \r')
        break
    else:
        f1.write('Waiting for observers ... \r')
        time.sleep(1)
    f1.flush()


f1.close()


#! /usr/bin/env python
import numpy as np
import sys
import os
import f90nml
import subprocess
import glob
from netCDF4 import Dataset
import datetime
from merra2_sbc import get_sst_sic
sys.path.append('/usr/local/other/SLES11/mpi4py-1.3/lib/python/')
from mpi4py import MPI

num_procs = MPI.COMM_WORLD.Get_size()
my_rank = MPI.COMM_WORLD.Get_rank()
my_name = MPI.Get_processor_name()
sys.stdout.write("Process %d of %d on %s.\n"% (my_rank, num_procs, my_name) )

yyyy              = sys.argv[1] 
mm                = sys.argv[2] 
dd                = sys.argv[3] 
NDAYS = num_procs

date_IC = datetime.datetime(int(yyyy),int(mm),int(dd))
date_origin = date_IC - datetime.timedelta(0.5)

if (my_rank==0):
    print date_IC
    print date_origin

sst_date = date_origin + datetime.timedelta(float(my_rank))
sys.stdout.write("Process %d of %d on %s. date %s \n"% (my_rank, num_procs, my_name,sst_date) )
get_sst_sic(sst_date.year, sst_date.month, sst_date.day)
MPI.COMM_WORLD.barrier()

if (my_rank==0):
    flist=glob.glob('./sst_*_1200z.nc')
    flist.sort()
    f = open('ocean_sst.txt', 'w')
    f.write('days since '+date_origin.isoformat(' ')+'\n')    
    f.write(str(date_origin.day).zfill(2)+'-'+date_origin.strftime('%b').upper()+'-'+str(date_origin.year)+' 12:00:00\n')    
    f.write(str(len(flist))+'\n')
    sst_date = date_origin
    for n in range(len(flist)):
        sst_date = date_origin + datetime.timedelta(float(n))
        #f.write(sst_date.isoformat(' ')+'\n')
        dt=abs(sst_date-date_origin).total_seconds()/(24.0*3600.0)
        f.write(str(dt)+'\n')
        f.write(flist[n][2:]+'\n')

#  add similar file for SIC
    #flist=glob.glob('./sic_*_1200z.nc')
    flist=glob.glob('./AICE_*_1200z.nc')
    flist.sort()
    f = open('ocean_sic.txt', 'w')
    f.write('days since '+date_origin.isoformat(' ')+'\n')    
    f.write(str(date_origin.day).zfill(2)+'-'+date_origin.strftime('%b').upper()+'-'+str(date_origin.year)+' 12:00:00\n')    
    f.write(str(len(flist))+'\n')
    sic_date = date_origin
    for n in range(len(flist)):
        sic_date = date_origin + datetime.timedelta(float(n))
        #f.write(sst_date.isoformat(' ')+'\n')
        dt=abs(sic_date-date_origin).total_seconds()/(24.0*3600.0)
        f.write(str(dt)+'\n')
        f.write(flist[n][2:]+'\n')

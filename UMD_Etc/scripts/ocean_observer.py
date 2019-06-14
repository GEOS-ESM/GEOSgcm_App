#! /usr/bin/env python
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title: ocean_observer.py
# *
# * Description: Scheduler for ocean_observer.csh
# *              Monitors the scratch directory and submit an observer each time a 
# *              new ocn3dT file is created
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

def get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, GROUP, QOS, seq=1, inovation_type='omf'):

    # ANADIR = oana-yyyymmdd_seq

    yyyy = fname[-18:-14]
    mm = fname[-14:-12]
    dd = fname[-12:-10]
    hh = fname[-9:-7]

    job_name = 'ocnobs_'+yyyy+mm+dd+'_'+hh
    command='sbatch --time=1:00:00 -n '+str(Ne)+' --ntasks-per-node=27 --job-name='+job_name+\
                ' -o '+job_name+'.o '+\
                ' -e '+job_name+'.e '+\
                ' --qos='+QOS+' -A '+GROUP+' --partition=compute '+PATH_TO_OBSERVER+'/ocean_observer.csh '+\
                yyyy+' '+mm+' '+dd+' '+hh+' '+str(seq)+' '+inovation_type+' '+ANADIR

    return command, yyyy, mm, dd, hh

inovation_type = sys.argv[1]  # 'omf' or 'oma'
ANADIR         = sys.argv[2]
WAIT           = sys.argv[3]  # True: Wait for history to be written
                              # False: Assumes history is present
DA_seq         = sys.argv[4]

Ne = int(os.environ['ODAS_Ne'])
PATH_TO_OBSERVER = os.environ['UMD_LETKFSCRIPTS']
ODAS_group = os.environ['ODAS_group']
ODAS_qos = os.environ['ODAS_qos']
OCN3D =  os.environ['OCN3D']
OCN2D =  os.environ['OCN2D']

f1=open('./ocean_observer.out', 'w+')
nw=0
n = 0
old_list = []
if WAIT=='True':
    while True:
        nw+=1
        #new_list = glob.glob('*.'+geosgcm_ocn3dT.*.nc4')
        new_list = glob.glob('*.'+OCN3D+'.*.nc4')
        new_list.sort()
        if (len(new_list)>len(old_list)):
            f1.write('Sending observer on '+new_list[-1]+'\n')
            fname = new_list[-1]
            command, yyyy, mm, dd, hh = get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, ODAS_group, ODAS_qos, seq=DA_seq,inovation_type=inovation_type)
            f1.write(yyyy+mm+dd+hh)
            f1.write('========================='+'\n')
            os.system(command) 
            print command
            f1.write(command+'\n')
            n+=1
        else:
            f1.write('Waiting for new history ... '+str(nw)+'\r')
            time.sleep(1)
        old_list = new_list
        f1.flush()


else:
    flist = glob.glob('*.'+OCN2D+'.*.nc4') #('*.geosgcm_ocn2dT.*.nc4')
    print flist
    for fname in flist:
        f1.write('Sending observer on '+fname+'\n')
        command, yyyy, mm, dd, hh = get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, seq=DA_seq,inovation_type=inovation_type)
        f1.write(yyyy+mm+dd+hh)
        f1.write('========================='+'\n')
        os.system(command) 
        print command
        f1.write(command+'\n')
        n+=1

f1.close()

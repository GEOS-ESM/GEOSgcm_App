#! /usr/bin/env python
#
# Prepare restarts for stop-gap seasonal system
#
# INPUT: yyyy     4 digit year
#        mmmdd    5 character month (mmm) and day (dd)
#
# OUTPUT: get restarts from archive and prepare the directory to run the UMD OLETKK
#
# Usage: ./MO2M2O.py 2012 aug14
#        Should return the restarts from sub-seasonal for aug09 2012  
#
# Guillaume 03/2016
#

import sys
import glob
import numpy as np
import os
import datetime
import os.path
import time
import ntpath
import calendar

IC_mmmdd = np.array(
           [
            'jan01', 'jan06', 'jan11', 'jan16', 'jan21', 'jan26', 'jan31',
            'feb05', 'feb10', 'feb15', 'feb20', 'feb25',
            'mar02', 'mar07', 'mar12', 'mar17', 'mar22', 'mar27',
            'apr01', 'apr06', 'apr11', 'apr16', 'apr21', 'apr26',
            'may01', 'may06', 'may11', 'may16', 'may21', 'may26', 'may31/',
            'jun05', 'jun10', 'jun15', 'jun20', 'jun25', 'jun30',
            'jul05', 'jul10', 'jul15', 'jul20', 'jul25', 'jul30',
            'aug04', 'aug09', 'aug14', 'aug19', 'aug24', 'aug29',
            'sep03', 'sep08', 'sep13', 'sep18', 'sep23', 'sep28',
            'oct03', 'oct08', 'oct13', 'oct18', 'oct23', 'oct28',
            'nov02', 'nov07', 'nov12', 'nov17', 'nov22', 'nov27',
            'dec02', 'dec07', 'dec12', 'dec17', 'dec22', 'dec27'
            ]
           )

suffix = 'Heracles-5_4_p1.a180x1080_o720x410_CF0180x6C_DE0360xPE0180'

#vegdyn = '/discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2/OutData/vegdyn_internal_rst'
# /bin/ln -sf /discover/nobackup/projects/gmao/t2ssp/h54/c180_o05/restart/MERRA2/OutData/vegdyn_internal_rst vegdyn.data

def mmmdd2mmdd(mmmdd):
    month=np.array(['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'])
    
    return str(np.where(month==mmmdd[0:3])[0][0]+1).zfill(2), mmmdd[-2:]


class G5Init:

    def __init__(self):
        self.yyyy = sys.argv[1]    # year
        self.mmmdd = sys.argv[2]   # Create restarts for mmmdd <--- Cycle letkf from previous restart date 

        self.rstyyyy = self.yyyy 
        if self.mmmdd == 'jan01':
	   self.rstyyyy = str(int(self.yyyy)-1)

        self.run_dir_yyyy  = 'Y'+self.yyyy
        self.run_dir_mmmdd = self.mmmdd
        self.expdir = self.run_dir_yyyy+'/'+self.run_dir_mmmdd
        self.subic = self.expdir+'/SUB-SEAS-IC/'
        self.script_name = 'get_subseas_rst_'+self.rstyyyy+self.mmmdd+'.csh'
        self.job_done = 'S2S_RST_READY_'+self.yyyy+self.mmmdd
        command = 'mkdir '+self.run_dir_yyyy
        os.system(command)
        command = 'mkdir '+self.expdir
        os.system(command)
        command = 'mkdir '+self.subic
        os.system(command)
        
    def get_subseasonal_restarts(self):
        #
        # Grab restarts 5 days before yyyy mmmdd from the archive
        #
        
        # get index for previous IC (~5 day before depending on leap/no-leap)
        I=np.where(IC_mmmdd == self.mmmdd)[0][0]  
        print 'getting ',IC_mmmdd[I-1],' restarts from sub-seasonal to prepare cycling to ',IC_mmmdd[I],' ',self.yyyy

        mmic, ddic = mmmdd2mmdd(IC_mmmdd[I-1]) # Converts mmmdd to mmdd


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Instead of going to archive for old subseasonal restarts, get new files from new location

#        path2rst = '/archive/u/gmaofcst/G5FCST/subseasonal/Heracles/Y'+self.yyyy+'/'+IC_mmmdd[I-1]+'/ens1/restarts.e'+self.yyyy+mmic+ddic+'_00z.tar'
#
#        command='rm '+self.job_done                                                      # Make sure the file indicating the job is done is not present
#        os.system(command)
#        self.write_get_rst(fname=path2rst)                                               # Write a script with the proper argument to de-archive restarts
#
#        #command = 'sbatch get_subseas_rst.csh'                                          # Submit to datamove
#        command = 'sbatch '+self.script_name                                          # Submit to datamove
#        os.system(command)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#       path2rst = '/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/restart/REINIT/restarts.e'+self.yyyy+mmic+ddic+'_00z.tar'
#       path2rst = '/gpfsm/dnb42/projects/p17/bzhao/geos5/exp/restart/restart/REINIT/restarts.e'+self.yyyy+mmic+ddic+'_00z.tar'
        path2rst = '/gpfsm/dnb42/projects/p17/zhaoli/GEOS-S2S/restart/REINIT/'+self.yyyy+'/restarts.e'+self.rstyyyy+mmic+ddic+'_00z.tar'
        command = 'cp '+path2rst+' .'
        print command
        os.system(command) 
#        self.wait_for_rst()                                                              # Wait for de-archiving process to be done (~20sec)
        head, tail = ntpath.split(path2rst)
        self.path2rst=glob.glob(tail)[0]       

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

        command = 'cp '+self.path2rst+' ./'+self.run_dir_yyyy+'/'+self.run_dir_mmmdd
        print command
        os.system(command)   
        command = 'tar xvf '+self.path2rst+' -C '+self.subic
        os.system(command)

        self.cap_restart = self.path2rst[-16:-8]+' '+self.path2rst[-7:-5]+'0000'
        
        print 'cap_restart:',self.cap_restart

        yyyy = self.cap_restart[0:4]
        mm = self.cap_restart[4:6]
        dd = self.cap_restart[6:8]
        hh = self.cap_restart[10:12]

        self.start_date   = datetime.datetime(int(yyyy), int(mm), int(dd), int(hh))

        mm, dd = mmmdd2mmdd(self.mmmdd)
        
        ########## DUMP RESTARTS AT 21Z ###########
        self.end_date     = datetime.datetime(int(self.yyyy), int(mm), int(dd), int(hh)) - datetime.timedelta(hours=3)

        print 'START DATE:',self.start_date        
        print 'END DATE:',str(self.end_date.year), str(self.end_date.month).zfill(2), str(self.end_date.day).zfill(2), str(self.end_date.hour).zfill(2)

        self.preffix = IC_mmmdd[I-1]
        self.suffix = suffix

        
    def strip_rst_name(self):
        
        flist = glob.glob(self.subic+'/?????.*_rst.*')
        
        for fname in flist:
            print fname
            print fname[30:-len(self.suffix)-1-14]
            print '========================================='
            command = 'mv '+fname+' '+self.expdir+'/'+fname[30:-len(self.suffix)-1-14]
            os.system(command)            

        command = 'mv '+self.subic+'/RESTART.* '+self.expdir+'/RESTART'
        os.system(command)

    def wait_for_rst(self): #, mmmdd):
        
        while True:
            if (os.path.isfile(self.job_done)):
                print 'Done waiting'
                break
            else:
                print 'Waiting for restarts ... '+self.job_done
                time.sleep(1)
    
    def write_get_rst(self, fname): #, mmmdd):
        
        ODAS_group = os.environ['ODAS_group']
        #text_file = open(self.expdir+'/get_subseas_rst.csh', 'w')
        text_file = open(self.script_name, 'w')
        text_file.write('#!/bin/csh \n')
        text_file.write('#SBATCH --time=02:00:00 \n')
        text_file.write('#SBATCH --job-name oe \n')
        text_file.write('#SBATCH -p datamove \n')
#       text_file.write('#SBATCH --account=g0609 \n')
        text_file.write('#SBATCH --account='+ODAS_group+' \n')
        text_file.write('rm '+self.job_done+' \n')
        text_file.write('set fname = `echo ls '+fname+'` \n')
        text_file.write('cp $fname . \n')
        text_file.write('touch '+self.job_done+' \n')
        text_file.close()

    def write_cap(self):
        
        text_file = open(self.expdir+'/cap_restart', 'w')
        text_file.write(self.cap_restart+' \n')
        text_file.close()

        text_file = open(self.expdir+'/CAP.rc', 'w')
        text_file.write('MAPLROOT_COMPNAME: GCS \n')
        text_file.write('ROOT_NAME: GCS \n')
        text_file.write('ROOT_CF: AGCM.rc \n')
        text_file.write('HIST_CF: HISTORY.rc \n')
        text_file.write('BEG_DATE:     '+self.cap_restart+' \n')
        text_file.write('END_DATE:     '+str(self.end_date.year)+str(self.end_date.month).zfill(2)+str(self.end_date.day).zfill(2)+' '+str(self.end_date.hour).zfill(2)+'0000 \n')
        if(self.mmmdd == 'mar02'):
                if(calendar.isleap(int(self.yyyy))):
                        print 'IS LEAP YEAR',
                        text_file.write('JOB_SGMT:     00000006 000000 \n')
		else:
			print 'IS NOT LEAP YEAR'
                        text_file.write('JOB_SGMT:     00000005 000000 \n')
        else:
                print 'KEEP 5 DAY ODAS CYCLE',
                text_file.write('JOB_SGMT:     00000005 000000 \n')
        text_file.write('NUM_SGMT:     1 \n')
        text_file.write('HEARTBEAT_DT:       450 \n')
        text_file.write('MAPL_ENABLE_TIMERS: YES \n')
        text_file.write('MAPL_ENABLE_MEMUTILS: NO \n')
        text_file.write('PRINTSPEC: 0 \n')
        text_file.close()

#
# Usage: MOtoM2O.py yyyy mmmdd  
# Example:  MOtoM2O.py 2012 aug14   --> Will re-initialize aug14 2012 
#                                       cycling with the ocean letkf 
#                                       starting from aug09 2012
#

g5init = G5Init()
g5init.get_subseasonal_restarts()   # Get the coupled restarts from the subseasonal experiments (limited to 1999-2015)
g5init.strip_rst_name()             # Remove preffix and suffix
g5init.write_cap()                  # Write cap_restart and CAP.rc





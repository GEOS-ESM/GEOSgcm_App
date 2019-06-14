#! /usr/bin/env python
#
# Creates namelist for oletkf according to the odas configuration in ocean_das_config
#
# Example: ./ocean_letkf_nml.py -c T <--- Create namelist with sea-ice specific localization length scale
#          ./ocean_letkf_nml.py      <--- Create namelist for ocean assimilation

import sys
import os
import f90nml
import getopt

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1", "True", ".true.", "T")

sea_ice_da = False

options, remainder = getopt.getopt(sys.argv[1:],'c:')

for opt, arg in options:
  if opt in ('-c'):
    sea_ice_da =  str2bool(arg)

# Get template namelist for oletkf
ODAS_RC        = os.environ['ODAS_RC']
fname_template = ODAS_RC+'OLETKF/input_prof.nml'

# Copy template namelist in work directory as input.nml  
command = 'cp '+fname_template+' input.nml'
os.system(command)

# Default is unactive observing system
ACTIVE_Tprof = False 
ACTIVE_Sprof = False 
ACTIVE_ADT   = False 
ACTIVE_SST   = False
ACTIVE_SSS   = False
ACTIVE_HICE  = False

# Update parameters according to ocean_das_config
if sea_ice_da:
  print 'SEA-ICE DA'
  sigma_obs    = os.environ['ODAS_loc_max_cice']
  sigma_obs0   = os.environ['ODAS_loc_min_cice']
  ACTIVE_AICE = True
else:
  print 'NO SEA-ICE DA'
  sigma_obs    = os.environ['ODAS_loc_max']
  sigma_obs0   = os.environ['ODAS_loc_min']
  ACTIVE_AICE = False
  ACTIVE_Tprof =  str2bool(os.environ['ODAS_ACTIVE_Tprof'])
  ACTIVE_Sprof =  str2bool(os.environ['ODAS_ACTIVE_Sprof'])
  ACTIVE_ADT   =  str2bool(os.environ['ODAS_ACTIVE_ADT'])
  ACTIVE_SST   =  str2bool(os.environ['ODAS_ACTIVE_SST'])
  ACTIVE_SSS   =  str2bool(os.environ['ODAS_ACTIVE_SSS'])
  ACTIVE_HICE  =  str2bool(os.environ['ODAS_ACTIVE_HICE'])

nml = f90nml.read('input.nml')

#Edit params_obs namelist
nml['params_obs_nml']['sigma_obs']  = float(sigma_obs)*1000.0   # [km to m]
nml['params_obs_nml']['sigma_obs0'] = float(sigma_obs0)*1000.0  # [km to m]

#Edit set_active_obs namelist
nml['set_active_obs_nml']['ACTIVE_Tprof'] =  ACTIVE_Tprof
nml['set_active_obs_nml']['ACTIVE_Sprof'] =  ACTIVE_Sprof
nml['set_active_obs_nml']['ACTIVE_ADT']   =  ACTIVE_ADT
nml['set_active_obs_nml']['ACTIVE_SST']   =  ACTIVE_SST
nml['set_active_obs_nml']['ACTIVE_SSS']   =  ACTIVE_SSS
nml['set_active_obs_nml']['ACTIVE_AICE']  =  ACTIVE_AICE
nml['set_active_obs_nml']['ACTIVE_HICE']  =  ACTIVE_HICE

nml.end_comma=True
nml.write('input.nml', force=True)



#!/usr/bin/env python

import argparse
import datetime
import os
import sys

def main():

   # Set founds to False
   # -------------------
    
   CMAP_found   = False
   anaeta_found = False
   rccode       = 0

   # Parse arguments
   # ---------------

   args    = parse_args()
   year    = args['year']
   month   = args['month']
   day     = args['day']
   verbose = args['verbose']

   # Create our input day datetime object
   # ------------------------------------

   inputday = datetime.datetime(year=year,month=month,day=day)

   # Check for CMAP file existence
   # -----------------------------

   CMAP_path   = "/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/hindcast_restarts/RESTART/"
   CMAP_suffix = "_2100z.ocean_sbc.res.nc"

   CMAP_target_time = inputday
   CMAP_target_time_string = CMAP_target_time.strftime("%Y%m%d")
   CMAP_filename = CMAP_target_time_string + CMAP_suffix
   CMAP_file = os.path.join(os.path.sep, CMAP_path, CMAP_filename)

   if verbose: print "Looking for", CMAP_file,
   print "Looking for", CMAP_file,

   if os.path.isfile(CMAP_file):
      if verbose: print "...Found!"
      print "...Found!"
      CMAP_found = True
   else:
      if verbose: print "...NOT FOUND"
      rccode += 1

   if verbose: print "rccode: ", str(rccode).zfill(3)

   if CMAP_found :
      sys.exit(0)
   else:
      sys.exit(rccode)


def parse_args():
    
   p = argparse.ArgumentParser(description='Date checker for S2S')

   requiredNamed = p.add_argument_group('required named arguments')

   requiredNamed.add_argument('--year',  type=int, help='Year',  default=None, required=True)
   requiredNamed.add_argument('--month', type=int, help='Month', default=None, required=True)
   requiredNamed.add_argument('--day',   type=int, help='Day',   default=None, required=True)

   p.add_argument('--verbose', help="Verbose", action='store_true')

   return vars(p.parse_args())

if __name__=="__main__":
   main()

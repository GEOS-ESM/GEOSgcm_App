#! /usr/bin/env python
import plot_utils as pu
import matplotlib.pyplot as plt
import sys 
import glob


plt.interactive(True)
#flist=glob.glob('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/GEOS-Yuri/GEOSodas5_2_beta/expa1o05/gv002/ocean_das/oana-*_2/ocean_observer_*/obs-????????_12.nc')
#flist=glob.glob('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/GEOS-Yuri/GEOSodas5_2_beta/expa1o05/gv002/ocean_das/oana-*_2/ocean_observer_*/obs-????????_12.nc')
flist=glob.glob('/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk015/ocean_das/oana-*_2/ocean_observer_*/obs-????????_12.nc')
#flist=glob.glob('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/GEOS-Yuri/GEOSodas5_2_beta/expa1o05/gv003/ocean_das/oana-20120112_2/obs-20120*_12.nc')
#flist=glob.glob('./obs-????????_??.nc')
flist.sort()
print flist

print 'flist:',flist
for fname in flist:
    testoda=pu.OdaStats(fname=fname)
    #testoda.plot2dpolar()
    testoda.plot2d()





#plt.savefig('omfstuffT')

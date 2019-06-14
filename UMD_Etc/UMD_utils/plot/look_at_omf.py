#! /usr/bin/env python
import plot_utils as pu
import matplotlib.pyplot as plt
import sys 
import glob
PATH1=sys.argv[1:]
#PATH2=sys.argv[2]

yyyy = '????'
#yyyy = '2013'
#yyyy = '199[2-3]'
mm   = '??'
#mm   = '11'
dd= '??'

alpha=.3
marker = 'o'
for PATH in PATH1:
    #flist=glob.glob(PATH+'/oana-*/ocean_observer_*/obs-????????_??.nc')
    flist=glob.glob(PATH+'/oana-*/ocean_observer_*/obs-'+yyyy+mm+dd+'_??.nc')
    flist.sort()
    print 'flist:',flist
    for fname in flist:
        testoda=pu.OdaStats(fname=fname, alpha=alpha, marker=marker)
        testoda.plot()
    alpha=1.0
    marker='.'
plt.interactive(False)
plt.show()





#!/usr/bin/env python

'''
plot ascending or descending orbit fields from RSS GMI data
see Readme_SA for details.

SA, Jul 2016
'''

import  sys

import  pylab              as    pylab
import  numpy              as    numpy
import  matplotlib.pyplot  as    plt
import  matplotlib.dates   as    md

from    mpl_toolkits.basemap import Basemap
from    datetime             import datetime, timedelta

from    gmi_daily_v8 import GMIdaily

iMissing        = -999.		# for rain, land, sea-ice. 
iGood		= 0.		# "good" pixel = 1
iBad		= 0.		# "bad"  pixel = 0
#----------------------------------------------------------------------------

def read_data(fName):

        dataset = GMIdaily(fName, missing= iMissing)    # -999. for missing values

        if not dataset.variables:
                sys.exit('problem reading file:\n[%s]'%(fName))

        return dataset
#----------------------------------------------------------------------------

def l2_gmi_rss(fName, date_, time_s, time_e):

	dataset	= read_data(fName)

        #----------------------------------------------------------------------------
	# grid related information
	lat_in = dataset.variables['latitude']	#  720 points- 0.25deg spaced
	lon_in = dataset.variables['longitude']	# 1440 points- 0.25deg spaced

	# make a regular grid of same dimension as time and sst
        lon, lat = numpy.meshgrid( lon_in, lat_in)

	lon = lon.flatten()
	lat = lat.flatten()

	# indices of ascending (0) and descending (1) passes
	iasc, idsc = [0, 1]        
	nx, ny 	   = [len(lon_in), len(lat_in)]
        #----------------------------------------------------------------------------

	# time related information
        time_asc = numpy.squeeze( dataset.variables['time'][iasc,:,:]).flatten()
        time_dsc = numpy.squeeze( dataset.variables['time'][idsc,:,:]).flatten()

	# SST related information-- yes! related, because satellites do not measure physical temperature!
	sst_asc  = numpy.squeeze( dataset.variables['sst'] [iasc,:,:]).flatten()
        sst_dsc  = numpy.squeeze( dataset.variables['sst'] [idsc,:,:]).flatten()
        #----------------------------------------------------------------------------

	asc_time	= numpy.empty_like( time_asc)
	dsc_time	= numpy.empty_like( time_dsc)

	date0 = date_ - timedelta(hours=date_.hour)	# start with 00UTC

	# add in the time info (hour and minute from file
	for iData in range(0, len(time_asc)):
		if ( time_asc[iData] == iMissing):
			asc_time[iData] = md.date2num( datetime( 1,1,1,0,0,0))
		else:
			hh_ = time_asc[iData]			# hour
			mm_ = (hh_ - numpy.int(hh_)) *60.	# minutes

			#print '%f\t%f'%(hh_, mm_)
			asc_time[iData] = md.date2num( date0 + \
				                       timedelta(hours  =numpy.int(hh_)) + \
				                       timedelta(minutes=numpy.int(mm_)))
			del hh_, mm_
        #----------------------------------------------------------------------------
	for iData in range(0, len(time_dsc)):
		if ( time_dsc[iData] == iMissing):
			dsc_time[iData] = md.date2num( datetime( 1,1,1,0,0,0))
		else:
			hh_ = time_dsc[iData]			# hour
			mm_ = (hh_ - numpy.int(hh_)) *60.	# minutes
			#print '%f\t%f'%(hh_, mm_)
			dsc_time[iData] = md.date2num( date0 + \
				                       timedelta(hours  =numpy.int(hh_)) + \
				                       timedelta(minutes=numpy.int(mm_)))
			del hh_, mm_
       	#----------------------------------------------------------------------------

	# select ASCENDING/DESCENDING data that is within my_time \pm my_tdelta
	iasc_data = [ (asc_time >= md.date2num(time_s)) & (asc_time <= md.date2num(time_e))]	
	idsc_data = [ (dsc_time >= md.date2num(time_s)) & (dsc_time <= md.date2num(time_e))]	

	if ( len(lat[iasc_data]) + len(lat[idsc_data]) ==0):
		print '%i'%(len(lat[iasc_data]))
		print '%i'%(len(lat[idsc_data]))
		sys.exit('Found no data in [%s] for the time requested: %s-\t%s'%(fName, time_s.strftime('%Y%m%d_%Hz'), time_e.strftime('%Y%m%d_%Hz')))

	# put all together
	lat_  = numpy.concatenate( (lat     [iasc_data],      lat[idsc_data]), axis=0)
	lon_  = numpy.concatenate( (lon     [iasc_data],      lon[idsc_data]), axis=0)
	time_ = numpy.concatenate( (asc_time[iasc_data], dsc_time[idsc_data]), axis=0)
	sst_  = numpy.concatenate( (sst_asc [iasc_data], sst_dsc [idsc_data]), axis=0)
	
	# qc flag, Good data = 1, Bad data = 0.
	qcf_  = numpy.ones_like(sst_)
	qcf_[ sst_== iMissing] = iBad

	II = numpy.where(qcf_ == 1)

	return lat_[II], lon_[II], time_[II], sst_[II], qcf_[II]
#----------------------------------------------------------------------------

def gather_l2_gmi_rss(yy_, mm_, dd_, hh_, NDAYS = 0.125, plot = False):
	
	#----------------------------------------------------------------------------
	data_path       = '/discover/nobackup/projects/gmao/advda/sakella/' + \
		          '/data/products/retrievals/RSS/GMI/'

	file_pref	= 'f35_'
	file_suff	= 'v8.1.gz'
	#----------------------------------------------------------------------------
	# processing date-- you would want to loop over yy_, mm_, dd_, hh_
	#yy_ 		= 2015		# year
	#mm_		= 7		# month    of above year
	#dd_		= 31   		# day      of above month
	#hh_             = 0             # UTC hour of above day

	proc_date	= datetime(yy_, mm_, dd_, hh_, 0, 0)

	# gather observations \pm 3 Hrs centered around proc_date
	NHOURS = NDAYS * 24.0   # 1/2 the window
	t_start		= proc_date - timedelta(hours=NHOURS)
	t_end		= proc_date + timedelta(hours=NHOURS)
	#----------------------------------------------------------------------------

	if ( hh_ !=0):	# 00UTC is handed separately	
		
		fName	= data_path + \
                          'Y%s/'%(proc_date.strftime('%Y'))    + 'M%s/'%(proc_date.strftime('%m')) + \
			  file_pref + proc_date.strftime('%Y%m%d') + file_suff
		print 'Gathering data from...\n[%s]'%(fName)

		[lat_, lon_, time_, sst_, qcf_] = l2_gmi_rss(fName, proc_date, t_start, t_end)
		sigo_ = 0.5 * numpy.ones(numpy.shape(sst_))		
	else:
		# need data from previous date
 		fName_a	= data_path + 'Y%s/'%(t_start.strftime('%Y'))   + 'M%s/'%(t_start.strftime('%m')) + \
 		          file_pref + t_start.strftime('%Y%m%d') + file_suff

		# and from proc_date
		fName_b	= data_path + 'Y%s/'%(proc_date.strftime('%Y')) + 'M%s/'%(proc_date.strftime('%m')) + \
			  file_pref + proc_date.strftime('%Y%m%d') + file_suff

 		print 'Gathering data from...\n[%s]\n[%s]'%(fName_a, fName_b)
 
 		[lat_a, lon_a, time_a, sst_a, qcf_a] = l2_gmi_rss(fName_a, t_start, t_start, proc_date)
 		[lat_b, lon_b, time_b, sst_b, qcf_b] = l2_gmi_rss(fName_b, proc_date, proc_date,   t_end)
		
		# put [(21z:00z)  & (00z:03z)] together
		lat_  = numpy.concatenate( (lat_a , lat_b),  axis=0)
		lon_  = numpy.concatenate( (lon_a,  lon_b),  axis=0)
		time_ = numpy.concatenate( (time_a, time_b), axis=0)
		sst_  = numpy.concatenate( (sst_a,  sst_b),  axis=0)
		qcf_  = numpy.concatenate( (qcf_a,  qcf_b),  axis=0)
		sigo_ = 0.5 * numpy.ones(numpy.shape(sst_))

		print time_[0]
		
	#----------------------------------------------------------------------------

	if  plot:
		m = Basemap(llcrnrlon=0,llcrnrlat=-80,urcrnrlon=360,urcrnrlat=80,projection='mill')
		x, y = m(lon_[qcf_==1], lat_[qcf_==1])
		
		fig=plt.figure(figsize=(8,4.5))
		ax = fig.add_axes([0.05,0.05,0.9,0.85], axisbg='0.75')
		ax.hold(True)
		
		m.scatter(x, y, s=4, c=sst_[qcf_==1.], marker='o', edgecolor='None', vmin=-1., vmax=34., cmap=plt.cm.jet), plt.colorbar()
		
		m.drawcoastlines(linewidth=1.25)
		m.fillcontinents(color='0.85')
		m.drawparallels(numpy.arange(-80,81,20),labels=[1,1,0,0], fontsize=8)
		m.drawmeridians(numpy.arange( 0,360,60),labels=[0,0,0,1], fontsize=8)
		
		ax.hold(False)	
		plt.title(r'GMI L2 SST RSS [%s-%s]'%(t_start.strftime('%Y%m%d_%HUTC'), t_end.strftime('%Y%m%d_%HUTC')))
		
		fName = 'rss_GMI_l2_SST_%s-%s'%(t_start.strftime('%Y%m%d_%Hz'), t_end.strftime('%Y%m%d_%Hz'))
		plt.savefig(fName + '.png', dpi=100)

	print 'shape:',numpy.shape(sst_)

	return lat_, lon_, time_, sst_, qcf_, sigo_
#----------------------------------------------------------------------------

#yy_ 		= 2015		# year
#mm_		= 7		# month    of above year
#dd_		= 31   		# day      of above month
#hh_             = 0             # UTC hour of above day

#gather_l2_gmi_rss(yy_, mm_, dd_, hh_, NDAYS = .125, plot = True )

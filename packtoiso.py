#!/usr/bin/env python3

import sys, datetime

def main():
	with open(sys.argv[1], 'r') as file:
		content = file.read().strip()

	date, time = content.split()
	year = int(date[:4])
	month = int(date[4:6])
	day = int(date[6:8])
	hour = int(time[:2])
	minute = int(time[2:4])
	second = int(time[4:6])

	iso_dt = datetime.datetime(year, month, day, hour, minute, second)
	print(iso_dt.strftime('%Y-%m-%dT%H:%M:%S'))

if __name__ == "__main__":
	main()

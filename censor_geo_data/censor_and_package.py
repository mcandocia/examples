import os
import csv
import re 
from bs4 import BeautifulSoup
import bs4
import shutil
import StringIO
import zipfile
import numpy as np 
PI = np.pi
import codecs

#note that I did not 



#will censor the fields if applicable
CENSOR_PARAMS = {
	'location':True,#doesn't do anything atm
	'heart_rate':False,
	'speed':True,
	'temperature':True,
	'speed_smooth':True,
	'elevation':True,
	'altitude':True,
	'timestamp':False,#if this is true, it will censor all of the fields in a CSV by removal
	'start_position_lat':True,
	'start_position_long':True,
	'end_position_lat':True,
	'end_position_long':True,
	'latitude':True,#should always be true
	'longitude':True,#should always be true
	'position_lat':True,
	'position_long':True,
	'enhanced_altitude':True,
	'enhanced_speed':True,
	#GPX-specific names
	#there may be others depending on the source
	'ele':True,
	'lat':True,
	'lon':True,
	'time':False,#if this is true, will censor all of a given field in the GPX by removal
}

#additional parameter pairs that can pass off as (latitude, longitude) ordinates
ADDITIONAL_LATLONG = [('start_position_lat','start_position_long'),
	('end_position_lat','end_position_long')]


#removes any NA values for coordinates
REMOVE_MISSING_COORDINATES = True

#string that replaces fields when censored; doesn't matter if field is removed 
#completely by CENSOR_PARAMS['timestamp'] or CENSOR_PARAMS['time']
CENSOR_STRING = '[CENSORED]'

#directory where SEARCH_DIRECTORY entries are located
ROOT_DIRECTORY = '/ntfsl/data/workouts'

#directories to search GPX/CSV files for
SEARCH_DIRECTORIES = [
'workout_gpx/strava_gpx',
'workout_gpx/garmin_fit',
'workout_gpx/cateye_gpx',
'workout_gpx/strava_gpx/gpx_csv']

#new directory to put cleaned files in
TARGET_DIRECTORY = 'CLEAN_WORKOUTS'

#name of zipped file to put TARGET_DIRECTORY's contents in
ZIP_FILENAME = 'CLEAN_WORKOUTS.ZIP'

#list containing {'latitude':..., 'longitude':...,'radius':...} entries
#by default it will be filled using CENSORFILE's data
CENSOR_COORDINATES = []

#should have 3 columns: longitude, latitude, radius (meters)
#will not add values to CENSOR_COORDINATES if =None
CENSORFILE = 'censor.csv'

#extra files (full path OR path relative to ROOT_DIRECTORY) that you want to copy
ADDITIONAL_FILES_TO_COPY = ['']

#will overwrite file if it already exists
OVERWRITE = False 
#these override the above
OVERWRITE_CSV = False
OVERWRITE_GPX = False 

#filenames that you do not want included in copying
#excludes directories
BLACKLIST = set(['test_file.csv'])

#radius of earth in meters
C_R = 6371. * 1000#/1.60934
def distcalc(c1, c2):
	lat1 = float(c1['lat'])*PI/180.
	lon1 = float(c1['lon'])*PI/180.

	lat2 = float(c2['lat'])*PI/180.
	lon2 = float(c2['lon'])*PI/180.

	dlat = lat2-lat1
	dlon = lon2-lon1

	a = np.sin(dlat/2.)**2 + np.cos(lat1)*np.cos(lat2)*np.sin(dlon/2)**2
	c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1-a))
	d = C_R * c 
	return d 

def calculate_distances(points):
	dists = np.asarray([distcalc(c2.attrs,c1.attrs) for c1, c2 in zip(points[1:],points[:-1])])
	return dists 

#checks to see if a longitude, latitude pair is within distance of a point
#that allows censoring
def is_censorable(longitude, latitude):
	censor = False 
	for cc in CENSOR_COORDINATES:
		dist = distcalc({'lat':cc['latitude'],
			'lon':cc['longitude']},
			{'lat':latitude,'lon':longitude})
		if dist <= cc['radius']:
			censor = True 
			break
	return censor 

CSV_REGEX = re.compile(r'.*\.csv$')
GPX_REGEX = re.compile(r'.*\.gpx$')

def find_csv(directory):
	files = os.listdir(directory)
	return [file for file in files if CSV_REGEX.match(file) and file not in BLACKLIST]

def find_gpx(directory):
	files = os.listdir(directory)
	return [file for file in files if GPX_REGEX.match(file) and file not in BLACKLIST]

def censor_line(x, template):
	return [e if not template[i] else CENSOR_STRING for i, e in enumerate(x)]

def transfer_csv(filename, directory):
	target_file = '/'.join([TARGET_DIRECTORY, directory, filename])
	if os.path.isfile(target_file) and not (OVERWRITE or OVERWRITE_CSV):
		return 1
	with open('/'.join([directory, filename]), 'rb') as f:
		reader = csv.reader(f)
		use_alternate_censoring = False 
		with codecs.open('/'.join([TARGET_DIRECTORY, directory, filename]), 'w', encoding='utf8') as of:
			writer = csv.writer(of)
			header = next(reader)
			writer.writerow(header)
			if 'latitude' in header:
				lat_index = header.index('latitude')
				lon_index = header.index('longitude')
			elif 'position_lat' in header:
				lat_index = header.index('position_lat')
				lon_index = header.index('position_long')
			else:
				use_alternate_censoring=True
				other_latlong_indexes = []
				for names in ADDITIONAL_LATLONG:
					try:
						other_latlong_indexes.append( ( header.index(names[0]), header.index(names[1])) )
					except ValueError:
						continue 
			#currently not in use
			censorable_columns = [i for i, column in enumerate(header) if CENSOR_PARAMS.get(column, False)]
			#currently in use 
			should_censor = [CENSOR_PARAMS.get(column, False) for i, column in enumerate(header)]
			#print should_censor
			for line in reader:
				skip_this = False
				if not use_alternate_censoring:
					try:
						longitude, latitude = ( float(line[lon_index]), float(line[lat_index]) )
						if is_censorable(longitude, latitude):
							if not CENSOR_PARAMS['timestamp']:
								writer.writerow(censor_line(line, should_censor))
						else:
							writer.writerow(line)
					except ValueError:
						#likely has one or both of the longitude/latitude values missing
						#I do not personally have files like this (I think), but it is possible
						#will fail to censor latitude/longitude if the other is not present, but that's
						#not realistic
						print '....'
						if not REMOVE_MISSING_COORDINATES:
							writer.writerow(line)
				else:
					will_censor = False 
					for latitude_idx, longitude_idx in other_latlong_indexes:
						try:
							latitude = float(line[latitude_idx])
							longitude = float(line[longitude_idx])
						except ValueError:
							#value of 'None' likely, will just ignore this...
							if REMOVE_MISSING_COORDINATES:
								skip_this=True 
							continue 
						will_censor = will_censor or is_censorable(latitude, longitude)
						if will_censor:
							break
					if skip_this:
						continue
					if will_censor:
						if not CENSOR_PARAMS['timestamp']:
							writer.writerow(censor_line(line, should_censor))
					else:
						writer.writerow(line)
		print 'transfered %s' % (directory + '/' + filename)


def load_censor_coordinates():
	global CENSOR_COORDINATES 
	if CENSORFILE is None:
		return 1
	with open(CENSORFILE,'rb') as f:
		reader = csv.reader(f)
		header = next(reader)
		lat_index = header.index('latitude')
		lon_index = header.index('longitude')
		radius_index = header.index('radius')
		for line in reader:
			CENSOR_COORDINATES.append({'latitude':float(line[lat_index]),
				'longitude':float(line[lon_index]),
				'radius':float(line[radius_index])}
				)
		print CENSOR_COORDINATES 
	print 'loaded CENSOR_COORDINATES'
	return 0

def transfer_gpx(filename, directory):
	target_file = '/'.join([TARGET_DIRECTORY, directory, filename])
	if os.path.isfile(target_file) and not (OVERWRITE or OVERWRITE_GPX):
		return 1
	with open('/'.join([directory, filename]),'r') as f:
		data = f.read()
		soup = BeautifulSoup(data, 'lxml',from_encoding="utf-8")
	trkpts = soup.find_all('trkpt')
	for pt in trkpts:
		lat, lon = (float(pt.attrs['lat']), float(pt.attrs['lon']) )
		will_censor = is_censorable(lon, lat)
		if will_censor:
			if CENSOR_PARAMS['time']:
				pt.decompose()
			else:
				for child in pt.children:
					if isinstance(child, bs4.element.Tag):
						if CENSOR_PARAMS.get(child.name, False):
							child.decompose()
				if CENSOR_PARAMS.get('lat', False):
					pt.attrs['lat'] = CENSOR_STRING
				if CENSOR_PARAMS.get('lon', False):
					pt.attrs['lon'] = CENSOR_STRING 
				
	with codecs.open('/'.join([TARGET_DIRECTORY, directory, filename]), 'w', encoding='utf8') as f:
		try:
			f.write(soup.prettify())
		except:
			print filename 
			print directory 
			raise Exception('fix that damn unicode bug')
	print 'processed %s' % '/'.join([directory,filename])
	return 0 

#makes directory tree in TARGET_DIRECTORY (and TARGET_DIRECTORY itself) if needed
def make_directories():
	counter = 0
	for directory in SEARCH_DIRECTORIES:
		path = '/'.join([TARGET_DIRECTORY, directory])
		if not os.path.exists(path):
			os.makedirs(path)
			counter += 1
	print 'made %d necessary directories' % counter 

def zip_target_directory():
	shutil.make_archive(ZIP_FILENAME, 'zip', TARGET_DIRECTORY)

def main():
	os.chdir(ROOT_DIRECTORY)
	load_censor_coordinates()
	make_directories()
	for directory in SEARCH_DIRECTORIES:
		print 'searching %s' % directory 
		csv_files = find_csv(directory)
		gpx_files = find_gpx(directory)
		#print gpx_files
		for cf in csv_files:
			try:
				transfer_csv(cf, directory)
			except Exception as e:
				print '!'
				print cf 
				raise e 
		for gf in gpx_files:
			transfer_gpx(gf, directory)
	for file in ADDITIONAL_FILES_TO_COPY:
		shutil.copyfile(file, TARGET_DIRECTORY + '/' + file)
	zip_target_directory()
	print 'made censored files and zipped them!'


if __name__=='__main__':
	main()
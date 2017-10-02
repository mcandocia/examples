"""
place this file inside of the directory containing the .fit files and execute it with python3

to execute it from another directory, use 
os.chdir('/.../directory_with_fit_files/')
os.system('python3 convert_fit_to_csv.py')

toggle ALT_FILENAME to change naming scheme
currently recommended to keep at =True, since event type is placed in filename 
of created objects
"""

import csv
import os
#to install fitparse, run 
#sudo pip3 install -e git+https://github.com/dtcooper/python-fitparse#egg=python-fitparse
import fitparse
import pytz
from copy import copy


#for general tracks
allowed_fields = ['timestamp','position_lat','position_long', 'distance',
'enhanced_altitude', 'altitude','enhanced_speed',
                 'speed', 'heart_rate','cadence','fractional_cadence',
                 'temperature']
required_fields = ['timestamp', 'position_lat', 'position_long', 'altitude']



#for laps
lap_fields = ['timestamp','start_time','start_position_lat','start_position_long',
               'end_position_lat','end_position_long','total_elapsed_time','total_timer_time',
               'total_distance','total_strides','total_calories','enhanced_avg_speed','avg_speed',
               'enhanced_max_speed','max_speed','total_ascent','total_descent',
               'event','event_type','avg_heart_rate','max_heart_rate',
               'avg_running_cadence','max_running_cadence',
               'lap_trigger','sub_sport','avg_fractional_cadence','max_fractional_cadence',
               'total_fractional_cycles','avg_vertical_oscillation','avg_temperature','max_temperature']
#last field above manually generated
lap_required_fields = ['timestamp', 'start_time','lap_trigger']

#start/stop events
start_fields = ['timestamp','timer_trigger','event','event_type','event_group']
start_required_fields = copy(start_fields)
#
all_allowed_fields = set(allowed_fields + lap_fields + start_fields)

UTC = pytz.UTC
CST = pytz.timezone('US/Central')

#files beyond the main file are assumed to be created, as the log will be updated only after they are created
ALT_FILENAME = True
ALT_LOG = 'file_log.log'

def read_log():
    with open(ALT_LOG, 'r') as f:
        lines = f.read().split()
    return lines

def append_log(filename):
    with open(ALT_LOG, 'a') as f:
        f.write(filename)
        f.write('\n')
    return None 

def main():
    files = os.listdir()
    fit_files = [file for file in files if file[-4:].lower()=='.fit']
    if ALT_FILENAME:
        if not os.path.exists(ALT_LOG):
            os.system('touch %s' % ALT_FILENAME)
            file_list = []
        else:
            file_list = read_log()
    for file in fit_files:
        if ALT_FILENAME:
            if file in file_list:
                continue 
        new_filename = file[:-4] + '.csv'
        if os.path.exists(new_filename) and not ALT_FILENAME:
            #print('%s already exists. skipping.' % new_filename)
            continue
        fitfile = fitparse.FitFile(file,  
            data_processor=fitparse.StandardUnitsDataProcessor())
        
        print('converting %s' % file)
        write_fitfile_to_csv(fitfile, new_filename, file)
    print('finished conversions')

def lap_filename(output_filename):
    return output_filename[:-4] + '_laps.csv'

def start_filename(output_filename):
    return output_filename[:-4] + '_starts.csv'

def get_timestamp(messages):
    for m in messages:
        fields = m.fields
        for f in fields:
            if f.name == 'timestamp':
                return f.value
    return None

def get_event_type(messages):
    for m in messages:
        fields = m.fields
        for f in fields:
            if f.name == 'sport':
                return f.value
    return None

def write_fitfile_to_csv(fitfile, output_file='test_output.csv', original_filename=None):
    messages = fitfile.messages
    data = []
    lap_data = []
    start_data = []
    if ALT_FILENAME:
        #this should probably work, but it's possibly 
        #based on a certain version of the file/device
        timestamp = get_timestamp(messages)
        event_type = get_event_type(messages)
        if event_type is None:
            event_type = 'other'
        output_file = event_type + '_' + timestamp.strftime('%Y-%m-%d_%H-%M-%S.csv')
    for m in messages:
        skip=False
        skip_lap = False 
        skip_start = False 
        if not hasattr(m, 'fields'):
            continue
        fields = m.fields
        #check for important data types
        mdata = {}
        for field in fields:
            if field.name in all_allowed_fields:
                if field.name=='timestamp':
                    mdata[field.name] = UTC.localize(field.value).astimezone(CST)
                else:
                    mdata[field.name] = field.value
        for rf in required_fields:
            if rf not in mdata:
                skip=True
        for lrf in lap_required_fields:
            if lrf not in mdata:
                skip_lap = True 
        for srf in start_required_fields:
            if srf not in mdata:
                skip_start = True
        if not skip:
            data.append(mdata)
        elif not skip_lap:
            lap_data.append(mdata)
        elif not skip_start:
            start_data.append(mdata)
    #write to csv
    #general track info
    with open(output_file, 'w') as f:
        writer = csv.writer(f)
        writer.writerow(allowed_fields)
        for entry in data:
            writer.writerow([ str(entry.get(k, '')) for k in allowed_fields])
    #lap info
    with open(lap_filename(output_file), 'w') as f:
        writer = csv.writer(f)
        writer.writerow(lap_fields)
        for entry in lap_data:
            writer.writerow([ str(entry.get(k, '')) for k in lap_fields])
    #start/stop info
    with open(start_filename(output_file), 'w') as f:
        writer = csv.writer(f)
        writer.writerow(start_fields)
        for entry in start_data:
            writer.writerow([ str(entry.get(k, '')) for k in start_fields])
    print('wrote %s' % output_file)
    print('wrote %s' % lap_filename(output_file))
    print('wrote %s' % start_filename(output_file))
    if ALT_FILENAME:
        append_log(original_filename)

if __name__=='__main__':
    main()
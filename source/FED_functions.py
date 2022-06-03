#%%
# This is where I might put helper functions and scripts
import csv
from datetime import datetime
import numpy as np
import trompy as tp

def get_FEDevents(filename, eventname):
    
    file = open (filename)
    csvreader= csv.reader(file)
    next(csvreader)
    rows= []
    for row in csvreader:
        rows.append(row)
    timestamps = []
    for row in rows: 
        if row[7] == eventname:
            date_time_obj = datetime.strptime(row [0], '%m/%d/%Y %H:%M:%S')
            timestamps.append (date_time_obj)
           
    t0 = rows[0] [0] 
    day = t0.split()[0] 
    lightson = day + " 07:00:00" 
    refpoint = datetime.strptime(lightson, '%m/%d/%Y %H:%M:%S')
    
                
    pellettimes = []
    for t in timestamps:
        Deltat = t-refpoint
        Deltatinseconds = Deltat.total_seconds()
        Deltatinhours = Deltatinseconds/3600
        pellettimes.append(Deltatinhours)

    return pellettimes

def get_intermealinterval (pellettimes):
    IPIs = np.diff(pellettimes)
    IMI= np.mean([x for x in IPIs if x > (1/60)])
    return IMI

# function to get timestamps from fed csv files
metafile = "..\\FEDXA DATA SHEETS METAFILE .xls"
rows, header = tp.metafilereader(metafile, sheetname="METAFILE")

mice = {}
for row in rows:
    mouse_id = row[1]
    if mouse_id not in mice.keys():
        mice[mouse_id] = {}

for key in mice.keys():
    for row in rows:
        if row[1] == key and row[3] == "FF":
            if row[2] == "GRAIN":
                mice[key]["grain_timestamps"] = get_FEDevents(row[0], event="Pellet")
            elif row[2] == "PR":
                mice[key]["pr_timestamps"] = get_FEDevents(row[0], event="Pellet")
            elif row[2] == "NR":
                mice[key]["NR_timestamps"] = get_FEDevents(row[0], event="Pellet")
            else:
                print(row[2], "is not a valid type of pellet for", key)
                
"""
make dictionary for each mouse with the following fields:
    order
    bodyweight
    hoarding
    grain_timestamps
    pr_timestamps
    nr_timestamps
    grain_pellets_per_day
    pr_pellets_per_day
    nr_pellets_per_day

    pr_intermeal_interval
    nr_intermeal_interval
    pr_mealsize
    nr_mealsize
    
how to show bodyweight?
how to show FR and FR-R - work out rolling % correct and determine when it reaches 95%
how to show choice? - work out proportion of pellets of each type per day
                    - work out average ratio reached for each pellet by day

should maybe roll out new release of trompy or give hamid instructions to get dev version

"""
# %%

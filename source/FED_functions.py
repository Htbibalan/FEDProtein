# This is where I might put helper functions and scripts
import trompy as tp

# function to get timestamps from fed csv files
metafile = "..\\metafilename.xls"
rows, header = tp.metafilereader(metafile, sheetname="MetaFile")

mice = {}
for row in rows:
    mouse_id = row[1]
    if mouse_id not in mice.keys():
        mice[mouse_id] = {}

for key in mice.keys():
    for row in rows:
        if row[1] == key and row[x] == "FF":
            if row[2] == "grain":
                mice[key]["grain_timestamps"] = get_event_times(row[0], event="Pellet")
            elif row[2] == "PR":
                mice[key]["pr_timestamps"] = get_event_times(row[0], event="Pellet")
            elif row[2] == "NR":
                mice[key]["NR_timestamps"] = get_event_times(row[0], event="Pellet")
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
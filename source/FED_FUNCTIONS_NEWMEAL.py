#%%
# This is where I might put helper functions and scripts
import csv
from datetime import datetime
import numpy as np
import trompy as tp

def get_FEDevents(filename, eventname):
    formats = ['%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S']
    file = open(filename)
    csvreader = csv.reader(file)
    next(csvreader)
    rows = []
    for row in csvreader:
        rows.append(row)

    # Determine which format should be used
    try:
        tmp_date_time_obj = datetime.strptime(rows[0][0], formats[0])
        date_format = formats[0]
    except ValueError:
        tmp_date_time_obj = datetime.strptime(rows[0][0], formats[1])
        date_format = formats[1]

    timestamps = []
    for row in rows: 
        if row[7] == eventname:
            date_time_obj = datetime.strptime(row[0], date_format)
            timestamps.append(date_time_obj)
           
    t0 = rows[0][0] 
    day = t0.split()[0] 
    lightson = day + " 07:00:00" 
    refpoint = datetime.strptime(lightson, date_format)
    
    pellettimes = []
    for t in timestamps:
        Deltat = t - refpoint
        Deltatinseconds = Deltat.total_seconds()
        Deltatinhours = Deltatinseconds / 3600
        pellettimes.append(Deltatinhours)

    return pellettimes

def get_data_subset(dictionary, selectors, verbose=True):
    output_dictionary = dictionary.copy()
    for key, value in selectors.items():
        for mouse_id in dictionary.keys():
            try:
                if output_dictionary[mouse_id][key] != value:
                    output_dictionary.pop(mouse_id)
            except KeyError: 
                pass

    if verbose:
        print("{} items in output dictionary".format(len(output_dictionary.keys())))
    
    return output_dictionary

def get_data_fields(dictionary, fields, selectors):
    output_list = []
    reduced_dictionary = get_data_subset(dictionary, selectors)
    
    if len(reduced_dictionary.keys()) > 0:
        for field in fields:
            output_sublist = []
            try:
                for key in reduced_dictionary.keys():
                    output_sublist.append(reduced_dictionary[key][field])
            except KeyError:
                print("{} is not a key in selected dictionary".format(field))
                return
            output_list.append(output_sublist)
    else:
        print("No data in fields in selected dictionary")

    if len(output_list) == 1:
        output_list = output_list[0]
        
    return output_list

def get_meal_and_snack_metrics(pellettimes, meal_threshold=1/60):
    if not pellettimes:
        return (0, 0, 0, 0, 0, 0, [0]*24, 0, 0)

    IPIs = np.diff(np.array(pellettimes))
    meals = []
    snacks = []
    mega_meals = []
    current_event = [pellettimes[0]]

    for i, ipi in enumerate(IPIs):
        if ipi <= meal_threshold:
            current_event.append(pellettimes[i + 1])
        else:
            if len(current_event) == 1:
                snacks.append(current_event)
            elif 2 <= len(current_event) <= 4:
                meals.append(current_event)
            elif len(current_event) >= 5:
                mega_meals.append(current_event)
            current_event = [pellettimes[i + 1]]

    # Handle the last sequence
    if current_event:
        if len(current_event) == 1:
            snacks.append(current_event)
        elif 2 <= len(current_event) <= 4:
            meals.append(current_event)
        elif len(current_event) >= 5:
            mega_meals.append(current_event)

    nmeals = len(meals)
    nsnacks = len(snacks)
    mega_meal_count = len(mega_meals)
    
    hourly_meals = np.zeros(24)
    for meal in meals:
        start_hour = int(meal[0]) % 24
        hourly_meals[start_hour] += 1

    total_pellets = len(pellettimes)
    mealsize = sum(len(meal) for meal in meals) / nmeals if nmeals else 0
    snack_size = sum(len(snack) for snack in snacks) / nsnacks if nsnacks else 0
    total_observation_period = max(pellettimes) - min(pellettimes)
    meal_frequency = nmeals / total_observation_period if total_observation_period > 0 else 0
    snack_frequency = nsnacks / total_observation_period if total_observation_period > 0 else 0
    average_mega_meal_size = sum(len(meal) for meal in mega_meals) / mega_meal_count if mega_meal_count else 0

    return mealsize, snack_size, nmeals, meal_frequency, nsnacks, snack_frequency, hourly_meals.tolist(), mega_meal_count, average_mega_meal_size

#%%

# Load the metafile and process the data
metafile = "..\\FEDProtein_METAFILE.xls"
rows, header = tp.metafilereader(metafile, sheetname="METAFILE")

mice = {}
for row in rows:
    mouse_id = row[1]
    if mouse_id not in mice.keys():
        mice[mouse_id] = {}
        mice[mouse_id]["sex"] = row[4]
        mice[mouse_id]["order"] = row[5]

for key in mice.keys():
    for row in rows:
        if row[1] == key and row[3] == "FF":
            filename = "..\\data\\{}".format(row[0])
            if row[2] == "GRAIN":
                mice[key]["grain_timestamps"] = get_FEDevents(filename, "Pellet")
            elif row[2] == "PR":
                mice[key]["pr_timestamps"] = get_FEDevents(filename, "Pellet")
            elif row[2] == "NR":
                mice[key]["nr_timestamps"] = get_FEDevents(filename, "Pellet")
            else:
                print(row[2], "is not a valid type of pellet for", key)

# Add meal and snack metrics to the dictionary
for key in mice.keys():
    pr_timestamps = mice[key]["pr_timestamps"]
    meal_metrics = get_meal_and_snack_metrics(pr_timestamps)
    mice[key]["pr_meal_size"], mice[key]["pr_snack_size"], mice[key]["pr_number_of_meals"], \
    mice[key]["pr_meal_frequency"], mice[key]["pr_number_of_snacks"], \
    mice[key]["pr_snack_frequency"], mice[key]["pr_hourly_meals"], \
    mice[key]["pr_mega_meal_count"], mice[key]["pr_average_mega_meal_size"] = meal_metrics

    nr_timestamps = mice[key]["nr_timestamps"]
    meal_metrics = get_meal_and_snack_metrics(nr_timestamps)
    mice[key]["nr_meal_size"], mice[key]["nr_snack_size"], mice[key]["nr_number_of_meals"], \
    mice[key]["nr_meal_frequency"], mice[key]["nr_number_of_snacks"], \
    mice[key]["nr_snack_frequency"], mice[key]["nr_hourly_meals"], \
    mice[key]["nr_mega_meal_count"], mice[key]["nr_average_mega_meal_size"] = meal_metrics

# %%
## gets bodyweights and adds to dictionary
metafile = "..\\FEDProtein_METAFILE.xls"
rows, header = tp.metafilereader(metafile, sheetname="BODYWEIGHT")

n_days = len(rows[0])

for row in rows:
    mouse_id = row[0]
    mice[mouse_id]["bodyweight"] = [row[i] for i in range(1, n_days)]

# %%
## gets hoarded pellets and adds to dictionary
metafile = "..\\FEDProtein_METAFILE.xls"
rows, header = tp.metafilereader(metafile, sheetname="HOARDING")

n_days = len(rows[0])

for row in rows:
    mouse_id = row[0]
    mice[mouse_id]["hoarding"] = [row[i] for i in range(1, n_days)]

# %%
# Calculate average pellets per day
for key in mice.keys():
    mice[key]["grain_avg_pellets"] = len(mice[key]["grain_timestamps"]) / 3
    mice[key]["pr_avg_pellets"] = len(mice[key]["pr_timestamps"]) / 7
    mice[key]["nr_avg_pellets"] = len(mice[key]["nr_timestamps"]) / 7

# %%
# Calculate pellets per day for each phase
def get_pellets_per_day(timestamps, start_time=4, days=7):
    pellets_per_day = []
    for day in range(days):
        pellets = [t for t in timestamps if (t > day * 24) and (t < (day + 1) * 24)]
        n_pellets = len(pellets)
        pellets_per_day.append(n_pellets)

    return pellets_per_day

for key in mice.keys():
    mice[key]["grain_pellets_per_day"] = get_pellets_per_day(mice[key]["grain_timestamps"], days=3)
    mice[key]["pr_pellets_per_day"] = get_pellets_per_day(mice[key]["pr_timestamps"])
    mice[key]["nr_pellets_per_day"] = get_pellets_per_day(mice[key]["nr_timestamps"])

# %%
# Assemble pellets per day for the entire time course
for key in mice.keys():
    if mice[key]["order"] == 2:
        mice[key]["all_pellets_per_day"] = mice[key]["grain_pellets_per_day"] + \
                                           mice[key]["pr_pellets_per_day"] + \
                                           mice[key]["nr_pellets_per_day"]
    else:
        mice[key]["all_pellets_per_day"] = mice[key]["grain_pellets_per_day"] + \
                                           mice[key]["nr_pellets_per_day"] + \
                                           mice[key]["pr_pellets_per_day"]

# %%
# Add meal parameters to the dictionary
def get_interpellet_intervals(pellettimes):
    IPIs = np.diff(pellettimes)
    return IPIs

def get_intermeal_interval(pellettimes):
    IPIs = np.diff(pellettimes)
    IMI = np.mean([x for x in IPIs if x > (1/60)])
    return IMI

def get_mealsize(pellettimes):
    """
    Calculates meal size from times of pellets.
    Parameters:
    ----------
    pellettimes : list of floats
        timestamps of pellet deliveries

    Returns:
    --------
    mealsize : float 
        mean size of meal in pellets 
    """
    npellets = len(pellettimes)
    IPIs = np.diff(pellettimes)
    nmeals = len([idx for idx, val in enumerate(IPIs) if val > 1/60])
    mealsize = npellets / nmeals if nmeals else 0

    return mealsize

for key in mice.keys():
    pr_timestamps = mice[key]["pr_timestamps"]
    mice[key]["interpellet_intervals_pr"] = get_interpellet_intervals(pr_timestamps)
    mice[key]["intermeal_interval_pr"] = get_intermeal_interval(pr_timestamps)
    mice[key]["mealsize_pr"] = get_mealsize(pr_timestamps)

    nr_timestamps = mice[key]["nr_timestamps"]
    mice[key]["interpellet_intervals_nr"] = get_interpellet_intervals(nr_timestamps)
    mice[key]["intermeal_interval_nr"] = get_intermeal_interval(nr_timestamps)
    mice[key]["mealsize_nr"] = get_mealsize(nr_timestamps)


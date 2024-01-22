# FEDProtein
 

## This repository contains data files and scripts for analysis of data collected during FEDXA and FEDXB experiments.

#### FEDXA includes 12 male mice and FEDXB 12 female mice.
#### Age: 6-8 weeks old at the time of arrival.8-10 weeks old at the beginning of the experiment
#### strain: C57BL/6NRj
####Date : 23 April- 17 May 2022 (FEDXA) and 2 June- 19 June 2022(FEDXB)
![paradigm](https://github.com/Htbibalan/FEDProtein/blob/main/source/paradigm.png)
*This figure shows the paradigm of the experiment, however it is not fully accurate for the two cohorts, i.e FEDXA and FEDXB, but basically mice start with 3 days of training when they receive grain pellets from FEDs, then they switch to either PR or NR diets and are maintained on the diet for 7 days, after that they switch to the other diet(NR--> PR and PR--> NR and) and are maintained on the new diet for another 7 days*


### Cohorts
* FEDXA and FEDXB
    * Males: FEDXA01-06, PR &#8594; NR ,  FEDXA07-12 NR &#8594; PR
    * Females: FEDXB01-06, PR &#8594; NR , FEDXB07-12 NR &#8594; PR  *note: ***FEDX09*** sacrificed and excluded from the experiment due to bad health condition*

### Food and bodyweight measurement
* number of pellets delivered by FEDs were counted everyday between 08-10 am (apart from final analysis using python script )
* bodyweight was measured everyday between 08-10 am.
* pellets discarded by the mice were counted everyday, removed from the cage bedding and logged as hoarded pellets.

### Experiment protocol details
* ###### FEDXA: 01-06 3 days grain pellets FF mode, 7 days PR pellets FF mode, 7 days NR pellets FF mode, 1 day FR1 NR pellets, 1 day FR1R NR pellets &#8594; 3 days NR diet,large pellets in Standard cages &#8594; Choice box to choose between 5% and 35% casein pellets from FEDs &#8594;  Sacrificed.

* ###### FEDXA: 07-12 3 days grain pellets FF mode, 7 days NR pellets FF mode, 7 days PR pellets FF mode, 1 day FR1 PR pellet, 1 day FR1R PR pellets &#8594; Choice box to choose between 5% and 35% casein pellets &#8594; Sacrificed.

* ###### FEDXB: 01-06 3 days grain pellets FF mode, 7 days PR pellets FF mode, 7 days NR pellets FF mode &#8594; Sacrificed.

* ###### FEDXB: 07-12(Excluding -FEDXB09) 3 days grain pellets FF mode, 7 days NR pellets FF mode, 7 days PR pellets FF mode &#8594; Sacrificed.

***note:*** FEDXB does not include any FR1,FR1R and Choice experiment, mice are sacrificed after 2 weeks of diet manipulation. 


### GitHub repository structure

* **/data** stores FED files of the two cohorts
* **/notebooks** stores python scripts to read or analysis the data
* **/results** stores plots or quantitative data extracted from python scripts
* **/source** contains function.py files plus Excel sheets for the two cohorts, i.e. the data collected manually and daily by the experimenter
* **/stats** mainly stores statistical analysis in JASP format

**Look at [Metafile](https://github.com/Htbibalan/FEDProtein/blob/BNA/FEDProtein_METAFILE.xls) to have an overview of the files attributed to each animal, this files also contains bodyweight and hoarding data.** 

***note:*** you can find the data collected manually for each individual mouse in the folder **/source** 




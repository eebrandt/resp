#!/usr/bin/python

# for moving around in folders
import os
# for making file and folder dialog boxes and  boxes
import Tkinter, Tkconstants, tkFileDialog, tkMessageBox
# for getting the contents of a directory
from os import listdir
# for doing numerical calcuations and for making numpy arrays
import numpy as np
import numpy.ma as ma
# for reading and writing csv files
import csv
# for getting current date and time for timestamp
import datetime
#for manipulating time data 
import time
# to make plots to check on how the analysis is going
import matplotlib.pyplot as plt
# to detect peaks
from pypeaks import Data, Intervals

# gets a timestamp to identify data files
timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H:%M:%S")

def find_csv_filenames(path_to_dir, suffix=".csv"):
	filenames = listdir(path_to_dir)
	return [ filename for filename in filenames if filename.endswith(suffix)]

def vco2_calc(trial_array, specimen_array, csvpath):
	#read in data .csv
	data_array = np.array(np.loadtxt(open(csvpath),dtype = "string", delimiter = ",", skiprows = 1))
	samples = data_array.shape[0]
	#creates an array to hold the sample data
	vco2 = np.zeros((10, samples), dtype=float)
	#extracts the date from the .csv file name
	name = csv[:-4]
	name = name.replace("-", "/")
	namearray = name.split("_")
	#print trial_array
	print namearray
	#subsets the trial data array by date
	trial_date = trial_array[trial_array["date"] == str(namearray[2])]
	#subsets the trial data array by subgroup (2 subgroups for each day, so need to distinguish)
	#TODO: initiate the array with this column as a string, rather than integer
	# subsets the trial data by "page section". This should mostly match up with subgroup, but sometimes other groups/subgroups are swapped in.
	trial_subgroup = trial_date[trial_date["page section"].astype(str) == namearray[0][1]]
	#print trial_subgroup
	trial_subgroup = trial_subgroup[trial_subgroup["syringe"].argsort()]
	
	# number of peaks, determines based on the number of rows in the subsetted trial array
	peaksnum = trial_subgroup.shape[0]
	
	#dumps the raw time and sample data into the first 2 columns of the vco2 array
	vco2[0] = data_array[:,0]
	vco2[1] = data_array[:,1]

	#perform drift correction
	#makes an array of the first and last 300 samples, combines them, and then averages to come up with an offset
	beginning = vco2[1][:300]
	end = vco2[1][-300:]
	baseline = np.append(beginning, end)
	offset = np.mean(baseline)
	#makes a new column in the vco2 array with the offset subtracted
	vco2[2] = vco2[1] - offset

	#defines the flow rate. since this is the same for all samples within a trial, the number is taken arbitrarily from the first row
	flowrate = float(trial_subgroup[0]["flow rate"])
	
	#perform general correction

	vco2[3] = (vco2[2]*flowrate)/1000000
	
	#find peaks, the smoothness factor can be changed. these peaks seem pretty straightforward, so this one seems to work for all trials so far
	peaks_obj = Data(vco2[0], vco2[3], smoothness=10)
	#second part of getting peaks
	peaks_obj.get_peaks(method='slope')
	# gives a raw, built-in plot of the peak data.  Axes aren't particularly meaningful, but can be a useful sanity check.	
	#peaks_obj.plot()
	#extracts peak data from peaks object
	peaks = np.array(peaks_obj.peaks["peaks"])
	#puts the peak data in an easier format to manage
	peaks = np.transpose(peaks)
	
	#reorganizes peaks chronologically. very important to match peak with associated data
	peaks = peaks[peaks[:, 0].argsort()]
	#print peaks
	#p1 = plt.plot(peaks[:,0], peaks[:,1],'ro') 
	#plt.show()

	i = 0
	#array to store information about each peak	
	areas = np.zeros((5, peaks.shape[0]))
	#array to store total minutes between T0 and T1
	times = np.zeros((1, peaks.shape[0]))

	# this section corrects for "bad" data. Bad data points can either be listed in the trial data but not have a peak associated with them, or have a peak that should be discounted. This is handled by masking (applying a boolean array) to both the trial data and the peaks data and then skipping anything that is masked out later
	
	badcount = 0
	#print trial_subgroup
	#creates masking array for trial data. Default is "False", which means the value is not masked
	badcount_trial_delete = np.zeros((1, trial_subgroup.shape[0]), dtype=bool)
	badcount_trial_delete = np.transpose(badcount_trial_delete)
	#creates masking array for peaks array
	badcount_peaks_delete = np.zeros((2, peaks.shape[0]), dtype=bool)
	badcount_peaks_delete = np.transpose(badcount_peaks_delete)
	
	# loops through trial data to determine if there is a bad peak ("don't use") and whether that bad peak exists
	while badcount < peaks.shape[0]:
		if (trial_subgroup[badcount]["use"] == "no" and trial_subgroup[badcount]["peak present?"] == "no"):
			#delete peak
			badcount_trial_delete[badcount] = True
			print "Skipping " + str(trial_subgroup[badcount]["individual"]) + ". Peak not present."

		if (trial_subgroup[badcount]["use"] == "no" and trial_subgroup[badcount]["peak present?"] == "yes"):
			#delete entry from trial data	
			badcount_trial_delete[badcount] = True			
			#delete peak 
			badcount_peaks_delete[badcount] = True
			print "Skipping " + str(trial_subgroup[badcount]["individual"]) + ". Peak present."
		badcount = badcount + 1
	#applies the masking arrays to trial and peak data, respectively
	trialmask = ma.masked_array(trial_subgroup, mask=badcount_trial_delete)
	#print trialmask
	peakmask = ma.masked_array(peaks, mask = badcount_peaks_delete)
	#print peakmask

	#loops through each peak, including blank this time
	i = 0
	while i < peakmask.shape[0]:
		#check to make sure a given entry isn't masked, both on trial and peak data
		if (peakmask[i][0] is not ma.masked) and (trialmask[i][0] is not ma.masked):
			#defines the area of each peak. We assume that the peak is 90 samples (seconds) wide. Can be changed.
			peaklow = int(peakmask[i][0] - 45)
			peakhigh = int(peakmask[i][0] + 45)
			peakrange = peakhigh - peaklow
			#subsets vco2 to isolate a single peak
			peaksubset = np.zeros((2,90))
			peaksubset[0] = vco2[0] [peaklow:peakhigh]
			peaksubset[1] = vco2[3] [peaklow:peakhigh]

			#optional plot of peaks
			#p1 = plt.plot(vco2[0, peaklow:peakhigh], vco2[3, peaklow:peakhigh],'ro') 
			#plt.show()
		
			#integrate under each curve, assigns to first column of "areas"
			areas[0,i]=(np.trapz(peaksubset[1], x=peaksubset[0], dx=1.0))
			#do volume correction
			areas[1,i] = areas[0,i]*(float(trialmask[i]["Total Volume"])/float(trialmask[i]["Volume Injected"]))
			#do time correction, extracts time data from trial_array, subtracts them, and turns it into minutes
			T0 = time.strptime(trialmask["T0"][i], '%H:%M')
			T1 = time.strptime(trialmask["T1"][i], '%H:%M')
			Ttotal = (time.mktime(T1) - time.mktime(T0))/60
			#writes total time to times array
			times[0,i] = Ttotal
			#writes time_corrected data to areas array
			areas[2,i] = areas[1,i]/Ttotal
			i = i + 1
		else:
			i = i+1
		
	i = 0
	#loops through each peak except blank, for blank and weight correction
	while i < peakmask.shape[0]-1:
			#again, makes sure that the given entry isn't masked
		if (peakmask[i][0] is not ma.masked) and (trialmask[i][0] is not ma.masked):
			sample_output=[]
			#do blank correction 
			blank = areas[2,-1]
			areas[3,i] = areas[2,i] - blank
			#do weight correction
			areas[4,i] = areas[3,i]/float(trialmask[i]["Weight (mg)"])
			# adding data to the array that will be written to the csv file eventually.
			#animal ID
			individual = trialmask[i]["individual"]
			#print individual
			ind_index = np.where(specimen_array["ID"] == individual)
			#print specimen_array[:][0]
			#print ind_index
			ind_info = specimen_array[ind_index]
			sample_output.append(trialmask[i]["individual"])
			#date
			sample_output.append(trialmask[i]["date"])
			#temperature
			sample_output.append(trialmask[i]["temp"])
			#date fed
			sample_output.append(trialmask[i]["fed"])
			#group
			sample_output.append(trialmask[i]["group"])
			#subgroup
			sample_output.append(trialmask[i]["subgroup"])
			#genus
			genus = str(ind_info["genus"]).replace("['", "")
			genus = genus.replace("']", "")
			sample_output.append(genus)
			#print ind_info["ID"]
			#species
			species = str(ind_info["species"]).replace("['", "")
			species = species.replace("']", "")
			sample_output.append(species)
			#sex
			sex = str(ind_info["sex"]).replace("['", "")
			sex = sex.replace("']", "")
			sample_output.append(sex)
			#purge time
			sample_output.append(trialmask[i]["T0"])
			#read time
			sample_output.append(trialmask[i]["T1"])
			# minutes total
			sample_output.append(times[0,i])
			# volume injected
			sample_output.append(float(trialmask[i]["Volume Injected"]))
			# total volume
			sample_output.append(float(trialmask[i]["Total Volume"]))
			#blank injected
			sample_output.append(float(trialmask[-1]["Volume Injected"]))
			#blank total
			sample_output.append(float(trialmask[-1]["Total Volume"]))
			#flow rate
			sample_output.append(flowrate)
			#weight
			sample_output.append(round(trialmask[i]["Weight (mg)"],3))
			#vco2 without weight correction
			sample_output.append(areas[3,i])
			#vco2 with weight correction
			sample_output.append(areas[4,i])
			#whether specimen is repeated measures (completed all trials) or not
			complete = str(ind_info["completed"]).replace("['", "")
			complete = complete.replace("']", "")
			sample_output.append(complete)
			#Was this sample analyzed by hand? No, because we used this program.
			sample_output.append("no")
			#comments from trial sheets
			sample_output.append(trialmask[i]["comments"])
			#comments from specimen sheet
			sample_output.append(specimen_array[i]["comments"])
			#print sample_output
			#write data to .csv
			writer.writerow(sample_output)
			i = i + 1
		else:
			i = i + 1

complete_output_header = ["individual", "date", "temperature", "date_fed", "group", "subgroup" , "genus", "species", "sex", "TI0", "TI1", "minutes_total", "vol_injected", "vol_total", "blank_injected", "blank_total", "flow rate (mL/min)", "weight (mg)", "VCO2_raw", "VCO2_corrected","complete", "by hand", "trial comments", "specimen comments"]

#read in file that contains information about each individual
specimen_file = tkFileDialog.askopenfilename(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/data/", title = "Choose the file that contains specimen info.")
specimen_array = np.recfromcsv(specimen_file, delimiter=',', case_sensitive=True, deletechars='', replace_space=' ')

#read in trial file, contains data collected at time of trial
trial_file = tkFileDialog.askopenfilename(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/data/", title = "Choose the file that contains trial info.")
trial_array = np.recfromcsv(trial_file, delimiter=',', case_sensitive=True, deletechars='', replace_space=' ')

byhand =  tkMessageBox.askyesno("By Hand Analysis", "Do you want to include a file of analysis done with Expedata?")

if byhand:
	#reads in file that contains any analysis done by hand (through expedata)
	byhand_file = tkFileDialog.askopenfilename(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/data/", title = 		"Choose the file that contains analysis done by hand.")
	kwargs = dict(delimiter=",",
             missing_values={22:False, 23:False},
             filling_values={22:"", 23:""})
	byhand_array = np.recfromcsv(byhand_file, **kwargs)
	#print byhand_array
	#byhand_array = np.recfromcsv(byhand_file, delimiter=',', case_sensitive=True, deletechars='', replace_space=' ')

#get directory that contains data files
data_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/data/", title = "Choose the folder that contains data files.")

# Defines and opens a .csv file that we'll write our data to
fl = open(data_folder + "/" + "vco2_data" + "_" + timestamp + '.csv', 'w')
writer = csv.writer(fl)
# writes header to csv file
writer.writerow(complete_output_header)

#get names of all sub-directories
# looks in each individual folder for trial folders
groups =  os.listdir(data_folder)
#print groups
for group in groups:
	# make sure each "individual" is a folder
	if os.path.isdir(data_folder + "/" + group):
		subgroups = os.listdir(data_folder + "/" + group)
		# loop to go through each subgroup
		for subgroup in subgroups:
			# defines the folders that we'll be looking in for csvs.
			subgroup_folder = data_folder + "/" + group + "/" + subgroup + "/auto_csvs"
			#print subgroup_folder
				# looks for all the csvs. in a subgroup folder
			csvs = find_csv_filenames(subgroup_folder, suffix = ".csv")
			#print csvs
			for csv in csvs:
				#run analysis function
				#print trial_array
				vco2_calc(trial_array, specimen_array, data_folder +"/"+group+"/"+subgroup+"/auto_csvs/"+csv)
if byhand:
	writer.writerows(byhand_array)
	print "Manual analysis file appended."
print "Done."
#close .csv file that has been written				
fl.close() 

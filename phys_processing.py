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
	name = samplefile[:-4]
	name = name.replace("-", "/")
	namearray = name.split("_")
	print namearray[0] +"_"+ namearray[1]
	#subsets the trial data array by date
	trial_date = trial_array[trial_array["date"] == str(namearray[2])]
	#subsets the trial data array by subgroup (2 subgroups for each day, so need to distinguish)
	#TODO: initiate the array with this column as a string, rather than integer
	# subsets the trial data by "page section". This should mostly match up with subgroup, but sometimes other groups/subgroups are swapped in.
	trial_subgroup = trial_date[trial_date["page section"].astype(str) == namearray[0][1]]
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
	peakmask = ma.masked_array(peaks, mask = badcount_peaks_delete)

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
			sample_output=np.zeros((24),dtype = "S25")
			#do blank correction 
			blank = areas[2,-1]
			areas[3,i] = areas[2,i] - blank
			#do weight correction
			areas[4,i] = areas[3,i]/float(trialmask[i]["Weight (mg)"])
			# adding data to the array that will be written to the csv file eventually.
			#animal ID
			individual = trialmask[i]["individual"]
			ind_index = np.where(specimen_array["ID"] == individual)
			ind_info = specimen_array[ind_index]
			sample_output[0] = (trialmask[i]["individual"])
			#date
			sample_output[1] = (trialmask[i]["date"])
			#temperature
			sample_output[2] = (trialmask[i]["temp"])
			#date fed
			sample_output[3] = (trialmask[i]["fed"])
			#group
			sample_output[4] = (trialmask[i]["group"])
			#subgroup
			sample_output[5] = (trialmask[i]["subgroup"])
			#genus
			genus = str(ind_info["genus"]).replace("['", "")
			genus = genus.replace("']", "")
			sample_output[6] = (genus)
			#species
			species = str(ind_info["species"]).replace("['", "")
			species = species.replace("']", "")
			sample_output[7] = (species)
			#sex
			sex = str(ind_info["sex"]).replace("['", "")
			sex = sex.replace("']", "")
			sample_output[8] = (sex)
			#purge time
			sample_output[9] = (trialmask[i]["T0"])
			#read time
			sample_output[10] = (trialmask[i]["T1"])
			# minutes total
			sample_output[11] = (times[0,i])
			# volume injected
			sample_output[12] = (float(trialmask[i]["Volume Injected"]))
			# total volume
			sample_output[13] = (float(trialmask[i]["Total Volume"]))
			#blank injected
			sample_output[14] = (float(trialmask[-1]["Volume Injected"]))
			#blank total
			sample_output[15] = (float(trialmask[-1]["Total Volume"]))
			#flow rate
			sample_output[16] = (flowrate)
			#weight
			sample_output[17] = (round(trialmask[i]["Weight (mg)"],3))
			#vco2 without weight correction
			sample_output[18] = (areas[3,i])
			#vco2 with weight correction
			sample_output[19] = (areas[4,i])
			#whether specimen is repeated measures (completed all trials) or not
			complete = str(ind_info["completed"]).replace("['", "")
			complete = complete.replace("']", "")
			sample_output[20] = (complete)
			#Was this sample analyzed by hand? No, because we used this program.
			sample_output[21] = ("no")
			#comments from trial sheets
			sample_output[22] = (trialmask[i]["comments"])
			#comments from specimen sheet
			sample_output[23] = (specimen_array[i]["comments"])
			#write data to .csv
			sample_output.transpose()
			addindex = len(writearray[writearray[:,0] != ""])
			writearray[addindex]=sample_output
			i = i + 1
		else:
			i = i + 1

#header for complete data file
complete_output_header = ["individual", "date", "temperature", "date_fed", "group", "subgroup" , "genus", "species", "sex", "TI0", "TI1", "minutes_total", "vol_injected", "vol_total", "blank_injected", "blank_total", "flow rate (mL/min)", "weight (mg)", "VCO2_raw", "VCO2_corrected","complete", "by hand", "trial comments", "specimen comments"]

#read in file that contains information about each individual
specimen_file = tkFileDialog.askopenfilename(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/respirometry/data/", title = "Choose the file that contains specimen info.")
specimen_array = np.recfromcsv(specimen_file, delimiter=',', case_sensitive=True, deletechars='', replace_space=' ')

#read in trial file, contains data collected at time of trial
trial_file = tkFileDialog.askopenfilename(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/respirometry/data/", title = "Choose the file that contains trial info.")
trial_array = np.recfromcsv(trial_file, delimiter=',', case_sensitive=True, deletechars='', replace_space=' ')
writearray = np.empty(shape=[len(trial_array), 24], dtype = "S25")

byhand =  tkMessageBox.askyesno("By Hand Analysis", "Do you want to include a file of analysis done with Expedata?")

if byhand:
	#reads in file that contains any analysis done by hand (through expedata)
	byhand_file = tkFileDialog.askopenfilename(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/respirometry/data/", title = 		"Choose the file that contains analysis done by hand.")
	byhand_array = np.recfromcsv(byhand_file, delimiter = ',')

#get directory that contains data files
data_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/dissertation/uncategorized/physiology/respirometry/data/", title = "Choose the folder that contains data files.")

# Defines and opens a .csv file that we'll write our data to
fl = open(data_folder + "/" + "vco2_data" + "_" + timestamp + '.csv', 'w')
writer = csv.writer(fl)
# writes header to csv file
writer.writerow(complete_output_header)

#get names of all sub-directories
# looks in each individual folder for trial folders
groups =  os.listdir(data_folder)
for group in groups:
	# make sure each "individual" is a folder
	if os.path.isdir(data_folder + "/" + group):
		subgroups = os.listdir(data_folder + "/" + group)
		# loop to go through each subgroup
		for subgroup in subgroups:
			# defines the folders that we'll be looking in for csvs.
			subgroup_folder = data_folder + "/" + group + "/" + subgroup + "/auto_csvs"
				# looks for all the csvs. in a subgroup folder
			samplefiles = find_csv_filenames(subgroup_folder, suffix = ".csv")
			for samplefile in samplefiles:
				#run analysis function
				vco2_calc(trial_array, specimen_array, data_folder +"/"+group+"/"+subgroup+"/auto_csvs/"+samplefile)

#adds any byhand data. Writes line by line to writearray
i = 0
byhand_size = byhand_array.size
if byhand:
	addindex = len(writearray[writearray[:,0] != ""])
	while i < byhand_size:
		j = 0
		while j < 22:
			writearray[addindex+i][j] = byhand_array[i][j]
			j = j +1
		i = i + 1
	print "Manual analysis file appended."
print "Done."

#writes complete data file
writer.writerows(writearray)	
#close .csv file that has been written			
fl.close() 

#make file that has separate line for each individual
#header for compiled file
compiled_output_header = ["ID", "species", "sex", "weight", "10_raw", "10_weight", "15_raw", "15_weight", "20_raw", "20_weight", "25_raw", "25_weight", "30_raw", "30_weight", "35_raw", "35_weight", "40_raw", "40_weight"]

# Defines and opens a .csv file that we'll write our data to for compiled file
comp = open(data_folder + "/" + "vco2_data_compiled" + "_" + timestamp + '.csv', 'w')
write = csv.writer(comp)

# writes header to csv file
write.writerow(compiled_output_header)

#generates list of individuals, since we're making a file with one row per individual
IDs = writearray[:,0]
#removes any rows that are empty
IDs = IDs[IDs != ""]
#removes any duplicates in list
IDs =  list(set(IDs))

#list of temperatures that we're going to loop through
temps = ["10", "15", "20", "25", "30", "35", "40"]

#for each individual
for ID in IDs:
	#array to hold all info for the row of the file
	compiled = np.empty((18), dtype = "S25")
	#subsets writearray for a given individual
	indsubset = writearray[writearray[:,0] == ID]
	#writes ID, sex, species, and weight. Weight is calculated as average across all treatments
	compiled[0] = ID
	compiled[1] = indsubset[0][7]
	compiled[2] = indsubset[0][8]
	weights = indsubset[:,17]
	weights = weights.astype(np.float)
	compiled[3] = np.mean(weights)
	#loops through all of the temperatures for a given individual
	for temp in temps:
		#calculates appropriate place in output array based on temperature
		tempint = int(temp)/5*2
		#subsets individual array based on temperature
		subset_temp = indsubset[indsubset[:,2] == temp]
		#if there's no data for a given temperature, write raw and weight-corrected data as empty
		if subset_temp.shape[0] <1:
			compiled[tempint] = ""
			compiled[tempint+1] = ""
		#if there's more than one entry for a given temperature, average them
		elif subset_temp.shape[0] > 1:
			values = subset_temp[:,18].astype(np.float)
			compiled[tempint] = np.mean(values)
			values_corr = subset_temp[:,19].astype(np.float)
			compiled[tempint+1] = np.mean(values_corr)
		#if there's one entry for a given temperature, write that to the output array
		else:
			compiled[tempint] = subset_temp[0,18]
			compiled[tempint+1] = subset_temp[0,19]
	#write output array to .csv
	write.writerow(compiled)

#close .csv file
comp.close()	

import re 
import os
import numpy as np
import zipfile
import matplotlib.pyplot as plt
import epidemic_calculations_MS_21Feb2016 as ec
from pylab import *
import csv
#################################################################################
#replace with the link to folder where zip files of epidemic results are located
os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
##########################################################################################
#write csv
writer = csv.writer(open('Epidemic_duration_for_contour.csv','wb'))
header = ["Graph", "num_modules", "Qvalue", "Tvalue", "Duration", "SD", "SE"] 
writer.writerow(header)	
#######################################

Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
num_modules_list = [10]+[125]*9

Trange= [0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.19, 0.2]
graphtype=["geometric"]
sigma = 0.2
N= 10000
##

for T in Trange:
	durdict = {}
	error={}
	se={}
	##########################################################
	for graph in graphtype:
	    print ("graph="),graph
	    durdict[graph] = {}
	    error[graph]={}
	    se[graph] = {}
	    for Q, num_modules in zip(Qrange, num_modules_list):
	    	epi_duration=[]
		zf=zipfile.ZipFile("Epidemic_"+graph+"N10000_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
		
		for filecount in xrange(50):
			file1="INF_"+graph+"n10000_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
		    	file2="REC_"+graph+"n10000_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
		    	###################################################################################
		    	with zf.open(file1,'r') as f1:
				lines=f1.readlines()
				x=0
				infected_list={}
				for line in lines:
				    data=re.split(',', line)[1:]
				    data[0]=data[0][2:]
				    data[-1]=data[-1][:-2]
				    infected_list[x]=[int(num) for num in data]
				    x+=1
		    	with zf.open(file2,'r') as f2:
				lines=f2.readlines()
				x=0
				recovered_list={}
				for line in lines:
				    data=re.split(',', line)[1:]
				    data[0]=data[0][2:]
				    data[-1]=data[-1][:-2]
				    recovered_list[x]=[int(num) for num in data]
				    x+=1
		        ###################################################################################
		        
		   	epi_duration.append([max(recovered_list[x]) for x in xrange(500) if ec.is_epidemic(infected_list[x])==True])
		
		duration_list=[x for sublist in epi_duration for x in sublist]
		if len(duration_list)==0:duration_list=[0] 
		if not durdict[graph].has_key(Q): durdict[graph][Q]={}
		if not error[graph].has_key(Q): error[graph][Q]={}
		if not se[graph].has_key(Q): se[graph][Q]={}
		durdict[graph][Q][num_modules] = np.mean(duration_list) #mean of all the epidemic size above the epidemic criteria
		error[graph][Q][num_modules] = np.std(duration_list)
		se[graph][Q][num_modules] = (1.*np.std(duration_list))/ np.sqrt(len(duration_list))
		print graph, num_modules, Q, T, np.mean(duration_list)

	#################################   
	#################################   
	
	    for Q, num_modules in zip(Qrange, num_modules_list):
		elements = [graph, str(num_modules), str(Q), str(T), durdict[graph][Q][num_modules], error[graph][Q][num_modules], se[graph][Q][num_modules]]
		writer.writerow(elements)
		
        

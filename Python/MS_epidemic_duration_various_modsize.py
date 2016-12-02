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
writer = csv.writer(open('Epidemic_duration.csv','wb'))
header = ["Graph", "Mod type", "num_modules", "Qvalue", "Tvalue", "Duration", "SD", "SE"] 
writer.writerow(header)	
#######################################

Qrange= [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]
num_modules_list = [10]*8


Trange= [0.1]
graph = "geometric"
modtype = ["POISSON", "GEOMETRIC"]
count=0
N = 10000
sigma = 0.2

for T in Trange:
	durdict = {}
	error={}
	se={}
	##########################################################
	for mod_graph in modtype:
	    print ("graph="),graph
	    durdict[mod_graph] = {}
	    error[mod_graph]={}
	    se[mod_graph] = {}
	    for Q, num_modules in zip(Qrange, num_modules_list):
	    	epi_duration=[]
		zf=zipfile.ZipFile("Epidemic_"+graph+"N"+str(N)+"_"+str(mod_graph)+"_mod_dist_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
		
		for filecount in xrange(50):
			file1="INF_"+graph+"n"+str(N)+"_"+str(mod_graph)+"_mod_dist_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
		    	file2="REC_"+graph+"n"+str(N)+"_"+str(mod_graph)+"_mod_dist_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
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
		if not durdict[mod_graph].has_key(Q): durdict[mod_graph][Q]={}
		if not error[mod_graph].has_key(Q): error[mod_graph][Q]={}
		if not se[mod_graph].has_key(Q): se[mod_graph][Q]={}
		durdict[mod_graph][Q][num_modules] = np.mean(duration_list) #mean of all the epidemic size above the epidemic criteria
		error[mod_graph][Q][num_modules] = np.std(duration_list)
		se[mod_graph][Q][num_modules] = (1.*np.std(duration_list))/ np.sqrt(len(duration_list))
		print mod_graph, num_modules, Q, T, np.mean(duration_list)

	#################################   
	#################################   
	
	    for Q, num_modules in zip(Qrange, num_modules_list):
		elements = [graph, mod_graph, str(num_modules), str(Q), str(T), durdict[mod_graph][Q][num_modules], error[mod_graph][Q][num_modules], se[mod_graph][Q][num_modules]]
		writer.writerow(elements)
		
        

import re 
import numpy as np
import matplotlib.pyplot as plt
import epidemic_calculations_MS_21Feb2016 as ec
import zipfile
import os
import csv
#################################################################################
Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
num_modules_list = [10] +[2]*4+[5]*7 + [10]*8+[100]*9 + [125]*9


Trange= [0.1, 0.18, 0.3]

graphtype=["poisson", "geometric"]
count=0
N = 10000
sigma = 0.2

#################################################################################

#replace with the link to folder where zip files of peakdemic results are located
os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
##########################################################################################
#write csv
writer = csv.writer(open('Modules_infected_13April2016.csv','wb'))
header = ["Graph", "num_modules", "Qvalue", "Tvalue", "Modules infected(%)", "SD", "SE"] 
writer.writerow(header)	
#######################################

for T in Trange:
	average_module_infected={}
	error={}
	se = {}
	for graph in graphtype:
	    average_module_infected[graph]={}
	    error[graph]={}
	    se[graph] = {}
	    for (Q, num_modules) in zip(Qrange, num_modules_list):
		print graph, Q, num_modules
		total_module_infected=[]
		zf=zipfile.ZipFile("Epidemic_"+graph+"N"+str(N)+"_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
		for filecount in xrange (50):
		    file1="INF_"+graph+"n"+str(N)+"_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
		    f1=(zf.open(file1))
		    lines=f1.readlines()
		    x=0
		    infected_list={}
		    for line in lines:
		    	data=re.split(',', line)[1:]
		        data[0]=data[0][2:]
		        data[-1]=data[-1][:-2]
		        infected_list[x]=[int(num) for num in data]
		        x+=1
		    
		    file2="REC_"+graph+"n"+str(N)+"_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
		    f2=(zf.open(file2))
		    lines=f2.readlines()
		    x=0
		    recovered_list={}
		    for line in lines:
			data=re.split(',', line)[1:]
			data[0]=data[0][2:]
			data[-1]=data[-1][:-2]
			recovered_list[x]=[int(num) for num in data]
			x+=1
		    total_module_infected.append([100-((1.0*ec.find_infected_modules(infected_list[x], num_modules))/num_modules *100.)  for x in xrange(500) if ec.is_epidemic(infected_list[x])==True])
		
		module_list=[x for sublist in total_module_infected for x in sublist]
		if len(module_list)==0:module_list=[0] 
		if not average_module_infected[graph].has_key(Q):  average_module_infected[graph][Q]={}
		if not error[graph].has_key(Q): error[graph][Q]={}
		if not se[graph].has_key(Q): se[graph][Q]={}

		average_module_infected[graph][Q][num_modules] = np.mean(module_list) #mean of all the epidemic size above the epidemic criteria
		error[graph][Q][num_modules] = np.std(module_list)
	 	se[graph][Q][num_modules] = (1.0*np.std(module_list))/np.sqrt(len(module_list))
		print graph, num_modules, Q, T, np.mean(module_list)

	#################################   
	    for (Q, num_modules) in zip(Qrange, num_modules_list):
		elements = [graph, str(num_modules), str(Q), str(T), average_module_infected[graph][Q][num_modules], error[graph][Q][num_modules],  se[graph][Q][num_modules]]
		writer.writerow(elements)


        
        

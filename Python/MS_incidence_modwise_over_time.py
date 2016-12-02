import re 
import itertools
import numpy as np
import epidemic_calculations_MS as ec
import matplotlib.pyplot as plt
import zipfile
import os
import csv
#################################################################################

#replace with the link to folder where zip files of epidemic results are located
os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
##########################################################################################

graphtype=["geometric"]
Trange = [0.1]
Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
num_modules_list = [10] +[2]*4+[5]*7 + [10]*8+[100]*9
Qrange= [0.8, 0.8]
num_modules_list = [10, 125]
count=0
sigma = 0.2
##################################################################
#write csv
writer1 = csv.writer(open('Incidence_modwise_15Feb2016.csv','wb'))
header = ["Graph", "num_modules", "Qvalue", "Tvalue", "module_order", "incidence_mean"] 
writer1.writerow(header)
####################################################################
for T in Trange:
	for graph in graphtype:
		for Q, num_modules in zip(Qrange, num_modules_list):
			print ("Q= "), graph, T, Q, num_modules
		        modwise_incidence_final ={}
		        modwise_incidence={}
		        for mod in xrange(num_modules): modwise_incidence[mod]=[] 
		        zf=zipfile.ZipFile("Epidemic_"+graph+"N10000_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
		        for filecount in xrange (50):
			    	file1="INF_"+graph+"n10000_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
				file2="REC_"+graph+"n10000_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
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
			     
			     	####################################
			     	for x in xrange(500):
			     		if ec.calculate_reproduction_number(infected_list[x], recovered_list[x])>1:
			     			incid_list= ec.incidence_modwise_over_time(infected_list[x], recovered_list[x], num_modules)
			    			for num in xrange(num_modules):  modwise_incidence[num].append(incid_list[num])
		    	####################################
		    	
		        #aggregegate modwise Ro time-series
		        for mod in xrange(num_modules):      		
		    	         modwise_incidence_final[mod]=[np.mean([num for num in x if num!= None ]) for x in itertools.izip_longest(* modwise_incidence[mod],fillvalue=0)]
	


		        ########################################################################
		        for module in xrange(num_modules):
		    	        element1 = [graph, str(num_modules), str(Q), str(T), str(module)]
		    	        element2 = modwise_incidence_final[module]
		    	        writer1.writerow(element1 + element2)
			######################################################################                 


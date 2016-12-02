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
writer = csv.writer(open('Epidemic_size_T0.2.csv','wb'))
header = ["Graph", "Num_modules", "Qvalue", "Tvalue", "Epidemic_size(%)", "SD", "num_samples", "SE"] 
writer.writerow(header)	
#######################################

Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
num_modules_list = [10] + [125]*9

Trange= [0.2]
graphtype=["geometric", "poisson"]
sigma = 0.2
N= 10000
##


##add graphtype over here

for T in Trange:
	epidict = {}
	error={}
	se ={}
	length = {}
	#########################################################
	for graph in graphtype:
	    print ("graph="),graph, 
	    epidict[graph] = {}
	    error[graph] = {}
	    se[graph] ={}
            length[graph] = {}
	    #plt.clf()
	    for Q, num_modules in zip(Qrange, num_modules_list):
	    	print T, Q, num_modules
		total_epi_size=[]
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
			
		   	total_epi_size.append([(len([num for num in infected_list[x] if num>0])/(1. *N)) * 100 for x in xrange(500) if ec.is_epidemic(infected_list[x])==True])
		
		epi_list=[x for sublist in total_epi_size for x in sublist]
		if len(epi_list)==0:epi_list=[0] 
		if not epidict[graph].has_key(Q): epidict[graph][Q]={}
		if not error[graph].has_key(Q): error[graph][Q]={}
		if not se[graph].has_key(Q): se[graph][Q]={}
		if not length[graph].has_key(Q): length[graph][Q] = {}
		epidict[graph][Q][num_modules] = np.mean(epi_list) #mean of all the epidemic size above the epidemic criteria
		error[graph][Q][num_modules] = np.std(epi_list)
		se[graph][Q][num_modules] = (1.0*np.std(epi_list))/np.sqrt(len(epi_list))
		length[graph][Q][num_modules] = len(epi_list)
		print graph, num_modules, Q, T, np.mean(epi_list)

	#####################################
	    for Q, num_modules in zip(Qrange, num_modules_list):
		elements = [graph, str(num_modules) , str(Q), str(T), epidict[graph][Q][num_modules], error[graph][Q][num_modules], length[graph][Q][num_modules], se[graph][Q][num_modules]]
		writer.writerow(elements)



        
        

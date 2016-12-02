import re 
import os
import numpy as np
import zipfile
import matplotlib.pyplot as plt
import epidemic_calculations_MS_21Feb2016 as ec
from pylab import *
import csv

#################################################################################
# Code to plot contour plot of peak epidemic size results. x axis: Q values, y axis: T values, z = peak epidemic size. 
# Three subplots for three degree distribution type
#################################################################################

#replace with the link to folder where zip files of epidemic results are located
os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
##########################################################################################
#####################################
#write csv
writer = csv.writer(open('Epidemic_resilience_for_pois_vs_geom.csv','wb'))
header = ["Graph", "num_modules", "Qvalue",  "Tvalue", "Proportion_outbreak", "SD", "SE"] 
writer.writerow(header)	
#######################################
Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
num_modules_list = [10] +[2]*4+[5]*7 + [10]*8+[100]*9 + [125]*9

Trange= [0.18]
graphtype=["poisson", "geometric"]
sigma = 0.2
N= 10000
##

for T in Trange:
	epidict = {}
	error={}
	se = {}
	#########################################################
	for graph in graphtype:
	    print ("graph="),graph
	    epidict[graph] = {}
	    error[graph] = {}
	    se[graph] = {}
	    #plt.clf()
	    for (Q, num_modules) in zip(Qrange, num_modules_list):
		epi_count=[]
		zf=zipfile.ZipFile("Epidemic_"+graph+"N10000_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
		
		for filecount in xrange (50):
			file1="INF_"+graph+"n10000_m"+str(num_modules)+"_d10_Q"+str(Q)+"T_"+str(T)+"_iter"+str(filecount)+".txt"
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
			#percetage of epidemic for each random graph
		   	epi_count.append((len([x for x in xrange(500) if ec.is_epidemic(infected_list[x])==True])*100.0)/500.)
		
		#epi_list=[x for sublist in epi_count for x in sublist]
		if len(epi_count)==0:epi_count=[0] 
		if not epidict[graph].has_key(Q): epidict[graph][Q]={}
		if not error[graph].has_key(Q): error[graph][Q]={}
		if not se[graph].has_key(Q): se[graph][Q]={}

		epidict[graph][Q][num_modules] = np.mean(epi_count) #mean of all the epidemic size above the epidemic criteria
		error[graph][Q][num_modules] = np.std(epi_count)
		se[graph][Q][num_modules] = (1.0*np.std(epi_count))/np.sqrt(len(epi_count))
		print Q, T, np.mean(epi_count)


	#####################################
	for graph in graphtype:
		for (Q, num_modules) in zip(Qrange, num_modules_list):
			elements = [graph, str(num_modules), str(Q), str(T), epidict[graph][Q][num_modules], error[graph][Q][num_modules], se[graph][Q][num_modules]]
			writer.writerow(elements)



        
        

import re 
import os
import numpy as np
import zipfile
import matplotlib.pyplot as plt
import epidemic_calculations_MS_21Feb2016 as ec
from pylab import *
import csv
import networkx as nx
#####################################
#write csv
writer = csv.writer(open('Local_vs_global_burden_July2016.csv','wb'))
header = ["Graph", "Num_modules", "Qvalue", "Tvalue", "Local_buden", "L_SD", "L_SE", "Global_buden", "G_SD", "G_SE"] 
writer.writerow(header)	
#######################################

Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
num_modules_list = [10]+[125]*9

num_mod = 125
Trange= [0.18]
graphtype = ["geometric"]
graph1="geometric"
sigma = 0.2
N= 10000


#################################################################################
#replace with the link to folder where zip files of epidemic results are located
os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
##########################################################################################

for T in Trange:
	l_epidict = {}
	l_error={}
	l_se ={}
	g_epidict = {}
	g_error={}
	g_se ={}
	#########################################################
	for graph in graphtype:
	    print ("graph="),graph, 
	    l_epidict[graph] = {}
	    l_error[graph] = {}
	    l_se[graph] ={}
	    g_epidict[graph] = {}
	    g_error[graph] = {}
	    g_se[graph] ={}
	    #plt.clf()
	    for Q, num_modules in zip(Qrange, num_modules_list):
		##########################################
		G = {}
	 	os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/random_modular_graphs/random_modular_graphs_July_6_2015")	
		zf=zipfile.ZipFile("Edge_list_"+graph1+"N10000_m"+str(num_modules)+"_d10_Q"+str(Q)+".zip", "r")
		for count in xrange (50):
			filename="Edge_list_"+graph1+"N10000_m"+str(num_modules)+"_d10_Q"+str(Q)+"_iter"+str(count)+".txt"
		    	myfile=zf.open(filename)
		    	G[count]=nx.read_edgelist(myfile, data=False, nodetype=int, edgetype=int)
		print Q, num_modules, ("graphs read!!!")
	    	##########################################
		local_burden=[]
		global_burden=[]
		os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
		zf=zipfile.ZipFile("Epidemic_"+graph+"N"+str(N)+"_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
		#############################################
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

		#############################################
			for x in xrange(500):
				if ec.is_epidemic(infected_list[x])==True:
					local1, global1 = ec.calculate_local_vs_global_burden(num_modules, infected_list[x], recovered_list[x], G[filecount])
					local_burden.append(local1)
					global_burden.append(global1)
		   	
		if len(local_burden)==0: local_burden = [0] 
		if len(global_burden)==0: global_burden = [0]
		if not l_epidict[graph].has_key(Q): l_epidict[graph][Q]={}
		if not l_error[graph].has_key(Q): l_error[graph][Q]={}
		if not l_se[graph].has_key(Q): l_se[graph][Q]={}
		if not g_epidict[graph].has_key(Q): g_epidict[graph][Q]={}
		if not g_error[graph].has_key(Q): g_error[graph][Q]={}
		if not g_se[graph].has_key(Q): g_se[graph][Q]={}
		
		l_epidict[graph][Q][num_modules] = np.mean(local_burden) #mean of all the epidemic size above the epidemic criteria
		l_error[graph][Q][num_modules] = np.std(local_burden)
		l_se[graph][Q][num_modules] = (1.0*np.std(local_burden))/np.sqrt(len(local_burden))

		g_epidict[graph][Q][num_modules] = np.mean(global_burden) #mean of all the epidemic size above the epidemic criteria
		g_error[graph][Q][num_modules] = np.std(global_burden)
		g_se[graph][Q][num_modules] = (1.0*np.std(global_burden))/np.sqrt(len(global_burden))


		print graph, num_modules, Q, T, np.mean(local_burden), np.mean(global_burden)

	#####################################
	    for Q, num_modules in zip(Qrange, num_modules_list):
		elements = [graph, str(num_modules) , str(Q), str(T), l_epidict[graph][Q][num_modules], l_error[graph][Q][num_modules], l_se[graph][Q][num_modules], g_epidict[graph][Q][num_modules], g_error[graph][Q][num_modules], g_se[graph][Q][num_modules]]
		writer.writerow(elements)



        
        

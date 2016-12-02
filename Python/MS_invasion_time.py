import re 
import itertools
import numpy as np
import epidemic_calculations_MS_21Feb2016 as ec
import matplotlib.pyplot as plt
import zipfile
import os
import csv
#################################################################################

Qrange= [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]
num_modules_list = [10]*9
Trange= [0.1]
graphtype =["geometric"]
sigma = 0.2
#################################################################################

#replace with the link to folder where zip files of peakdemic results are located
os.chdir("/home/prathasah/Dropbox (Bansal Lab)/SBLab_Community_structure_dynamics/Community_Structure_Dynamics/analyze_epidemic_results/Epidemic_results_6July_2015")
##########################################################################################
#write csv
writer = csv.writer(open('Invasion_time.csv','wb'))
header = ["Graph", "num_modules",  "Qvalue", "Tvalue", "module#", "Invasiontime(thresh=20)", "SD", "SE"] 
writer.writerow(header)	
#######################################

for T in Trange:
        invasion_dict={}
	SE={}
	SD={}
	print ("T"), T
	for graph in graphtype:
                invasion_dict[graph]={}
	    	SE[graph]={}
	    	SD[graph]={}
	    	
		for (Q, num_modules) in zip(Qrange, num_modules_list):
                        print ("graph"), graph, Q, num_modules
                        zf=zipfile.ZipFile("Epidemic_"+graph+"N10000_m"+str(num_modules)+"_Q"+str(Q)+"_T_"+str(T)+"_sigma_"+str(sigma)+".zip", "r")
                        invasion_time=[]
			if Q not in invasion_dict[graph].keys(): 
				invasion_dict[graph][Q]={}
				SD[graph][Q]={}
				SE[graph][Q]={}
				
			if num_modules not in invasion_dict[graph][Q].keys(): 
				invasion_dict[graph][Q][num_modules]={}
				SD[graph][Q][num_modules]={}
				SE[graph][Q][num_modules]={}

			for filecount in xrange(50):      
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
				    
				###################
				for x in xrange(500):
					if ec.is_epidemic(infected_list[x])==True:
						invasion_time.append(ec.calculate_invasion_time (infected_list[x], num_modules))
				###################
                
			#mean of list of lists. Do not unequal sublist with 0
			invasion_dict[graph][Q][num_modules] = [np.mean([num for num in x if num!= None ]) for x in itertools.izip_longest(*invasion_time)]
			
			SE[graph][Q][num_modules] = [np.std([num for num in x if num!= None ])/(np.sqrt(len([num for num in x if num!= None ]))) for x in itertools.izip_longest(*invasion_time)]
			SD[graph][Q][num_modules] = [np.std([num for num in x if num!= None ]) for x in itertools.izip_longest(*invasion_time)]
                        
                        
		
		
			

        #################################   
		for (Q, num_modules) in zip(Qrange, num_modules_list):
			for num in xrange(num_modules):
				elements1 = [graph, str(num_modules), str(Q), str(T), str(num)]
				if len(invasion_dict[graph][Q][num_modules]) < num+1: elements2 = ['', '', '']
				else: elements2 =[invasion_dict[graph][Q][num_modules][num], SD[graph][Q][num_modules][num], SE[graph][Q][num_modules][num]]
				elements = elements1+elements2
				writer.writerow(elements)

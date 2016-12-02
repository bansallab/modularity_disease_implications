# This module simulates time-based epidemic simulation
#############################################################
import networkx as nx
import random as rnd
import matplotlib.pyplot as plt
import numpy as np
import time
###############################################################

def run_timebased_epidemic(G,beta,sigma):
    """N=network size, T=transmissibility, tau=recovery probability"""
    N=len(G.nodes())
    #infected_list is a list that contains zeros and ones. 
    #Zero indicates that the node is susceptibile (or recovered). One indicates that the node is infected
    infected_list=[0]*N #Initialize list with zeros indicating all nodes are susceptibles
    #recovered_list is a list that contains zeros and ones. 
    #Zero indicates that the node has not recovered. One indicates that the node has recovered
    recovered_list = [0]*N #Initialize list with zeros indicating none of the nodes are recovered at t=0
    track_infection=[0]*N #tracks time at infection for each node.
    track_recovery=[0]*N  #tracks time of recovery for each node.
    p_zero = rnd.choice(G.nodes()) # choose a random node to infect
    track_time=1
    infected_list[p_zero] = 1
    track_infection[p_zero]=1
    #pos = nx.spring_layout(G)
    while sum(infected_list)>0:
        #print ("time=="), time
        #visualize_epidemic(G, pos, infected_list, recovered_list, track_time)
        track_time += 1
        # infect the susceptibles with the probability 1-exp(-bk) where k is the number of infeced neighbors.
        infected_list_new = [1 if not infected_list[node] and not recovered_list[node] and rnd.random()< (1- np.exp(-beta*infected_neighbors(G, node, infected_list))) else infected_list[node] for node in xrange(N)]       
        #Store the time of infection for the newly infected nodes in track_infection list
        track_infection=[track_time if infected_list_new[node]-infected_list[node]==1 and not recovered_list[node] else track_infection[node] for node in xrange(N)]
        # update the original infected_list
        infected_list=[infected_list_new[x] if infected_list_new[x]==1 else infected_list[x] for x in xrange(N)]
        #All the infected nodes recover with a probailty sigma
        recovered_list_new=[1 if infected_list[node]==1 and rnd.random()<sigma else 0 for node in xrange (N) ]
        # Store the time of recovery for the newly recovered nodes in track_recovered list
        track_recovery=[track_time if recovered_list_new[node]-recovered_list[node]==1 else track_recovery[node] for node in xrange(N)] 
        #update infected_list
        infected_list=[0 if recovered_list_new[node]==1 else infected_list[node] for node in xrange(N)]
        # update orignial recovered_list
        recovered_list=[recovered_list_new[x] if recovered_list_new[x]==1 else recovered_list[x] for x in xrange(N)]      
    
    return track_infection, track_recovery   
        
        
def visualize_epidemic(G, pos, infected_list, recovered_list, track_time):
    """ visualize epidemic spread. Red indicates infected node, blue indicate 
    susceptible and black indicate recovered"""
    susceptible_list = [node for node in G.nodes() if infected_list[node] == 0 and recovered_list[node]==0] 
    #print ("susceptible list"), susceptible_list
    nx.draw_networkx_nodes(G, pos,nodelist = [node for node in xrange(len(G.nodes())) if infected_list[node]==1], node_color='r')
    nx.draw_networkx_nodes(G, pos, nodelist = [node for node in xrange(len(G.nodes())) if recovered_list[node]==1], node_color='k')
    nx.draw_networkx_nodes(G, pos, nodelist = susceptible_list, node_color='b')
    nx.draw_networkx_edges(G, pos, edgelist = G.edges())
    plt.title("time=" + str(time) + ", susceptible= " + str(len(susceptible_list))+", infected= "+str(sum(infected_list))+ ", recovered= "+str(sum(recovered_list)))
    plt.savefig('modular_time'+str(time).zfill(3)+'.png')

def infected_neighbors(G, node, infected_list):
    """Calculates the number of infected neighbors for the node"""
    return sum([infected_list[node_i] for node_i in G.neighbors(node)])   
    
        
if __name__ == "__main__":
    """Main function to mimic C++ behavior. This function requires the file
    random-modular-network-generator.py."""
    #try:  
    ##################################################################
    #Generate graph
    print "Generating graph....." 
    G = nx.gnp_random_graph(5000, 0.4)
    print ("Graph generated!")
    print ("Starting epidemic simulations")
    track_infection, track_recovery = run_timebased_epidemic(G,0.025, 0.143)
    print ("Ran one instance of epidemic simulation")
    #print ("list of nodal infection time "), track_infection
    #print ("list of nodal reovery time "), track_recovery
    #except (IndexError, IOError):
    #    print "Reached an errror. Try again"

        

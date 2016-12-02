import networkx as nx
#importing modular graph generator by Sah2014
import random_modular_generator_variable_modules as rmg
#importing sequence generator by Sah2014
import sequence_generator as sg
import graph_tool.all as gt
import numpy as np
import matplotlib.pyplot as plt
import math
from random import randrange as rd
#####################################################3
##Original network = N=60, d=3 m=3, Q=0.3, degree distribution = regular, moddist=regular
##higher fragmentation = N=60,  d=3, m=6, Q = 0.3+1./3 - 1./6
##higher cohesion = N=60, d=3, m=3, Q=0.49
###############################################
def generate_random_point(radius, xcenter, ycenter):
	
	
	x = xcenter+rd(-radius,radius)
	y = ycenter+rd(-radius,radius)
	return (x,y)
###############################################
def PointsInCircum(r,n, center):
    xcenter, ycenter = center
    return [(math.cos(2*np.pi/n*x)*r + xcenter,math.sin(2*np.pi/n*x)*r +ycenter) for x in xrange(0,n+1)]

###############################################
def PointsInCircum_for_FRAG(n):
  	
    r= float(n)/2 + 0.5
    xcenter=50
    ycenter =50
    return [(math.cos(2*np.pi/n*x)*r + xcenter,math.sin(2*np.pi/n*x)*r +ycenter) for x in xrange(0,n+1)]
	
###############################################
def is_inedge(edge, mod_nodes):
    node1, node2 = edge
    module1 = [mod for mod in mod_nodes.keys() if node1 in mod_nodes[mod]][0]
    module2 = [mod for mod in mod_nodes.keys() if node2 in mod_nodes[mod]][0]
    return module1==module2

#######################################	
def calculate_Qmax(G, mod_nodes):


	Lt= sum([G.degree(node) for node in G.nodes()])
	total  =0
	
	for mod in mod_nodes.keys():
		Lk = sum([G.degree(node) for node in mod_nodes[mod]])
		total+= (1.0*Lk/Lt) - (1.0*Lk/Lt)**2 
		
		

	return total

##########################################################
N=60
d=4
num_modules1=3
num_modules2=10
num_modules3=5
Q1 = 0.0
Q2=0.54
Q3 = 0.54
Q4=0.54
sfunction = sg.regular_sequence
modfunction = sg.regular_sequence
modfunction2= sg.geometric_sequence
################################################################
G1, mod_nodes1 = rmg.generate_modular_networks(N, sfunction, modfunction, Q1, num_modules1, d)
nx.write_graphml(G1, "network1.graphml")
Qmax =calculate_Qmax(G1, mod_nodes1)
print ("Qrel=="), Q1/Qmax
in_edges1 = [edge1 for edge1 in G1.edges() if is_inedge(edge1, mod_nodes1)==True]
print ("inedges 1"), in_edges1
########################################################################
print ("graph with high cohesion!!!!")
G2, mod_nodes2 = rmg.generate_modular_networks(N, sfunction, modfunction, Q2, num_modules1, d)
nx.write_graphml(G2, "network_high_cohesion.graphml")
Qmax =calculate_Qmax(G2, mod_nodes2)
print ("Qrel=="), Q2/Qmax
in_edges2 = [edge1 for edge1 in G2.edges() if is_inedge(edge1, mod_nodes2)==True]
########################################################################
print ("graph with high fragmentation!!!!")
G3, mod_nodes3 = rmg.generate_modular_networks(N, sfunction, modfunction, Q3, num_modules2, d)
nx.write_graphml(G3, "network_high_fragmentation.graphml")
Qmax =calculate_Qmax(G3, mod_nodes3)
print ("Qrel=="), Q3/Qmax
in_edges3 = [edge1 for edge1 in G3.edges() if is_inedge(edge1, mod_nodes3)==True]

########################################################################
print ("graph with subgroup size variation!!!")
G4, mod_nodes4 = rmg.generate_modular_networks(N, sfunction, modfunction2, Q4, num_modules3, d)
Qmax =calculate_Qmax(G4, mod_nodes4)
print ("Qrel=="), Q4/Qmax
nx.write_graphml(G4, "network_subgroup_variation.graphml")
in_edges4 = [edge1 for edge1 in G4.edges() if is_inedge(edge1, mod_nodes3)==True]
########################################################################

########################################################################
G1= gt.load_graph("network1.graphml")
G2 = gt.load_graph("network_high_cohesion.graphml")
G3 = gt.load_graph("network_high_fragmentation.graphml")
G4 = gt.load_graph("network_subgroup_variation.graphml")
pos1 = G1.new_vertex_property("vector<double>")
pos2 = G2.new_vertex_property("vector<double>")
pos3 = G3.new_vertex_property("vector<double>")
pos4 = G4.new_vertex_property("vector<double>")
ecolor1 = G1.new_edge_property('string')
ecolor2 = G2.new_edge_property('string')
ecolor3 = G3.new_edge_property('string')
ecolor4 = G4.new_edge_property('string')
esize1 = G1.new_edge_property('int')
esize2 = G2.new_edge_property('int')
esize3 = G3.new_edge_property('int')
esize4 = G4.new_edge_property('int')
#shape1 = G1.new_vertex_property("int")
#shape2 = G2.new_vertex_property("int")
#shape3 = G3.new_vertex_property("int")
coord_center1 = [(40,25), (55,30), (45,35)]
coord_center2 = PointsInCircum_for_FRAG(10)
coord_center3 = PointsInCircum_for_FRAG(5)
######################################
##Fix node position
for module in xrange(num_modules1):

	radius = 3	
	nodelist1 = mod_nodes1[module]
	nodelist2 = mod_nodes1[module]
	coord_points = PointsInCircum(radius, len(nodelist1), coord_center1[module])
	for node1, node2, coord in zip(nodelist1, nodelist2, coord_points):
		pos1[G1.vertex(node1)] = coord
		pos2[G1.vertex(node2)] = coord
		#shape1[G1.vertex(node1)] = module
		#shape2[G2.vertex(node2)] = module
		#shape3[G3.vertex(node2)] = module

######################################
##Fix node position
for module in xrange(num_modules2):

	radius = 1	
	nodelist3 = mod_nodes3[module]
	coord_points = PointsInCircum(radius, len(nodelist3), coord_center2[module])
	for node1,  coord in zip(nodelist3, coord_points):
		pos3[G3.vertex(node1)] = coord
##############################################
##Fix node position
for module in xrange(num_modules3):

	radius = 1	
	nodelist4 = mod_nodes4[module]
	coord_points = PointsInCircum(radius, len(nodelist4), coord_center3[module])
	for node1,  coord in zip(nodelist4, coord_points):
		pos4[G4.vertex(node1)] = coord
##############################################
############Fix edge color
for e in G1.edges():
	source = int(e.source())
	target = int(e.target())
	if (source, target) in in_edges1 or (target, source) in in_edges1: 
		ecolor1[e] =  "#e08214"
		esize1[e] = 2.5
	else:  
		ecolor1[e] ="#8073ac"
		esize1[e] = 1

for e in G2.edges():
	source = int(e.source())
	target = int(e.target())
	if (source, target) in in_edges2 or (target, source) in in_edges2: 
		ecolor2[e] = "#e08214"
		esize2[e] = 2.5
	else:  
		ecolor2[e] = "#8073ac"
		esize2[e] = 1

for e in G3.edges():
	source = int(e.source())
	target = int(e.target())
	if (source, target) in in_edges3 or (target, source) in in_edges3: 
		
		ecolor3[e] = "#e08214"
		esize3[e] = 2.5
	else:  
		ecolor3[e] = "#8073ac"
		esize3[e] = 1

for e in G4.edges():
	source = int(e.source())
	target = int(e.target())
	if (source, target) in in_edges4 or (target, source) in in_edges4: 
		
		ecolor4[e] = "#e08214"
		esize4[e] = 2.5
	else:  
		ecolor4[e] = "#8073ac"
		esize4[e] = 1


############draw graph#####################
state = gt.minimize_blockmodel_dl(G2)
b = state.b

gt.graph_draw(G1, pos1,  vertex_size=15,  vertex_shape= "circle", vertex_fill_color="#3f3f3f", edge_color= ecolor1,  edge_pen_width=esize1, output="1.network1.pdf")
gt.graph_draw(G2, pos2, vertex_size=15,  vertex_shape= "circle",  vertex_fill_color="#3f3f3f",  edge_color=ecolor2, edge_pen_width=esize2,  output="2.network_high_cohesion.pdf")

gt.graph_draw(G3, pos3, vertex_size=15,  vertex_shape = "circle",  vertex_fill_color="#3f3f3f",  edge_color=ecolor3, edge_pen_width=esize3,  output="3.network_high_fragmentation.pdf")

gt.graph_draw(G4, pos4, vertex_size=15,  vertex_shape = "circle",  vertex_fill_color="#3f3f3f",  edge_color=ecolor4, edge_pen_width=esize4,  output="4.network_subgroup_variation.pdf")

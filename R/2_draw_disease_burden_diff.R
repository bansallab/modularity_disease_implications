library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(lattice)
library(cowplot)
library(MASS)
library(Hmisc)
library(RColorBrewer) 

de <- read.csv("Empirical_disease_burden.csv", header=T)
str(de)
de$episize <- de$Epidemic_size...
ds <- read.csv("Simple_random_disease_burden.csv", header=T)
str(ds)
ds$episize <- ds$Epidemic_size...

dt <- read.csv("Network_mechanism_modularity_10Oct2016.csv", header=T)
dt$Qrel <- ifelse(dt$Qmax>0, dt$Q/dt$Qmax, 0)
str(dt)
describe(dt)
dt

dtemp<-read.csv("Network_TEMPORAL_mechanism_modularity_12July2016.csv", header=T)
dtemp$Qrel <- ifelse(dtemp$Qmax>0, dtemp$Q/dtemp$Qmax, 0)

str(dt)
#I have removed Mann dolphin from this version
select <- c(2,3,4,5,6,7,8,9,10,15,17,18,22,26,27,28,29,33,35,37,38,39,41,42,53,44,45,46,50,51,54,55,56,57,58,59,60,62,68)
epi_diff <- data.frame("network" = as.character(), "class"= as.character() , "Qrel"=  as.double(), "emp_size" = as.double(), "ran_size"= as.double())
for (graph in select){
  print (graph)
  Qrel <-subset(de,Network==graph)$Qrel 
  empsize<-subset(de,Network==graph)$episize
  ransize<- subset(ds,Network==graph)$episize
  class <- subset(dt, Graph.==graph)$class
  epi_diff <-rbind(epi_diff, data.frame("network" = graph, "class"= class , "Qrel"= Qrel, "emp_size" = empsize, "ran_size"= ransize))
}


##epi_diff1 and dur_diff1 are datframes after removing networks where
##Qrel of modular random was not less than empirical networks
#p1 <- ggplot(dt, aes(y = Qrel, x = Q)) +   geom_jitter(aes(fill=class), width=0.02, height=0.02, pch=21, size= 2.1, color="black", stroke= 0.05)+
#  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8),legend.key = element_blank(), legend.title = element_text(size=5), legend.box = "horizontal", legend.position=c(0.63,0.2),legend.text=element_text(size=5), legend.key.size = unit(1.5, "mm"),plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
#  scale_fill_manual(values=c("#1b9e77", "#d95f02","#e7298a", "#7570b3",  "#fed9a6"))+
#  xlab(expression(paste("Newman modularity, ", italic(Q))))+
#  ylab(expression(paste("Relative modularity, ", italic(Q[rel]))))+ xlim(0.0, 1) + ylim(0.0, 1)+ theme(plot.margin=unit(c(.2 , 0.2, 0, 0.02), "cm"))+
#  labs(fill="Taxonomic class") + geom_abline(intercept = 0, slope=1, size= 0.1, linetype=2)+ ggtitle("A")
#p1
#p2 <- ggplot(dtemp1, aes(y = Qrel, x = Q)) +   geom_point(aes(fill= abbr,  shape=group), size= 1.6, stroke=0.05, color="black")+
#  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8),legend.key = element_blank(), legend.title = element_text(size=5), legend.box = "horizontal", legend.position=c(0.7,0.2),legend.text=element_text(size=5), legend.key.size = unit(1.5, "mm"),plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
#  scale_shape_manual(values=c(21, 22, 23, 24, 25), guide="none")+
#  scale_fill_manual(values=c('Ant' = "#66c2a5", 'Raccoon'="#8da0cb",  'Vole'="#e78ac3"))+
#  xlab(expression(paste("Newman modularity, ", italic(Q))))+
#  ylab(expression(paste("Relative modularity, ", italic(Q[rel]))))+ xlim(0.0, 1) + ylim(0.0, 1)+ theme(plot.margin=unit(c(.2 , 0.2, 0, -0.02), "cm"))+
#  guides(fill=guide_legend(override.aes = list(color=c('Ant' = "#66c2a5", 'Raccoon'="#8da0cb",  'Vole'="#e78ac3"))))+
#  ggtitle("B") + geom_abline(intercept = 0, slope=1, size= 0.1, linetype=2)+ labs(fill="Species")


p1 <- ggplot(dt, aes(y = Qmax, x = Q)) +   geom_point(aes(fill=Qrel),  pch=21, size=1.8, color="black", stroke= 0.05)+
  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8),legend.key = element_blank(), legend.title = element_text(size=5), legend.box = "horizontal", legend.position=c(0.7,0.2),legend.text=element_text(size=5), legend.key.size = unit(1.5, "mm"),plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
  scale_fill_gradient(low="yellow", high="#e31a1c", limits=c(-0.1, 1.1))+
  xlab(expression(paste("Newman modularity, ", italic(Q))))+
  ylab(expression(paste("Maximum modularity, ", italic(Q[max]))))+ xlim(0.0, 1) +   ylim(0.0, 1)+ theme(plot.margin=unit(c(.2 , 0.2, 0, 0.02), "cm"))+
  labs(fill=expression(paste("Relative modularity, ", italic(Q[rel])))) + geom_abline(intercept = 0, slope=1, size= 0.1, linetype=2)+ ggtitle("A")
p1




dtemp1 <-subset(dtemp, dtemp$group <2)
dtemp1 <-subset(dtemp1, dtemp1$abbr!="Ant")
dtemp1
dtemp1$group <- as.factor(dtemp1$group)
describe(dtemp1)

#

p2 <- ggplot(dtemp1, aes(y = Qrel, x = time_slice)) +   geom_point(aes(color=abbr), size= 1.2)+
  geom_line(size=0.3)+
  scale_fill_manual(values=c("#1b9e77", "#d95f02","#e7298a"))+
  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8),legend.position = "None")+
  xlab("Time point")+ ylab(expression(paste("Relative modularity, ", italic(Q[rel]))))+ylim(0.0, 1)+ theme(plot.margin=unit(c(.2 , 0.2, 0, -0.02), "cm"))+
  ggtitle("B")+facet_grid(abbr~.) +theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
  geom_text(data=data.frame(x=42, y= 0.1, label= c("C. fellah", "P. lotor", "M. agrestis"), abbr=c("Ant1", "Raccoon", "Vole")), aes(x,y,label=label), size=3, fontface="italic", inherit.aes=FALSE)
p2


p3 <- ggplot(data=epi_diff, aes(y = emp_size , x = Qrel)) + 
  geom_point(aes(fill=class),  pch=21, size= 2, color="black", stroke= 0.01)+
  scale_fill_manual(values=c("#1b9e77", "#d95f02","#e7298a", "#7570b3",  "#fed9a6"))+
  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8), plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
  theme(legend.key = element_blank(), legend.title = element_text(size=5), legend.box = "horizontal", legend.position=c(0.2,0.2),legend.text=element_text(size=6), legend.key.size = unit(1.5, "mm"))+
  geom_segment(aes(xend=Qrel+0, yend=ran_size), arrow = arrow(length = unit(0.08,"cm")), color="black", size=0.15)+ 
  ylab("Outbreak size") + xlab(expression(paste("Relative modularity, ", italic(Q[rel])))) + xlim(0,1) + ylim(0,62) +  theme(plot.margin=unit(c(.2,0, 0, -0.1), "cm"))+ 
  geom_rect(data=epi_diff[1,], aes(xmin=0.618, xmax=1, ymin=0, ymax= 62), fill="grey", alpha=0.2)+
  ggtitle('C')+labs(fill="Taxonomic class")
p3


#give dimension in inches
cairo_ps("2_disease_burden_emp_vs_simple_random.eps",  height = 2.2, width = 7)
#label = textGrob(expression(paste("Relative modularity, ", italic(Q[rel]))), rot = 90, vjust = 0.5)
p<- grid.arrange(p1,p2, p3, ncol = 3)
p
dev.off()
#ggsave("2_disease_burden_emp_vs_simple_random.eps", p, height = 5, width = 17.8, units = "cm", dpi = 1200)

#


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
dt$Qrel <- dt$Q/dt$Qmax
str(dt)
describe(dt)
dt


str(dt)
select <- c(2,3,4,5,6,7,8,9,10,15,17,18,22,26,27,28,29,33,35,37,38,39,41,42,53,44,45,46,50,51,54,55,56,57,58,59,60,62,68)
#select <- c(1:69)
epi_diff <- data.frame("network" = as.character(), "class"= as.character() , "Qrel"=  as.double(), "Fragmentation"= as.double(),"Subgroup cohesion" =as.double())
for (graph in select){
  Qrel <-subset(dt, Graph.== graph)$Qrel 
  num.modules<- log(subset(dt, Graph.==graph)$num.modules)
  wd.d.ratio <- subset(dt, Graph.==graph)$wd.d.ratio
 class <- subset(dt, Graph.==graph)$class
  epi_diff <-rbind(epi_diff, data.frame("network" = graph, "class"= class , "Qrel"= Qrel, "Fragmentation"= num.modules,"Subgroup cohesion" = wd.d.ratio))
}

epi_diff$col <- cut(epi_diff$Qrel,
                              breaks = c(-Inf, 0.618, Inf),
                              labels = c("<= 0.618", "> 0.618"))

p1 <- ggplot(epi_diff, aes(y = Fragmentation, x = Subgroup.cohesion, size=Qrel, shape = col)) +
  geom_point(aes(fill=class, shape=col),   size= 1.2, color="black", stroke= 0.05)+
  scale_fill_manual(values=c("#1b9e77", "#d95f02","#e7298a", "#7570b3",  "#fed9a6"), guide =guide_legend(override.aes = list(shape = 21), title = "Taxonomic class"))+
  scale_shape_manual(values=c(21, 24))+
  scale_size_continuous(range = c(0.25, 1.5))+
theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8))+
  theme(legend.key = element_blank(), legend.title = element_text(size=5), legend.box = "horizontal", legend.position=c(0.4,0.8),legend.text=element_text(size=5), legend.key.size = unit(2, "mm"),plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
xlab("Subgroup cohesion")+ ylab("Network fragmentation")+ xlim(0.3, 1) + ylim(0.5, 3)+
  labs(shape = expression(paste("Relative modularity, ", italic(Q[rel]))))
  p1

ggsave("2_extra_fragmentation_cohesion.eps", p1, height = 7, width = 6.5, units = "cm", dpi = 1200)
  
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(lattice)
library(cowplot)
library(MASS)
library(Hmisc)
library(RColorBrewer) 
library(effects)
library(car)
library(lme4)
library(tree)

de <- read.csv("Empirical_disease_burden.csv", header=T)
str(de)
de$episize <- de$Epidemic_size...


ds <- read.csv("Simple_random_disease_burden.csv", header=T)
str(ds)
ds$episize <- ds$Epidemic_size...

dn <- read.csv("Network_mechanism_modularity_10Oct2016.csv", header=T)
dn$fclass <- factor(dn$class)
str(dn)
dn$frag <- log(dn$num.modules)

##############################
dt <- data.frame("Graph"=character(),  "class"=character() , "fclass" =factor(), "order"=character(), "genus"= character(), "species"=character(),
                 "sociality"=character(),"Qrel" = double(), "fragmentation" = double(), "cohesion" = double(), "Subgroup size variation"=double(),  "size" = double())

do_select<- de$Network
#for these netorks, Qrel of simple random is same as Qrel of empirical
not_select <- c(16)
do_select <- setdiff(do_select, not_select)
for (graph in do_select){
  esize <- subset(de, de$Network==graph)$episize
  ssize <- subset(ds, ds$Network==graph)$episize
  size <- (100*(esize-ssize))/ssize 
  Qrel <-subset(dn, Graph.==graph)$Q/  subset(dn, Graph.==graph)$Qmax
  frag <-round(log(subset(dn, Graph.==graph)$num.modules),2)
  coh <- subset(dn, Graph.==graph)$wd.d.ratio
  subgroup_var <- subset(dn, Graph. ==graph)$CV.modsize
  order <-subset(dn, Graph.==graph)$order
  class <-subset(dn, Graph.==graph)$class
  fclass <-subset(dn, Graph.==graph)$fclass
  genus <-subset(dn, Graph.==graph)$genus
  species <-subset(dn, Graph.==graph)$species
  social <-subset(dn, Graph.==graph)$Sociality
  print (paste(graph, Qrel, size))
  dt <-rbind(dt, data.frame("Graph"= graph, "class"=class, "fclass" = fclass,  "order"= order, "genus"= genus, "species"= species,
                            "sociality" = social ,"Qrel" = Qrel, "fragmentation" = frag, "cohesion" = coh, "Subgroup size variation"=subgroup_var, "size" = size))
  
}



str(dt)
dt$Fragmentation <- ifelse(dt$fragmentation < 2, "< 2",  ">2 and <2.5 ")
p1<- ggplot(data=dt, aes(y=size, x=cohesion, color=Fragmentation, shape= Fragmentation)) + geom_point(size=1.2)+
  scale_colour_manual(values = c("#ff7f00", "#a65628"))+
  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8))+
  theme(legend.box = "vertical", legend.position = c(0.25, 0.22),  legend.direction="vertical", legend.text=element_text(size=6),legend.key = element_blank(), legend.key.size = unit(2, "mm"))+
  theme(legend.title = element_text(size = 6))+
  xlab("Subgroup cohesion")+ylab(expression(paste(Delta, "Outbreak size")))+ylim(-100, 0)+xlim(0,1)+
  theme(legend.background = element_rect(fill="transparent"))  +
  annotate("text", x = 0.003, y = 0, label = "A", size = 2)
p1

colfunc2 <- colorRampPalette(c("red", "black"),space="rgb") 
p2<- ggplot(data=dt, aes(y=size, x=cohesion, color=Subgroup.size.variation, size= Subgroup.size.variation)) + geom_point()+
  theme_bw() +theme(panel.grid.minor= element_blank(), panel.grid.major= element_blank(),text = element_text(size=8))+
  theme(legend.box = "vertical", legend.position = c(0.3, 0.22),  legend.direction="vertical",  legend.title = element_text(size = 6), legend.text=element_text(size=6),legend.key = element_blank(), legend.key.size = unit(2, "mm"))+
  scale_size_continuous(range = c(0.05,1.6)) +
  scale_color_gradientn(colours=colfunc2(10), limits=c(0,0.6))+
  theme(axis.title.y=element_blank())+
  xlab("Subgroup cohesion")+ylim(-100, 0)+xlim(0,1)+
  theme(legend.background = element_rect(fill="transparent"))  +
  guides(color=guide_legend(title="Subgroup size variation"), size = guide_legend(title="Subgroup size variation"))+
  annotate("text", x = 0.003, y = 0, label = "B", size = 2)
p2

p<- grid.arrange(p1, p2, ncol=2)
p
ggsave("disease_burden_by_MECHANISMS.eps", p, height = 5, width = 12, units = "cm", dpi = 1200)

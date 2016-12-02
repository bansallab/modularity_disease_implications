library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(lattice)
library(reshape)
library(RColorBrewer)   # for brewer.pal(...)
library(cowplot)
library(Hmisc)
library(stringr)
################################################################
ds <- read.csv("Invasion_time.csv", header=T)
str(ds)
ds$invasion <- ds$Invasiontime.thresh.20.
ds$module_sort <- factor(ds$module+1)
ds$Qrel <- ds$Qvalue/0.9
ds$wd_d_ratio <- ds$Qvalue + (1.0/ds$num_modules)
#######################
dm <- read.csv("Modules_uninfected_PS.csv", header=T)
str(dm)
dm$Qmax <- 1 - 1/dm$num_modules
dm$Qrel <- dm$Qvalue/dm$Qmax
dm$Qrel <- round(dm$Qrel, 2)
dm$wd_d_ratio <- dm$Qvalue + (1.0/dm$num_modules)
dm$mod_uninfected <- dm$Modules.uninfected...
dm$Transmissibility  <- factor(dm$Tvalue)
dm$Num_modules <-factor(dm$num_modules)
dm
#######################
dr <- read.csv("Epidemic_resilience.csv", header=T)
dr$Qmax <- 1 - 1/dr$num_modules
dr$Qrel <- dr$Qvalue/dr$Qmax
dr$Qrel <- round(dr$Qrel, 2)
dr$wd_d_ratio <- dr$Qvalue + (1.0/dr$num_modules)
dr$mod_uninfected <- dm$mod_uninfected
str(dr)
#################################################################
colfunc <- colorRampPalette(colors = (brewer.pal(9,"Greens")))

p1 <- ggplot(ds, aes(x=module_sort , y= invasion, group=wd_d_ratio,colour = wd_d_ratio)) + geom_line(size=0.3)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=8),legend.title=element_text(size=6), legend.position=c(0.2,0.74),legend.text=element_text(size=5), legend.key = element_rect(size=0.3),legend.key.size = unit(1.4, "mm"), axis.title=element_text(size=8))+
  scale_colour_gradientn(colours = colfunc(11), limits=c(0, 1))+
  geom_errorbar(aes(ymin= invasion-SE, ymax= invasion+SE),  width=.02, size=0.3)+ geom_point(size=1) +  guides(colour = guide_colourbar(title="Cohesion"))+
  theme(plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
  theme(plot.margin=unit(c(.2 , 0.1, 0, 0.02), "cm"))+
  xlab("Ordered subgroups")+ylab("Time to disease invasion")+ggtitle('A')
p1

p2 <- ggplot(dr, aes(x= wd_d_ratio , y= mod_uninfected)) + geom_line(color="grey85")+
  geom_point(aes(size=Proportion_outbreak))+ scale_size(range = c(0.4, 2))+
  theme_bw() +  theme(legend.key=element_rect(colour = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=8),legend.title=element_text(size=6), legend.position=c(0.35,0.76),legend.text=element_text(size=5), legend.key = element_rect(size=0.4),legend.key.size = unit(1.8, "mm"), axis.title=element_text(size=8))+
  geom_errorbar(aes(ymin= mod_uninfected-SE, ymax= mod_uninfected+SE),  width=.02, size=0.3)+ xlim(0,1)+
  theme(plot.title=element_text(hjust=0.02, size=6, margin=margin(b = -8, unit = "pt")))+
  xlab("Subgroup cohesion")+ylab("Uninfected subgroups (%)")+
  theme(plot.margin=unit(c(.2 , 0.2, 0, 0.1), "cm"))+
  guides(size=guide_legend(title="Percent outbreaks"))+ggtitle('B')

p2

p<-plot_grid(p1, p2, align='h', ncol=2)
p
ggsave("4_Structural_delay_trapping.eps", p, height = 4.5, width = 8.7, units = "cm", dpi = 1200)

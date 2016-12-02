library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(lattice)
library(reshape)
library(RColorBrewer)   # for brewer.pal(...)
library(cowplot)
library(Hmisc)
library(segmented)
################################################################
ds <- read.csv("Epidemic_size_for_contour.csv", header=T)
str(ds)
ds
ds$Qrel <- ds$Qvalue/0.992
ds$episize <- ds$Epidemic_size...
max.episize <- max(ds$episize)
ds$norm.episize <- (ds$episize*100)/max.episize

dd <- read.csv("Epidemic_duration_for_contour.csv", header=T)
str(dd)
describe(dd)
dd$Qrel <- dd$Qvalue/0.992
max.duration <- max(dd$Duration)
dd$norm.duration <- (dd$Duration*100)/(max.duration)

dt <- read.csv("Numerical_threshold_PS.csv", header=T)
str(dt)
dt$Qrel <- dt$Qvalue/0.992

dl <- read.csv("Local_vs_global_burden_July2016.csv", header=T)
str(dl)
dl$Qrel <- dl$Qvalue/0.992
dl$norm.burden1 <- (dl$burden/10000)*100
dl$norm.burden2 <- ((dl$burden/10000)/(dl$episize/100))*100


Qrel.list <- setdiff(ds$Qrel, c(0))
Tlist <- unique(ds$Tvalue)

dq <- data.frame("Qrel" = double(), "Tvalue"=double(), "size_diff"=double())
dnull<-subset(ds, ds$Qrel==0)
for (Q in Qrel.list){
  Tmin <- subset(dt, dt$Qrel==Q)$Threshold
  ds1 <- subset(ds, ds$Qrel==Q)
  for (Tr in Tlist){
    emp.size <- subset(ds1, ds1$Tvalue==Tr)$episize
    null.size<-subset(dnull, dnull$Tvalue==Tr)$episize
    size.diff <- (100*(null.size - emp.size))/null.size
    size.diff <- ifelse(Tr <=Tmin, 0, size.diff)
    print (paste(Q, Tr, Tmin, emp.size, null.size, size.diff))
    dq <- rbind(dq, data.frame("Qrel" = Q, "Tvalue" =Tr, "size_diff"=size.diff))
  }
}
dq
dq$size_diff <- ifelse(!is.finite(dq$size_diff), 0, dq$size_diff)
dq$size_diff <- ifelse(dq$size_diff <0, 0, dq$size_diff)
dq
#####################
de <- read.csv("Epidemic_size.csv", header=T)
de1 <-subset(de, Graph=="geometric")
str(de1)
de1$Qmax <- 1 - 1/de1$Num_modules
de1$Qrel <- de1$Qvalue/de1$Qmax
de1$Qrel <- round(de1$Qrel, 2)
de1$wd_d_ratio <- de1$Qvalue + (1.0/de1$Num_modules)
de1$episize <-de1$Epidemic_size...
max.episize <- max(de1$episize)
de1$norm.episize <- (de1$episize*100)/max.episize
de1$norm.episize <-ifelse(de1$norm.episize>0, de1$norm.episize, NA)
de1$Transmissibility <- factor(de1$Tvalue)
de1$Modularity <-de1$Qvalue
de1$Num_modules <-factor(de1$Num_modules)
de1$module_tag <- ifelse(de1$Num_modules=="2", "Fragmentation = 0.69",
                         ifelse(de1$Num_modules=="5", "Fragmentation = 1.61",
                                ifelse(de1$Num_modules=="10", "Fragmentation = 2.30",  
                                       ifelse(de1$Num_modules=="100", "Fragmentation = 4.60", "Fragmentation = 4.83"))))

de1$module_tag <- factor(de1$module_tag, c("Fragmentation = 0.69", "Fragmentation = 1.61","Fragmentation = 2.30","Fragmentation = 4.60", "Fragmentation = 4.83"))
de2 <-subset(de1, de1$Transmissibility ==0.1)
de2 <-subset(de2, de2$Num_modules %in% c("10", "100", "125")) 
max.episize <- max(de2$episize)
de2$episize <-ifelse(de2$episize>0, de2$episize, NA)
de2$norm.episize <- (de2$episize*100)/max.episize

####################################################################333
##ttest  mod vs empirical
Qrel.list <- setdiff(unique(ds$Qrel), c(0))
Qrel.list
dqline <-data.frame("Tvalue"=double(), "Qthresh" = double())
Tlist <- unique(ds$Tvalue)
for (Tr in Tlist){
  dq1 <-subset(dq, dq$Tvalue==Tr)
  qthresh <- c()
  for (Q in Qrel.list){
    Tmin <- subset(dt, dt$Qrel==Q)$Threshold
    diff <- subset(dq1, dq1$Qrel==Q)$size_diff
    print(paste(Tr,Q, is.finite(diff)))
    if ((is.finite(diff)) &(diff>10) &(Tr>=Tmin)){qthresh <- c(qthresh, Q)}
  }
  if (length(qthresh)>0){minq <- min(qthresh)
  dqline <- rbind(dqline, data.frame("Tvalue"=Tr, "Qthresh" = minq))
  }
}

#####################################################################
colfunc1 <- colorRampPalette(c("white", "red", "black"),space="rgb") 
#line color = color="#7f7fff"
colfunc2 <- colorRampPalette(c("white", "#3182bd", "black"),space="rgb") 


str(ds)

p1 <- ggplot(ds, aes(x=Qrel, y=Tvalue))  + 
  geom_tile(aes(fill = norm.episize)) +geom_line(data=dt, aes(x=Qrel, y=Threshold), size=0.8, color="yellow")+
  theme_bw() +theme(panel.grid.minor = element_blank(),  panel.grid.major = element_blank(), text = element_text(size=7), axis.title=element_text(size=8), legend.title=element_blank())+
  scale_fill_gradientn(colours=colfunc2(10), limits=c(0,100))+
  guides(fill = guide_legend(keywidth = 0.2, keyheight = 0.7))+ 
  theme(plot.margin=unit(c(0.25, 0.6, 0, -0.05), "cm"), legend.position=c(1.14, 0.5))+
  theme(legend.text=element_text(size=4))+annotate("text", x = 0.93, y = 0.04, label = "A", colour="black", size = 2)+
  xlab(expression(paste("Relative modularity, ", italic(Q[rel])))) + ylab("Pathogen transmissibility")

p1

p2 <- ggplot(dq, aes(y=Qrel, x=Tvalue))  + 
  geom_tile(aes(fill = size_diff)) + theme_bw() +
  geom_line(data=dqline, aes(x=Tvalue, y=Qthresh), size=0.8, color="#b2df8a")+
  theme(panel.grid.minor = element_blank(),  panel.grid.major = element_blank(), text = element_text(size=7), axis.title=element_text(size=8), legend.title=element_blank())+
  scale_fill_gradientn(colours=colfunc1(20), limits=c(0,60))+
  guides(fill = guide_legend(keywidth = 0.2, keyheight = 0.7))+
  theme(plot.margin=unit(c(0.25, 0.6, 0.05, -0.05), "cm"), legend.position=c(1.14, 0.5))+
  theme(legend.text=element_text(size=4))+annotate("text", x = 0.2, y = 0.07, label = "B", colour="black", size = 2)+
  ylab(expression(paste("Relative modularity, ", italic(Q[rel])))) + xlab("Pathogen transmissibility")

p2


p3 <- ggplot(dl, aes(x=Qrel , y= norm.burden2, group=Type, color=Type)) + geom_line(size=0.3)+ geom_point(size=0.5)+
  #p3 <- ggplot(dl, aes(x=Qrel , y= norm.burden1, fill=Type)) + geom_bar(stat = "identity", width=0.065)+
  theme_bw() + 
  theme(legend.key=element_rect(colour = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=8),legend.title=element_blank(), legend.position=c(0.5,0.9),legend.text=element_text(size=7), legend.background = element_rect(fill="transparent"),legend.key.size = unit(3, "mm"))+
  theme(axis.title=element_text(size=8))+
  theme(plot.margin=unit(c(0.2, 0,-0.04, 0), "cm"))+
  xlab(expression(paste("Relative modularity, ", italic(Q[rel]))))+ ylim(0,100)+
  xlim(-0.05,1) +ylab("Transmission events (%)") +
  annotate("text", x = 0.95, y = 2, label = "C", size = 2)
p3

p4 <- ggplot(de2, aes(y = norm.episize, x = wd_d_ratio, color=module_tag, group= module_tag, shape=module_tag)) + geom_line(size=0.2)+ geom_point(size= 1)+
  theme_bw() +theme(panel.grid.minor = element_blank(),  panel.grid.major = element_blank(),text = element_text(size=8),  axis.title=element_text(size=8))+
  geom_errorbar(aes(ymin= norm.episize-SE, ymax=  norm.episize+SE),  width=.02, size=0.3)+ 
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a"))+
  xlab("Subgroup cohesion")+ylab("Outbreak size")+ xlim(0, 1) + ylim(0, 100)+
  theme(plot.margin = unit(c(0.2, 0.05, -0.04, 0), "cm"), legend.box = "vertical", legend.position = c(0.42, 0.22),  legend.direction="vertical", legend.text=element_text(size=6),legend.key = element_blank(), legend.key.size = unit(2, "mm"))+
  theme(legend.title = element_blank(),  legend.background = element_rect(fill="transparent"))+
  annotate("text", x = 0.95, y =2, label = "D", size = 2)
p4




p<- grid.arrange(p1, p2,  p3, p4, nrow=2, ncol=2)
p
cairo_ps("Qthreshold_vs_pathogen_robustness.eps",  height =3, width = 3.42)
p<- grid.arrange(p1, p2,  p3, p4, nrow=2, ncol=2)
dev.off()
#ggsave("3_Contour_disease_burden.eps", p, height = 7.5, width = 8.7, units = "cm", dpi = 1200)

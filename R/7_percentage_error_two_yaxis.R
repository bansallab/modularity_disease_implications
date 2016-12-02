library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(lattice)
library(cowplot)
library(MASS)
library(Hmisc)
library(car)
library(gtable)
###################################################################3
de <- read.csv("Empirical_disease_burden.csv", header=T)
str(de)
de$episize <- de$Epidemic_size...
de$Network <- factor(de$Network)
dm <- read.csv("Matched_modules_disease_burden.csv", header=T)
dm$episize <- dm$Epidemic_size...
ds <- read.csv("Simple_random_disease_burden.csv", header=T)
ds$episize <- ds$Epidemic_size...
##############################################
select <- c("2", "5", "6", "7","10","15","17","18","26","29","33","35","41","43","54","57", "62", "63", "66")
####################################
##ttest  mod vs empirical
dt <- data.frame("Graph"=character(),"Qrel" = double(), "type" = character(),"perc_error" = double())
for (graph in select){
  emp.size <-subset(de,Network==graph)$episize
  net.size <-subset(dm,Network==graph)$episize
  rnd.size <-subset(ds, Network==graph)$episize
  err_mod <- ((emp.size - net.size)*100)/emp.size
  err_rnd <- ((emp.size - rnd.size)*100)/emp.size
  Q <- subset(de,Network==graph)$Qrel
  dt <-rbind(dt, data.frame("Graph"= graph,"Qrel" = Q ,"type"="Modular null networks" , "perc_error" = err_mod))
  dt <-rbind(dt, data.frame("Graph"= graph, "Qrel" = Q , "type"="Homogeneous null networks" ,"perc_error" = err_rnd))
  
}
######################################################################
dt$species <-ifelse(dt$Graph=="2", "CP2",
                    ifelse(dt$Graph=="5", "CF3",
                           ifelse(dt$Graph=="6", "CF4",
                                  ifelse(dt$Graph== "7", "CF5",
                                         ifelse(dt$Graph=="10", "DR",
                                                ifelse(dt$Graph=="12", "MAG2",
                                                       ifelse(dt$Graph=="13", "MAG3",
                                                              ifelse(dt$Graph=="15", "TT1",
                                                                     ifelse(dt$Graph=="17", "TT3",
                                                                            ifelse(dt$Graph=="18", "MA",
                                                                                   ifelse(dt$Graph=="26", "CCR",
                                                                                          ifelse(dt$Graph=="29", "TR",
                                                                                                 ifelse(dt$Graph=="33", "BA",
                                                                                                        ifelse(dt$Graph=="35", "CC",
                                                                                                               ifelse(dt$Graph=="41", "MM1",
                                                                                                                      ifelse(dt$Graph=="43", "MT",
                                                                                                                             ifelse(dt$Graph=="54", "PC5",
                                                                                                                                    ifelse(dt$Graph=="57","HM",
                                                                                                                                           ifelse(dt$Graph=="62","BB",
                                                                                                                                                  ifelse(dt$Graph=="63","DC","MF"
                                                                                                                                                         
                                                                                                                                                  ))))))))))))))))))))

head(dt)
dt
#####################################################################
grid.newpage()
dodge <- position_dodge(width=0.5)
p1 <- ggplot(data=dt, aes(x=reorder(species, Qrel), y= perc_error, group=type, fill=type)) + 
  geom_bar(stat="identity", position = dodge, width=0.4)+ theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=7), axis.title=element_text(size=7))+
  theme(legend.key = element_blank(), legend.title= element_blank(),  legend.position=c(0.3,0.2),legend.text=element_text(size=6), legend.key = element_rect(size=5),legend.key.size = unit(3, "mm"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept = 0, size=0.2)+
  annotate("rect", xmin=0.5, xmax=19.5, ymin=-15,ymax=15, fill="grey50", alpha=0.3) +
  xlab("Animal social networks") + ylab("Percentage error") + ylim(-75, 75)


p2 <- ggplot(data=dt, aes(x=reorder(species, Qrel), y= Qrel, group=1)) + 
  geom_line(color="red", size=0.4)+geom_point(size=0.4)+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=7), axis.title=element_text(size=6))+
  theme(axis.text.y=element_text(color="red"), axis.title.y=element_text(color="red"),  axis.ticks.y = element_line(colour = 'red'))+
  xlab("") + ylab(expression(paste("Relative modularity, ", italic(Q[rel])))) + ylim(-1,1)+theme(panel.background = element_rect(fill = NA))
  

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

pp<-c(subset(g1$layout,name=="panel",se=t:r))
g<-gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]],pp$t,pp$l,pp$b,pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

ia2 <- which(g2$layout$name == "ylab")
ga2 <- g2$grobs[[ia2]]
ga2$rot <- 90
g <- gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)

grid.draw(g)

cairo_ps("7_modfit_predictions.eps",  height = 2, width = 3.42, fallback_resolution =  1200)
grid.draw(g)
dev.off()



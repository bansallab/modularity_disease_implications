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
de <- read.csv("Empirical_disease_burden_all_ro.csv", header=T)
str(de)
de$episize <- de$Epidemic_size...
de$Network <- factor(de$Network)
dm <- read.csv("Matched_modules_disease_burden_by_R0.csv", header=T)
dm$episize <- dm$Epidemic_size...
str(dm)
ds <- read.csv("Simple_random_disease_burden_all_ro.csv", header=T)
ds$episize <- ds$Epidemic_size...
str(ds)
dp <- read.csv("Network_mechanism_modularity_10Oct2016.csv", header=T)
dp$Qrel <- ifelse(dp$Qmax>0, dp$Q/dp$Qmax, 0)
##############################################
select_net <- c("2", "5", "6", "7","10","15","17","18","26","29","33","35","41","43","54","57", "62", "63", "66")
plot_list = list()
rolist <- c(1.05, 1.45, 1.8, 2)
title <- c("A", "B", "C", "D")
i=1
for (ro in rolist){
  
  de1 <-subset(de, de$R0==ro)
  ds1 <- subset(ds, ds$R0==ro)
  dm1 <- subset(dm, dm$R0==ro)
  do_select_smp <- ds1$Network
  do_select_emp <- de1$Network
  do_select_mod <- dm1$Network
  ##select all networks that ran in empirical and simple random
  do_select <- intersect(do_select_emp, select_net)
  do_select <- intersect(do_select, do_select_smp)
  do_select <- intersect(do_select, do_select_mod)
  print (ro)
  print (do_select)
  ####################################
  ##ttest  mod vs empirical
  dt <- data.frame("Graph"=character(),"Qrel" = double(), "type" = character(),"perc_error" = double(), "species"=character())
  for (graph in do_select){
    emp.size <-subset(de1,Network==graph)$episize
    net.size <-subset(dm1,Network==graph)$episize
    rnd.size <-subset(ds1, Network==graph)$episize
    err_mod <- ifelse(emp.size>0,((emp.size - net.size)*100)/emp.size,0)
    err_rnd <- ifelse(emp.size>0,((emp.size - rnd.size)*100)/emp.size,0)
    Q <- subset(dp, Graph.==graph)$Qrel
    species <- subset(dp, Graph.==graph)$abbr
    print (Q)
    print (paste(ro, graph, emp.size, net.size, rnd.size, err_mod, species))
    dt <-rbind(dt, data.frame("Graph"= graph,"Qrel" = Q ,"type"="Modular null networks" , "perc_error" = err_mod, "species"=species))
    dt <-rbind(dt, data.frame("Graph"= graph, "Qrel" = Q , "type"="Homogeneous null networks" ,"perc_error" = err_rnd, "species"=species))
    
  }
  ######################################################################
  #####################################################################
  grid.newpage()
  dodge <- position_dodge(width=0.5)
  p1 <- ggplot(data=dt, aes(x=reorder(species, Qrel), y= perc_error, group=type, fill=type)) + 
    geom_bar(stat="identity", position = dodge, width=0.4)+ theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=7), axis.title=element_text(size=7))+
    theme(legend.key = element_blank(), legend.title= element_blank(),  legend.position=c(0.3,0.2),legend.text=element_text(size=6), legend.key = element_rect(size=5),legend.key.size = unit(3, "mm"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(yintercept = 0, size=0.2)+ ggtitle(paste("Basic reproduction number =", as.character(ro)))+
    annotate("rect", xmin=0.5, xmax=19.5, ymin=-15,ymax=15, fill="grey50", alpha=0.3) +
    xlab("Animal social networks") + ylab("Percentage error") + ylim(-100, 100)
  
  
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
  
  plot_list[[i]] = g
  i=i+1
}  
  
dt  
cairo_ps("7_EXTRA_modfit_predictions.eps",  height = 5, width =6)
p<- grid.arrange(plot_list[[1]],plot_list[[2]], plot_list[[3]],plot_list[[4]], ncol = 2)
grid.draw(p)
dev.off()



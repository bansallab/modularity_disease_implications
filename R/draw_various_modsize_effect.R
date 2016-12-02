library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(lattice)
library(reshape)
library(RColorBrewer)   # for brewer.pal(...)
library(cowplot)
library(Hmisc)
################################################################
ds <- read.csv("Epidemic_size_various_modsize.csv", header=T)
str(ds)
ds$Qmax <- 1 - 1/ds$Num_modules
ds$Qrel <- ds$Qvalue/ds$Qmax
ds$Qrel <- round(ds$Qrel, 2)
ds$wd_d_ratio <- ds$Qvalue + (1.0/ds$Num_modules)
ds$episize <-ds$Epidemic_size...
ds$Transmissibility <- factor(ds$Tvalue)
ds$Num_modules <-factor(ds$Num_modules)
ds$Module_type <- ifelse(ds$Mod.type=="CONSTANT", "Constant",
                         ifelse(ds$Mod.type=="POISSON", "Homogenous", "Heterogenous"))

ds$Module_type <- factor(ds$Module_type, levels = c("Constant", "Homogenous", "Heterogenous"))       
max.episize <- max(ds$episize)
ds$norm.episize <- (ds$episize*100)/max.episize
################################################################
dd <- read.csv("Epidemic_duration_various_modsize.csv", header=T)
str(dd)
describe(dd)
dd$Qmax <- 1 - 1/dd$num_modules
dd$Qrel <- dd$Qvalue/dd$Qmax
dd$Qrel <- round(dd$Qrel, 2)
dd$wd_d_ratio <- dd$Qvalue + (1.0/dd$num_modules)
dd$Transmissibility <- factor(dd$Tvalue)
dd$Num_modules <-factor(dd$num_modules)
max.duration <- max(dd$Duration)
dd$norm.duration <- (dd$Duration*100)/(max.duration)
#################################################################
#overall Q
ds
p1 <- ggplot(ds, aes(x=Qrel , y=norm.episize, shape=Module_type))+ geom_line(colour="darkgray", size=0.3)+
  theme_bw() + geom_point(size=1.2)+
  theme(panel.grid.major= element_blank(), panel.grid.minor = element_blank(), text = element_text(size=10), legend.key = element_blank(), legend.position=c(0.4,0.2),legend.title=element_text(size=7), legend.text=element_text(size=5), legend.key.size = unit(3, "mm"), axis.title=element_text(size=10))+
  geom_errorbar(aes(ymin=norm.episize-SE, ymax= norm.episize+SE),  width=.02, size=0.3)+ 
  xlab(expression(paste("Relative modularity, ", italic(Q[rel]))))+ylab("Outbreak size (%)")+ xlim(-0.1, 1) + ylim(50, 100)  + labs(shape="Subgroup size variation")

p1
p2 <- ggplot(dd, aes(x=Qrel, y=norm.duration, shape=Mod.type))+ geom_line(colour="darkgray", size=0.3)+
  theme_bw() +geom_point(size= 1.2)+
  theme(panel.grid.major= element_blank(),panel.grid.minor = element_blank(), text = element_text(size=10), legend.position="none", axis.title=element_text(size=10))+
  geom_errorbar(aes(ymin=norm.duration-SE, ymax= norm.duration+SE),  width=.02, size=0.3)+ 
  xlab(expression(paste("Relative modularity, ", italic(Q[rel]))))+ylab("Outbreak duration")+ xlim(-0.1, 1) + ylim(50,100)

p<-plot_grid(p1, p2, align='h', ncol=2, labels=c('A', 'B'), label_size = 8, hjust = -0.8)
p

ggsave("Effect_variable_modsize_distribution.eps", p,  height = 6.2, width = 11.4, units = "cm", dpi = 1200)


#####################################################################3
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

####################################################################
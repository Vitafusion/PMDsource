
##################################################### 
# Plotting Accuracy Test Result For The SIM Method  #
#####################################################


library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)
source('Sec4.3/code/simulation_fun.R')


# loading data
dat.simu <- read.table("Sec4.3/data/simu.txt",sep = '\t')
colnames(dat.simu)[3] <- 'b'
dat.simu$b <- as.factor(dat.simu$b)



################################################################################
# Plotting


p1 <- SIM.plot(dat.simu, y.name='err.max', subtitle='mode')
p2 <- SIM.plot(dat.simu, y.name='err.95', subtitle='0.95')
p3 <- SIM.plot(dat.simu, y.name='err.90', subtitle='0.9')

ggarrange(p1,p2,p3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

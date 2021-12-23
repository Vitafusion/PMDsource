
###########################################
# Plotting Accuracy Results For NA Method #
###########################################


library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)
library(reshape2)

source("Sec4.2/code/norm_fun.R")


# load data
m3 <- read.table("Sec4.2/data/norm_m3.txt", sep="\t")
m5 <- read.table("Sec4.2/data/norm_m5.txt", sep="\t")
m7 <- read.table("Sec4.2/data/norm_m7.txt", sep="\t")

# data reformat
dat.norm <- rbind(m3,m5,m7)
colnames(dat.norm) <- c('n','m','N.A','Baseline')
dat.norm <- melt(dat.norm ,  id.vars = c('n','m'), 
                 measure.vars = c('N.A','Baseline'), 
                 variable.name = 'method',
                 value.name = 'mae')



# plotting
p3 <- norm.plot(dat.norm, m=3, 10,90)


p5 <- norm.plot(dat.norm, m=5, 10,50)



p7 <- norm.plot(dat.norm, m=7, 3,15)


setEPS()
postscript("Sec4.2/plot/normal_mae.eps")
ggarrange(p3,p5,p7, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()



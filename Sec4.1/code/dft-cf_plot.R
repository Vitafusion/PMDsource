##################################################
# Plotting Accuracy Result for The DFT-CF Method #
##################################################

library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)
source("dft-cf_fun.R")

# load data
dat.bino <- read.table("../data/binomial.txt")
dat.poi <- read.table("../data/poibiom.txt")



# binomial
dat.bino <- dat.bino[10*c(1:10),]

p1 <- dft.mae.plot(dat.bino)
p2 <- dft.tae.plot(dat.bino)

setEPS()
postscript("Sec4.1/plotbino_mae.eps")
p1
dev.off()


setEPS()
postscript("Sec4.1/plotbino_tae.eps")
p2
dev.off()




# Poison binomial

p3 <- dft.mae.plot(dat.poi)
p4 <- dft.tae.plot(dat.poi)


setEPS()
postscript("Sec4.1/plot/poi_mae.eps")
p3
dev.off()


setEPS()
postscript("Sec4.1/plotpoi_tae.eps")
p4
dev.off()



##################################################
# Plotting Accuracy Result for The DFT-CF Method #
##################################################

library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)


# load data
dat.bino <- read.table("Sec4.1/data/binomial.txt")
dat.poi <- read.table("Sec4.1/data/poibiom.txt")



# binomial
dat.bino <- dat.bino[10*c(1:10),]

p1 <- dat.bino %>% ggplot() + 
  geom_path(aes(x=n,y=mae), size=.8) + ylab('MAE') + 
  scale_y_continuous(name='MAE',limits=c(4e-11,1e-10)) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.text.y = element_text(face="bold", 
                                   size=15),
        axis.title=element_text(size=16,face="bold")) 

p1
p2 <- dat.bino %>% ggplot() + 
  geom_path(aes(x=n,y=tae), size=.8) + ylab('TAE') +
  scale_y_continuous(name='TAE',limits=c(1e-9,1e-8)) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.text.y = element_text(face="bold", 
                                   size=15),
        axis.title=element_text(size=16,face="bold")) 

p2


setEPS()
postscript("bino_mae.eps")
p1
dev.off()


setEPS()
postscript("bino_tae.eps")
p2
dev.off()

# Poison binomial

p3 <- dat.poi %>% ggplot() + 
  geom_path(aes(x=n,y=mae), size=.8) + ylab('MAE') + 
  scale_y_continuous(name='MAE',limits=c(4e-11,1e-10)) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.text.y = element_text(face="bold", 
                                   size=15),
        axis.title=element_text(size=16,face="bold")) 

p3
p4 <- dat.poi %>% ggplot() + 
  geom_path(aes(x=n,y=tae), size=.8) + ylab('TAE') +
  scale_y_continuous(name='TAE',limits=c(1e-9,1e-8)) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.text.y = element_text(face="bold", 
                                   size=15),
        axis.title=element_text(size=16,face="bold")) 

p4


setEPS()
postscript("poi_mae.eps")
p3
dev.off()


setEPS()
postscript("poi_tae.eps")
p4
dev.off()



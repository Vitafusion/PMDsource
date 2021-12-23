
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




p3 <- dat.norm %>% filter(m==3) %>% filter(n>=10 & n<=90) %>%ggplot(aes(x=n, y=mae)) + 
  geom_path(aes(color=method, linetype=method)) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = 'm=3') + 
  ylab("MAE") + theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(name="Method",
                     labels=c("NA", "Baseline"),
                     values=c("blue","red")) +
  scale_linetype_manual(name="Method",
                        labels=c("NA", "Baseline"),
                        values=c("solid", "dashed")) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=8),
        axis.text.y = element_text(face="bold", 
                                   size=8),
        axis.title=element_text(size=10,face="bold")) 


p5 <- dat.norm %>% filter(m==5) %>% filter(n>=10 & n<=50) %>% ggplot(aes(x=n, y=mae)) + 
  geom_path(aes(color=method, linetype=method)) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = 'm=5') + 
  ylab("MAE") + theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(name="Method",
                     labels=c("NA", "Baseline"),
                     values=c("blue","red")) +
  scale_linetype_manual(name="Method",
                        labels=c("NA", "Baseline"),
                        values=c("solid", "dashed")) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=8),
        axis.text.y = element_text(face="bold", 
                                   size=8),
        axis.title=element_text(size=10,face="bold"))



p7 <- dat.norm %>% filter(m==7, n>=3 & n <= 15) %>% ggplot(aes(x=n, y=mae)) + 
  geom_path(aes(color=method, linetype=method)) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = 'm=7') + 
  ylab("MAE") + theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(name="Method",
                     labels=c("NA", "Baseline"),
                     values=c("blue","red")) +
  scale_linetype_manual(name="Method",
                        labels=c("NA", "Baseline"),
                        values=c("solid", "dashed")) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=8),
        axis.text.y = element_text(face="bold", 
                                   size=8),
        axis.title=element_text(size=10,face="bold"))



ggarrange(p3,p5,p7, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


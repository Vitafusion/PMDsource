



library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)
library(reshape2)

m3 <- read.table("Sec4.2/data/norm_m3.txt", sep="\t")
m5 <- read.table("Sec4.2/data/norm_m5.txt", sep="\t")
m7 <- read.table("Sec4.2/data/norm_m7.txt", sep="\t")


dat.norm <- rbind(m3,m5,m7)
colnames(dat.norm) <- c('n','m','N.A','Baseline')
dat.norm <- melt(dat.norm ,  id.vars = c('n','m'), 
                 measure.vars = c('N.A','Baseline'), 
                 variable.name = 'method',
                 value.name = 'mae')




base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

scaleFUN <- function(x) sprintf("%.4f", x)


dat.norm %>% filter(m==3) %>% filter(n>=10 & n<=90) %>%ggplot(aes(x=n, y=mae)) + 
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


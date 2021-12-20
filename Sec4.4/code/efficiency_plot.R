################################################### 
# Plotting efficiency test result for the DFT_CF  #
###################################################


# Loading packages and data

library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)
source('efficiency_fun.R')

m2 <- read.table('Sec4.4/data/effi_m2.txt')
m3 <- read.table('Sec4.4/data/effi_m3.txt')
m4 <- read.table('Sec4.4/data/effi_m4.txt')
m5 <- read.table('Sec4.4/data/effi_m5.txt')

################################################################################
# Plotting

p2 <- effi.plot(m2, 'm=2', seq(0,0.27,0.03))
p3 <- effi.plot(m3, 'm=3', seq(0,0.5,0.1))
p4 <- effi.plot(m4, 'm=4', seq(0,16,2))
p5 <- effi.plot(m5, 'm=5', seq(0,100,10))

# combine plots into one plot
ggarrange(p2, p3, p4, p5, ncol=4, nrow=1, common.legend = TRUE, legend="bottom")


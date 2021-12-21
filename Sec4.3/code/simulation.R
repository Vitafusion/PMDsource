
############################################# 
#  Accuracy Test Result For The SIM Method  #
#############################################



library(poissonmulti)
source("simulation_fun.R")
K <- 100
m <- 3
b <- 10


args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("seed: ", seed, "\n", sep="")

n <- seed


if(seed > 10 && seed <= 20){
  b <- 10^5
  n <- seed - 10
} else if(seed > 20){
  b <- 10^7
  n <- seed -20
}


result <- simulation.accuracy(n, m, b, K)
save(result, file=paste("sim_", seed, ".RData", sep=""))




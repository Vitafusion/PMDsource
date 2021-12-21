library(poissonmulti)

K <- 100
m <- 3

args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("i: ", i, "\n", sep="")
cat("B is ", b, "\n", sep="")


n <- i
res <- simulation.accuracy(n, m, b, K)
save(res, file=paste("sim_", i, "_", b, ".RData", sep=""))




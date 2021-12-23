####################################
# Computing Accuracy For NA Method #
####################################



library(PoissonMultinomial)
source("norm_fun.R")


args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("seed: ", seed, "\n", sep="")
cat("n: ", n, "\n", sep="")
cat("m: ", m, "\n", sep="")
cat("K: ", K, "\n", sep="")

multi <- 10
if(m==7){
  multi <- 3
}


# initialize
results <- matrix(0, nrow = 1, ncol = 4)
results <- as.data.frame(results)
colnames(results) <- c("n", "m", "MAE", "baseline")


# compute mae and baseline
for(i in 1:n){
  res <- norm.accuracy(multi*i, m, K)
  results <- rbind(results,res)
}


# remove the first row
results <- results[-1,]

save(results, file=paste("norm_", m, "_", seed, ".RData", sep=""))


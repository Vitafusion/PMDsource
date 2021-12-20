# For computing efficiency, we use parallel computing here. All computation is running on Virginia Tech
# ARC server Tinkercliffs.

###################################################################################################

# loading parallel and PMD packages
library(foreach)
library(doParallel)
library(PoissonMultinomial)
source('efficiency_fun.R')

# parallizing 
detectCores()
cl = makeCluster(100)
registerDoParallel(cl)

k <- 1000 # repeat 1000 times for each case.

###################################################################################################


###################################################################################################
# Let m=2 and n goes from 10 to 1000, computing each combination k times to conclude average timing

n <- c(1:100)
N <- length(n)
m <- 2

results <- matrix(0, nrow = N, ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c('n', 'm', 'time')

for (i in n) {
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PoissonMultinomial',.combine = 'rbind') %dopar% {
    pmd.effi(10*i,m)
  }
  results$`n`[i] <- 10*i
  results$`m`[i] <- m
  results$`time`[i] <- mean(res[,3])
  print(results[i,])
}

write.table(results,'effi_m2.txt', sep = '\t')


###################################################################################################
# Let m=3 and n goes from 10 to 100, computing each combination k times to conclude average timing

n <- c(1:10)
N <- length(n)
m <- 3

results <- matrix(0, nrow = N, ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c('n', 'm', 'time')

for (i in n) {
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PoissonMultinomial',.combine = 'rbind') %dopar% {
    pmd.effi(10*i,m)
  }
  results$`n`[i] <- 10*i
  results$`m`[i] <- m
  results$`time`[i] <- mean(res[,3])
  print(results[i,])
}

write.table(results,'effi_m3.txt', sep = '\t')


###################################################################################################
# Let m=4 and n goes from 10 to 70, computing each combination k times to conclude average timing

n <- c(1:7)
N <- length(n)
m <- 4

results <- matrix(0, nrow = N, ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c('n', 'm', 'time')

for (i in n) {
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PoissonMultinomial',.combine = 'rbind') %dopar% {
    pmd.effi(10*i,m)
  }
  results$`n`[i] <- 10*i
  results$`m`[i] <- m
  results$`time`[i] <- mean(res[,3])
  print(results[i,])
}

write.table(results,'effi_m4.txt', sep = '\t')





###################################################################################################
# Let m=5 and n goes from 10 to 40, computing each combination k times to conclude average timing

n <- c(1:4)
N <- length(n)
m <- 5

results <- matrix(0, nrow = N, ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c('n', 'm', 'time')

for (i in n) {
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PoissonMultinomial',.combine = 'rbind') %dopar% {
    pmd.effi(10*i,m)
  }
  results$`n`[i] <- 10*i
  results$`m`[i] <- m
  results$`time`[i] <- mean(res[,3])
  print(results[i,])
}

stopCluster(cl)
write.table(results,'effi_m5.txt', sep = '\t')
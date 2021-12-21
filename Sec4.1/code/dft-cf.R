###################################
# Test Accuracy for DFT-CF Method #
###################################

library(poibin)
library(PoissonMultinomial)
source("Sec4.1/code/dft-cf_fun.R")



# binomial distribution

m <- 2
n <- 1:100
N <- length(n)
K <- 1000

binom.res <- dft.bino(N, m, K)

write.table(binom.res, 'binomial.txt', sep = '\t')





# Poisson binomial distribution

m <- 2
n <- 1:100
N <- length(n)
K <- 1000

pbino.res <- dft.pbino(N, m, K)

write.table(pbino.res, 'poibiom.txt', sep = '\t')

###################################
# Test Accuracy for DFT-CF Method #
###################################

library(poibin)
library(PoissonMultinomial)




# binomial distribution

m <- 2
n <- 1:100
N <- length(n)
K <- 1000
binom.res <- matrix(0, nrow = N, ncol = 3)
binom.res <- as.data.frame(binom.res)
colnames(binom.res) <- c('n','mae','tae')
temp <- binom.res[1,2:3]


for(i in 1:N){
  for(k in 1:K){
    p.bino <- p.matrix(n=1, m=2)
    pp <- matrix(rep(p.bino,10*i),nrow=10*i,byrow=T)
    mm <- ncol(pp) # m categories
    nn <- nrow(pp) # n people
    res0 <- dpmd(pp)
    res1 <- dbinom(0:nn, nn, p.bino[1])
    temp[1] <- max(abs(res0-res1))
    temp[2] <- sum(abs(res0-res1))
    binom.res[i,2:3] <- temp + binom.res[i,2:3]
  }
  binom.res$`n`[i] <- nn 
  binom.res[i,2:3] <- binom.res[i,2:3]/K
}


write.table(binom.res, 'binomial.txt', sep = '\t')





# Poisson binomial distribution

m <- 2
n <- 1:100
N <- length(n)
K <- 1000
pbino.res <- matrix(0, nrow = N, ncol = 3)
pbino.res <- as.data.frame(pbino.res)
colnames(pbino.res) <- c('n','mae','tae')
temp <- pbino.res[1,2:3]


for(i in 1:N){
  for(k in 1:K){
    pp = p.matrix(10*i,m)
    mm=ncol(pp) # m categories
    nn=nrow(pp) # n people
    res0 = dpmd(pp)
    res1 = dpoibin(0:nn, pp[,1])
    temp[1] <- max(abs(res0-res1))
    temp[2] <- sum(abs(res0-res1))
    pbino.res[i,2:3] <- temp + pbino.res[i,2:3]
  }
  pbino.res$`n`[i] <- 10*i
  pbino.res[i,2:3] <- pbino.res[i,2:3]/K
}

write.table(pbino.res, 'poibiom.txt', sep = '\t')

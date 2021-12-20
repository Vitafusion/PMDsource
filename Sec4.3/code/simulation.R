library(PoissonMultinomial)

K <- 100
m <- 3
n <- c(1:10)
N <- length(n)
b <- c(10, 10^5, 10^7)
results <- matrix(0,nrow = 1 ,ncol = 9)
results <- as.data.frame(results)
colnames(results) <- c("n","m","B","max","err.max","per.75","err.75","per.50","err.50")
temp <- results
temp2 <- temp


args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("covariates index: ", covidx, "\n", sep="")
cat("seed is ", seed, "\n", sep="")




for (j in 1:length(b)) {
  for (i in 1:N) {
    temp2$B <- b[j]
    temp2$n <- 10*i
    temp2$m <- m
    for(k in 1:K){
      pp <- p.matrix(10*i,m)
      res0 <- pmd(pp)
      res1 <- pmd(pp, method = "simulation", t=b[j])
      
      index.max <- q.find(res0,1)
      index.75 <- q.find(res0,0.75)
      index.50 <-  q.find(res0,0.50)
      err.max <- abs(res0[index.max] - res1[index.max])
      err.75 <- abs(res0[index.75] - res1[index.75])
      err.50 <- abs(res0[index.50] - res1[index.50])
      temp$max <- res0[index.max]
      temp$err.max <- err.max
      temp$`per.75` <- res0[index.75]
      temp$err.75 <- err.75
      temp$`per.50` <- res0[index.50]
      temp$err.50 <- err.50
      temp2[,4:9] <- temp2[,4:9] + temp[,4:9]
    }
    temp2[,4:9] <- temp2[,4:9]/K
    results <- rbind(results,temp2)
    print(results)
  }
}

results

write.table(results, 'results75.txt', sep='\t')
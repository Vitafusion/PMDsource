library(PoissonMultinomial)

K <- 10
m <- 3
results <- matrix(NA,nrow = 1 ,ncol = 9)
results <- as.data.frame(results)
colnames(results) <- c("n","m","B","max","err.max","per.95","err.95","per.90","err.90")

temp <- matrix(0,nrow = 1 ,ncol = 6)
colnames(temp) <- c("max","err.max","per.95","err.95","per.90","err.90")


args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("i: ", i, "\n", sep="")
cat("seed is ", seed, "\n", sep="")



if(i>10){
  b <- 10^7
  n <- i-10
} else{
  n <- i
}



simulation.accuracy <- function(n,m,b,K){
  results <- matrix(NA,nrow = 1 ,ncol = 9)
  results <- as.data.frame(results)
  colnames(results) <- c("n","m","B","max","err.max","per.95","err.95","per.90","err.90")
  results$`n` <- n
  results$`m` <- m
  results$B <- b

  temp <- matrix(0,nrow = 1 ,ncol = 6)
  colnames(temp) <- c("max","err.max","per.95","err.95","per.90","err.90")

  for(k in 1:K){
        pp <- p.matrix(10*n,m)
        res0 <- dpmd(pp)
        res1 <- dpmd(pp, method = "SIM", B=10)
        
        index.max <- q.find(res0,1)
        index.95 <- q.find(res0,0.95)
        index.90 <-  q.find(res0,0.90)
        err.max <- abs(res0[index.max] - res1[index.max])
        err.95 <- abs(res0[index.95] - res1[index.95])
        err.90 <- abs(res0[index.90] - res1[index.90])
        temp$max <- temp$max + res0[index.max]
        temp$err.max <- temp$err.max + err.max
        temp$`per.75` <- temp$`per.75` + res0[index.75]
        temp$err.75 <- temp$err.75 + err.75
        temp$`per.50` <- temp$`per.50` + res0[index.50]
        temp$err.50 <- temp$err.50 + err.50
  }
  temp <- temp/K
  results[,4:9] <- temp

  return(results)
}

results$`n` <- 10*n
results$m <- m
results$B <- b


for(k in 1:K){
      pp <- p.matrix(10*n,m)
      res0 <- dpmd(pp)
      res1 <- dpmd(pp, method = "simulation", t=10)
      
      index.max <- q.find(res0,1)
      index.95 <- q.find(res0,0.95)
      index.90 <-  q.find(res0,0.90)
      err.max <- abs(res0[index.max] - res1[index.max])
      err.95 <- abs(res0[index.95] - res1[index.95])
      err.90 <- abs(res0[index.90] - res1[index.90])
      temp$max <- temp$max + res0[index.max]
      temp$err.max <- temp$err.max + err.max
      temp$`per.75` <- temp$`per.75` + res0[index.75]
      temp$err.75 <- temp$err.75 + err.75
      temp$`per.50` <- temp$`per.50` + res0[index.50]
      temp$err.50 <- temp$err.50 + err.50
}
temp <- temp/K
results <- rbind(results,temp2)


for(k in 1:K){
      pp <- p.matrix(10*i,m)
      res0 <- dpmd(pp)
      res1 <- dpmd(pp, method = "simulation", t=10^5)
      
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

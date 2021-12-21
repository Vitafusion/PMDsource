# generate P matrix
p.matrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r = runif(m)
    r = r/sum(r) #generate row
    r[1:(m-1)] = round(r[1:(m-1)],3)
    while(sum(r[1:(m-1)])>1){
      r = runif(m)
      r = r/sum(r) #generate row
      r[1:(m-1)] = round(r[1:(m-1)],3)
    }
    r[m] = 1-sum(r[1:(m-1)])
    p[i,] = r
  }
  return(p)
}

# binomial
dft.bino <- function(N, m, K){
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
  return(binom.res)
}


# Poisson binomial
dft.pbino <- function(N, m, K){
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
  return(pbino.res)
}





# plot mae 
dft.mae.plot <- function(dat){
  dat %>% ggplot() + 
  geom_path(aes(x=n,y=mae), size=.8) + ylab('MAE') + 
  scale_y_continuous(name='MAE',limits=c(4e-11,1e-10)) + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.text.y = element_text(face="bold", 
                                   size=15),
        axis.title=element_text(size=16,face="bold")) 
}


# plot tae
dft.tae.plot <- function(dat){
  dat %>% ggplot() + 
    geom_path(aes(x=n,y=tae), size=.8) + ylab('TAE') +
    scale_y_continuous(name='TAE',limits=c(1e-9,1e-8)) + 
    theme(axis.text.x = element_text(face="bold", 
                                     size=15),
          axis.text.y = element_text(face="bold", 
                                     size=15),
          axis.title=element_text(size=16,face="bold")) 
}
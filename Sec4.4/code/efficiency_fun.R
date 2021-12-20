# This file contains functions used in efficiency study of the DFT-CF method.


# generating P matrix
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


# computing time efficiency
pmd.effi <- function(n,m){
  pp <- p.matrix(n,m)
  t <- system.time(dpmd(pp))
  return(c(n,m,t[3]))
}


# plot
effi.plot <- function(dat, title, ybreaks){
  dat %>% ggplot() + geom_path(aes(x=n,y=time)) + ggtitle(title) + 
    ylab('Time in Seconds') +
    scale_y_continuous(breaks = ybreaks) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(face="bold", 
                                     size=8),
          axis.text.y = element_text(face="bold", 
                                     size=8),
          axis.title=element_text(size=10,face="bold")) 
}

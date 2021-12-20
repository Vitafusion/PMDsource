# find mode
q.find <- function(res,q){
  res.temp <- res[res!=0]
  q.temp <- quantile(res.temp,q)
  
  if(length(which(res==q.temp))>1){
    q.addr <- which(res==q.temp)[1]
  }
  else{
    q.addr <- which(res==q.temp)
  }

  if(length(q.addr)==0){
    dis <- abs(res.temp-q.temp)
    temp.addr <- which(dis==min(dis))[1]
    q.addr <- which(res==res.temp[temp.addr])[1]
  }
  return(q.addr)
}


# Generating P matrix
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





# plot
SIM.plot <- function(data, y.name, subtitle){
  
  sub <- paste0('expression(bold(x)[',subtitle,'])')
  
  if(y.name=='err.95'){
  dat.simu %>% filter(n<=75 & n!=60 ) %>% filter(b==10|b==1e+05|b==1e+07) %>% ggplot() + 
    geom_path(aes(x=n,y=eval(parse(text=y.name)),colour=b,group=b,linetype=b), show.legend = T) + 
    labs(color="Number of Repeats", linetype='Number of Repeats') + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 2*10^x)) + 
    labs(subtitle = eval(parse(text=sub))) + 
    ylab("AE") + theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(face="bold", 
                                     size=8),
          axis.text.y = element_text(face="bold", 
                                     size=8),
          axis.title=element_text(size=10,face="bold"))
  } else {
    dat.simu %>% filter(n<=75) %>% filter(b==10|b==1e+05|b==1e+07) %>% ggplot() + 
      geom_path(aes(x=n,y=eval(parse(text=y.name)),colour=b,group=b,linetype=b), show.legend = T) + 
      labs(color="Number of Repeats", linetype='Number of Repeats') + 
      scale_y_log10(breaks = trans_breaks("log10", function(x) 2*10^x)) + 
      labs(subtitle = eval(parse(text=sub))) + 
      ylab("AE") + theme(plot.subtitle = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(face="bold", 
                                       size=8),
            axis.text.y = element_text(face="bold", 
                                       size=8),
            axis.title=element_text(size=10,face="bold"))
  } 
}


 
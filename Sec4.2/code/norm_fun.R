# generate P matrix
p.matrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r = runif(m)
    r = r/sum(r) #generate row
    r[1:(m-1)] = round(r[1:(m-1)],3)
    while(sum(r[1:(m-1)])>1){
        r = runif(m)
        r = r/sum(r)
        r[1:(m-1)] = round(r[1:(m-1)],3)
    }
    r[m] = 1-sum(r[1:(m-1)])
    p[i,] = r
  }
  return(p)
}

# compute l vector
l.vec.compute=function(k, cn.vec, m)
{
  k=k-1
  l.vec=rep(0, m-1)
  for(i in 1:(m-1))
  {
    aa=k%%cn.vec[i]
    bb=(k-aa)/cn.vec[i]
    l.vec[i]=bb
    k=aa
  }
  l.vec=l.vec+1
  return(l.vec)
}


# compute mae for given n, m, using normal method
mae.norm = function(n,m){
  pp = p.matrix(n,m)
  res0 = dpmd(pp)
  mm = m
  nn = n
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)
  res_normal=double(nnt)
  for(ii in 1:nnt)
  {
    idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
    if(nn-sum(idx-1)<0)
      res_normal[ii] = 0
    else
    {
      vec = c(idx-1,nn-sum(idx-1))
      res_normal[ii] = dpmd(pp, x = vec, method = 'NA')
    }
  }
  mae <- max(abs(res0-res_normal))
  base <- max(res0) 
  return(c(mae, base))
}



# compute averaged mae given n, m, K using normal method (averaged on K). 
norm.accuracy <- function(n, m, K){

  results <- matrix(0, nrow = 1, ncol = 4)
  results <- as.data.frame(results)
  colnames(results) <- c("n", "m", "MAE", "baseline")
  results$n <- n
  results$m <- m
  mae <- 0
  base <- 0
  for(i in 1:K){
    t <- mae.norm(n, m)
    mae <- mae + t[1]
    base <- base + t[2]
  }
  mae <- mae/K
  base <- base/K
  results$MAE <- mae
  results$baseline <- base

  return(results)
}


# breaks functions
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

# scale function
scaleFUN <- function(x) sprintf("%.4f", x)

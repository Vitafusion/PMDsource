
compute_l.vec = function(result){
  m = length(result)
  res = result[1:(m-1)]
  l.vec = rep(0,m-1)
  for (i in 1:(m-1)) {
    l.vec[i] = as.numeric(res[i]) + 1
  }
  return(l.vec)
}


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



make.data.groups = function(dat, groups_number,category_name=c("setosa","versicolor","virginica"),category_column = 'Species'){
  n = nrow(dat)
  group = list()
  k=1
  if(n %% groups_number != 0)
    stop("invalid number for grouping.")
  out = matrix(NA,nrow = groups_number,ncol = length(category_name))
  ss <- sample(1:groups_number,size=n,replace=TRUE,prob=rep(1/groups_number,groups_number))
  for (i in 1:groups_number) {
    if(nrow(dat[ss==i,])!=0){
      group[[k]] = dat[ss==i,]
      expr = paste(category_column)
      for(j in 1:length(category_name)){
        out[k,j] = nrow(group[[k]][which(group[[k]][,expr]==category_name[j]),])
      }
      k = k + 1
    }
  }  
  out = na.omit(out)
  return(list(group,out))
}



cal_pmatrix = function(parm, x_mat, cat_number){
  x_mat = as.matrix(x_mat)
  
  n = ncol(x_mat) - 1
  #x_mat = cbind(matrix(1, nrow = nrow(x_mat),ncol = 1), x_mat)
  
  m = cat_number
  parm = matrix(parm,n+1,m-1, byrow=T)
  
  #P = matrix(0, nrow = nrow(x_mat), ncol = category_number)
  
  P = exp(x_mat%*%parm)/(rowSums(exp(x_mat%*%parm))+1)
  P = cbind(P,1/(rowSums(exp(x_mat%*%parm))+1))
  for(i in 1:nrow(P)){
    P[i,1:(m-1)] = round(P[i,1:(m-1)],5)
    s = sum(P[i,1:(m-1)])
    if(s>=1){
      P[i,1:(m-1)] = P[i,1:(m-1)]/s
    }
    P[i,m] = 1 - sum(P[i,1:(m-1)])
  }
  return(P)
}


point.loglik.calcu = function(pp,count_result){
  #browser()
  res = dpmd(pp)
  l.vec = compute_l.vec(count_result)
  l.vec = paste(l.vec,collapse = ",")
  expr0 = "res["
  expr = paste0(expr0,l.vec,"]")
  likel = eval(parse(text=expr))
  log_lik = log(likel)
  #browser()
  return(log_lik)
}



toltal.loglik.calcu.ai4i = function(parm,
                                    result_mat = count_result,
                                    group = groups,
                                    cat_number = category_number,
                                    covariate_name = c("Air temperature [K]","Process temperature [K]","Rotational speed [rpm]")) {
  minus_log_lik = 0
  for (i in 1:length(result_mat)) {
    x = group[[i]]
    if(is.na(covariate_name)) {
      x_mat = matrix(1, nrow = nrow(x), ncol = 1)
    } else { 
      x_mat = x[,covariate_name] 
    } 
    
    x_mat = as.matrix(x_mat)
    x_mat = cbind(matrix(1, nrow = nrow(x_mat),ncol = 1), x_mat)
    
    
    result = result_mat[[i]]
    P = cal_pmatrix(parm, x_mat, cat_number)
    prob = point.loglik.calcu(P,result)
    if(prob == -Inf){
      prob = -10000
    }
    minus_log_lik = minus_log_lik - prob
    if(minus_log_lik==Inf){
      stop("!!")
    }
    #browser()
  }
  #browser()
  return(minus_log_lik)
}

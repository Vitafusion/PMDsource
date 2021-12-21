

################################################
#   Statistical Inference for Aggregated Data  #
#                   Ai4i Data                  #
################################################



##############################################################################
# data prepare

raw.ai4i = read.table(file = 'Sec5.2/data/ai4i2020.csv',sep = ',')
colname.ai4i <- raw.ai4i[1,]
colnames(raw.ai4i) <- c('UDI', 'Product ID', 'Type', 
                        'Air temperature [K]', 'Process temperature [K]', 
                        'Rotational speed [rpm]', 
                        'Torque [Nm]', 'Tool wear [min]', 
                        'Machine failure', 
                        'TWF', 'HDF', 'PWF', 'OSF', 'RNF')
raw.ai4i <- raw.ai4i[-1,]

raw.ai4i[,4:14] <- apply(raw.ai4i[,4:14], 2, as.numeric)
raw.ai4i[,9:14] <- apply(raw.ai4i[,9:14], 2, as.numeric)
ai4i <- raw.ai4i

ai4i$`Air temperature [K]` <- scale(ai4i$`Air temperature [K]`)
ai4i$`Process temperature [K]` <- scale(ai4i$`Process temperature [K]`)
ai4i$`Rotational speed [rpm]` <- scale(ai4i$`Rotational speed [rpm]`)
ai4i$`Torque [Nm]` <- scale(ai4i$`Torque [Nm]`)



# grouping, based on unique combination of `Type` and `Tool wear [min]`

rule.group <- unique(ai4i[,c('Type','Tool wear [min]')])
groups <- list()
k <- 1
for(i in 1:nrow(rule.group)){
  type <- rule.group[i,1]
  toolwear <- rule.group[i,2]
  idx <- which(ai4i$Type==type & ai4i$`Tool wear [min]`==toolwear)
  groups[[k]] <- ai4i[idx,]
  k <- k+1
}


# compute result counts for each group
# category 1: (TWF=1 or OSF=1) no others
# category 2: (HDF=1 or PWF=1) no others
# category 3: censored (non failure and others)

library(dplyr)
#plot of distribution for each category

h1 <- ai4i[which((ai4i$TWF==1 | ai4i$OSF==1)&(ai4i$HDF==0 & ai4i$PWF==0)),]
h2 <- ai4i[which((ai4i$HDF==1 | ai4i$PWF==1)&(ai4i$TWF==0 & ai4i$OSF==0)),]
h3 <- setdiff(ai4i,rbind(h1,h2))
mat.hist <- matrix(c(nrow(h1), nrow(h2), nrow(h3)), nrow=1, byrow = T)
colnames(mat.hist) <- c('TWF_OSF','HDF_PWF','other')


t1 <- mat.hist[1,]
t2 <- c('TWF_OSF','HDF_PWF','other')
mat <- t(rbind(t1,t2))
mat <- as.data.frame(mat)
mat$t1 <- as.numeric(mat$t1)
colnames(mat) <- c('counts', 'category')

library(ggplot2)
ggplot(mat, aes(category, counts)) +
  geom_bar(stat = 'identity')   

# generateing result counts
res.count <- list()
for (i in 1:nrow(rule.group)) {
  #if(nrow(groups[[i]]) != 1)
    for(j in nrow(groups[[i]])){
      temp.dat <- groups[[i]]
      c1 <- c2 <- c3 <- 0
      for(k in 1:nrow(temp.dat)){
        temp <- temp.dat[k,]
        if((temp$TWF==1 | temp$OSF==1)&(temp$HDF==0 & temp$PWF==0))
          c1 <- c1+1
        else if((temp$TWF==0 & temp$OSF==0)&(temp$HDF==1 | temp$PWF==1))
          c2 <- c2+1
        else
          c3 <- c3+1
      }
    }
  res.count[[i]] <- c(c1,c2,c3)
}



# Use "Air temperature [K]", "Rotational speed [rpm]" as covariates
covariates <- c("Air temperature [K]", "Rotational speed [rpm]")
# category number 3
m <- 3



covname <- covariates

library(PoissonMultinomial)
f = function(parm){
  toltal.loglik.calcu.ai4i(parm,
                           result_mat = res.count,
                           group = groups,
                           cat_number = 3,
                           covariate_name = covname)
}



# optim to find estimates for betas, m categories and k covariates
# so beta(including intercep) will be a (m-1)*(k+1) matrix
if(is.na(covname[1])){ 
  parm <- rep(1,2)
} else { 
  parm <- c(1, 1, rep(0, (m-1)*(length(covname))))
}

op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
op 



h <- op$hessian
hinv <- solve(h)

if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_7.RData')
}


h <- op$hessian
hinv <- solve(h)
if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_6.RData')
}



h <- op$hessian
hinv <- solve(h)
if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_5.RData')
}


h <- op$hessian
hinv <- solve(h)
if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_4.RData')
}




# #parm <- c(7.142795, -3.243091,  1.927554,  3.688145, -2.688194,  1.608610)

# # estimated beta
# beta.hat <- op$par

# # the hessian and inverse of hessian
# H <- op$hessian
# H.inv <- solve(H)


# # se of beta

# se <- sqrt(diag(H.inv))

# # 0.95 CI
# left.CI <- beta.hat - 1.96*se
# right.CI <- beta.hat + 1.96*se


# # calculate p matrix for all groups
# pp = list()
# covariate_name = c("Air temperature [K]",
#                    "Process temperature [K]",
#                    "Rotational speed [rpm]",
#                    "Torque [Nm]")
category_number = 3
for(i in 1:length(groups)) {
  x = groups[[i]]
  x = x[,covariate_name]
  x = as.matrix(x)
  x = cbind(matrix(1, nrow = nrow(x),ncol = 1), x)
  pp[[i]]= cal_pmatrix(op$par, x, category_number)
  
}

# # P matrix for group 1, 5 and 8
# pp[[1]]
# pp[[5]]
# pp[[8]]


# save results

# expr <- paste0('ai4i_', seed, '.RData')
# save(op, beta.hat, H, file=expr)





############################################ 
# Collecting Test Data For The SIM Method  #
############################################



# find the saved .RData files
files <- list.files("./", pattern="simulation_[1-30]*.RData")



# initialization

  res <- matrix(NA,nrow = 1 ,ncol = 9)
  res <- as.data.frame(res)
  colnames(res) <- c("n","m","B","max","err.max","per.95","err.95","per.90","err.90")



for(i in 1:length(files)){
	f <- files[i]
	load(f)
	cat(" ", f, sep="")
	res <- rbind(res, result)
}

cat("\n")

write.table(res, file = "simu.txt", sep = "\t")

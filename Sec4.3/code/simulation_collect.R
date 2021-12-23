

############################################ 
# Collecting Test Data For The SIM Method  #
############################################



# find the saved .RData files
files <- list.files("./", pattern="RData")



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

# remove the first row
res <- res[-1,]

write.table(res, file = "simu.txt", sep = "\t")

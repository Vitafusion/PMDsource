

########################################## 
# Collecting Test Data For The NA Method #
##########################################



# find the saved .RData files
files <- list.files("./", pattern="RData")


for(i in 1:length(files)){
	f <- files[i]
	load(f)
	cat(" ", f, sep="")
	if(i==1){
		res <- results
	} else {
		res <- res + results
	}
}

res <- res/length(files)
cat("\n")

write.table(res, file = "norm_m3.txt", sep = "\t")

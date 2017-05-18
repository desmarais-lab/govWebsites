library("tools") #for file_ext()
require("xtable")
#require(stringr)

#This script extracts the filetypes, folder sizes, etc.
#from the 10 test cases scraped from the Wayback Machine

setwd("./websites/current/")

a <- list.files(recursive = T) #create a list of all files in all subdirectories

#file types
b <- file_ext(a) #get file extension
b <- data.frame(sort(table(b), decreasing = T)) #turn into table/data frame
names(b) <- c("Filetype", "Frequency") #rename columns
b <- b[1:25,] #only get top 10

#turn table into latex object
print(xtable(b, caption="The 25 most common file types in scraped websites"), include.rownames = F)


#folder size
#get folder names
d <- list.files()
e <- vector()
#print directory size with Linux command 'du -s'
for(i in 1:length(d)){
  e[i] <- system(paste("du",d[i],"-s"), intern=T)
}
#split character vector into dataframe
require(reshape2)
f <- colsplit(e,"\\t",c("Size (MB)","Website"))


##number of files
g <- vector()
#number of files per website/folder
for(i in 1:length(d)){
g[i] <- length(list.files(list.files()[i],recursive = T, all.files=T))
}
#add to data frame created above
f$Files <- g
#re-order and turn table into latex object
print(xtable(f[,c(2,3,1)], caption="Number of files and size of websites"), include.rownames = F)


rm(a,b,c,d,e,f,g,i)

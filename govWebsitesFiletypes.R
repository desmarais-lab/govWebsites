#This script extracts the filetypes, folder sizes, etc.
#from the 10 test cases scraped from the Wayback Machine

setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/")

a <- list.files(recursive = T) #create a list of all files in all subdirectories

#file types
require(stringr)
#extract file type
b <- str_extract(a,"(\\.[^.]+)$")
#create table for file types with at most 5 characters
c <- data.frame(table(b[which(str_length(b)<6)]))
names(c) <- c("File type","Occurrences")
c <- c[-c(1:8),]
c <- c[order(c$Occurrences, decreasing = T),]
require(xtable)
#omit some of the less useful types and turn table into latex object
print(xtable(c, caption="File types in scraped websites"), include.rownames = F)


#folder size
#get folder names
d <- list.files()
e <- vector()
#print directory size with Linux command 'du -s'
for(i in 1:10){
  e[i] <- system(paste("du",d[i],"-s"), intern=T)
}
#split character vector into dataframe
require(reshape2)
f <- colsplit(e,"\\t",c("Size (MB)","Website"))


##number of files
g <- vector()
#number of files per website/folder
for(i in 1:10){
g[i] <- length(list.files(list.files()[i],recursive = T, all.files=T))
}
#add to data frame created above
f$Files <- g
#re-order and turn table into latex object
print(xtable(f[,c(2,3,1)], caption="Test websites"), include.rownames = F)


rm(a,b,c,d,e,f,g,i)

setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/")

a <- list.files(recursive = T, all.files=T)
list.dirs(recursive = T)
#aa <- system(sprintf('ls %s', getwd()), intern=TRUE)
dir(pattern = ".html$", recursive = T)

library(stringr)
b <- str_extract(a,"(\\.[^.]+)$")
c <- table(b[which(str_length(b)<6)])

library(xtable)
xtable(c[10:36])

##folder size
d <- list.files() #list.dirs(recursive = F)
for(i in 1:10){
  system(paste("du",d[i],"-s"))
}

system("du -s")

##number of files
abc <- vector()
for(i in 1:10){
abc[i] <- length(list.files(list.files()[i],recursive = T, all.files=T))
}
abc
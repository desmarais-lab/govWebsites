require(jsonlite)
require(xtable)

setwd("~/Dropbox/4_RA/govWebsites") #Linux
load(file="data/indiana2015.rdata")

date <- c(20150701,20150801,20150901,20151001,20151101,20151201,20160101,20160201,20160301,20160401,20160501,20160601)
snapshots <- data.frame(matrix(NA,nrow=12,17))
snapshots$X1 <- date
names(snapshots) <- c("Date", as.character(indiana$District))


#############################################################################

for(j in 1:(ncol(snapshots)-1)){
  for(i in 1:nrow(snapshots)){
    API_base <- 'http://archive.org/wayback/available?url='
    timestamp <- paste('&timestamp=', snapshots$Date[i], sep="")
    
    website <- indiana$Domain.Name[j] #loop through websites
    
    #get nearest date from WaybackMachine API
    API_URL <- paste(API_base,website, timestamp, sep = "")
    wayback <- fromJSON(API_URL)
    snapshots[i,j+1] <- wayback$archived_snapshots$closest$timestamp
  }
}

print(xtable(snapshots, caption="Dates of WaybackMachine snapshots 2015-16"), include.rownames = F)

save(snapshots, file="data/indianaSnapshots.rdata")

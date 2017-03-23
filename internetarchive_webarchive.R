setwd("/home/markus/Dropbox/4_RA/govWebsites")
load("data/govWebsitesVerified.Rdata")


library(jsonlite)

API_base <- 'http://archive.org/wayback/available?url='


#randomly select N websites to test
test <- sample(data$redirect, 10)

#loop through websites, results automatically get saved into 'websites' folder inside wd
for (i in 1:length(test)){
  
  website <- test[i] #loop through websites
  
  #the following three lines aren't actually needed when using the Ruby package
  API_URL <- paste(API_base,website,sep = "")
  wayback <- fromJSON(API_URL)
  waybackURL <- wayback$archived_snapshots$closest$url

  #pasting input for Ruby package, then executing it
  #--concurrency 20 causes 20 items to be downloaded at the same time
  #the default is 1, this takes WAY too long (i.e. one hour for a website...)
  #--from 201610 downloads a snapshot from October 2016, or, if not available, later
  WBMD_base <- "wayback_machine_downloader --concurrency 20 --from 201610"
  WBMD_site <- paste(WBMD_base,website)
  system(WBMD_site, intern = T) #just ignore the printout if running outside of loop
}

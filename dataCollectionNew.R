# Pipeline file to do the data collection part of the pipeline

library(stringr)
source("wget.R")
source("fixExtensions.R")
source("convertToText.R")

# Example: mayors

#load URLs
load("rfiles/allURLs.rdata")

#reduce the city website url to its base url
websiteUrls$cityWebsite <- str_extract(websiteUrls$cityWebsite, "^.+?[^\\/:](?=[?\\/]|$)")
#get the version of the urls that were checked with Selenium in python
urlsVerified <- read.table("websites/urls_verified.txt")
websiteUrls$cityWebsiteVerified <- as.character(urlsVerified$V1)
#fix the websites that just didn't work at all
websiteUrls$cityWebsiteVerified[websiteUrls$State_City=="Indiana_Linton"] <- "http://www.cityoflinton.com/"
websiteUrls$cityWebsiteVerified[websiteUrls$State_City=="Louisiana_Marion"] <- "http://www.townofmarionla.com/"

#use wget function to download everything
wget(websiteUrls$cityWebsite, "~/hd2/cityWebsites")

#check which sites didn't get downloaded

#-----------------------------------------------------------------------------#
library(tools)

filepath <- "/media/mneumann/hd2/cityWebsites2"
## Reading in the data
#create a list of all files in all subdirectories
f <- list.files(path = filepath, recursive = T)

#file types
ext <- file_ext(f) #get file extension
folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]

#store objects in a data frame
d <- data.frame(path = str_c(filepath, f, sep = "/"), 
                folder = str_c(filepath, folder, sep = "/"),
                filename,
                ext)

#get the city
d$city <- str_split_fixed(d$path, str_c(filepath, "/"), 2)[,2]
d$city <- sapply(d$city, str_extract, "^([^/]*)")

d2 <- data.frame(table(d$city))
d2 <- d2[d2$Freq<5,]

websiteUrls$redo <- F
for(i in 1:length(d2$Var1)){
  websiteUrls$redo[str_detect(websiteUrls$cityWebsite, as.character(d2$Var1)[i])] <- T
}
  
redo <- websiteUrls$cityWebsiteVerified[websiteUrls$redo==T]

#-----------------------------------------------------------------------------#

#use wget function to download the stragglers
wget(redo, "~/hd2/cityWebsites3")


#use the fixExtensions function to make sure all the file types (i.e. endings) are correct
#this is necessary because readtext determines file type by extension
fixExtensions("./websites/bigcitymayors2")

#some websites have linux invalid filenames, fix them
#fixFileNames("./websites/bigcitymayors2")

#use the readtext package to convert everything to text (in a new folder)
convertToText("./websites/bigcitymayors2", "./websites/bigcitymayors2TXT")

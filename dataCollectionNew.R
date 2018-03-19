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

#use wget function to download everything
wget(websiteUrls$cityWebsite, "~/hd2/cityWebsites")

#use the fixExtensions function to make sure all the file types (i.e. endings) are correct
#this is necessary because readtext determines file type by extension
fixExtensions("./websites/bigcitymayors2")

#some websites have linux invalid filenames, fix them
#fixFileNames("./websites/bigcitymayors2")

#use the readtext package to convert everything to text (in a new folder)
convertToText("./websites/bigcitymayors2", "./websites/bigcitymayors2TXT")

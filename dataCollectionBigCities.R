# Pipeline file to do the data collection part of the pipeline

source("wget.R")
source("fixExtensions.R")
source("convertToText.R")

# Example: mayors

#load URLs
load("rfiles/campaign_websites.rdata")

#use wget function to download everything
wget(df$city_website, "~/hd1/govWebsites/websites/bigcities")

#use the fixExtensions function to make sure all the file types (i.e. endings) are correct
#this is necessary because readtext determines file type by extension
fixExtensions("./websites/bigcitymayors")

#some websites have linux invalid filenames, fix them
fixFileNames("./websites/bigcitymayors")

#use the readtext package to convert everything to text (in a new folder)
convertToText("./websites/bigcitymayors", "./websites/bigcitymayorsTXT")

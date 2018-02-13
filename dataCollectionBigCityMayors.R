# Pipeline file to do the data collection part of the pipeline

source("wget.R")
source("fixExtensions.R")
source("convertToText.R")

# Example: mayors

#load URLs
load("rfiles/campaign_websites.rdata")
#remove mayors without a campaign website
df <- df[is.na(df$campaign_website)==F,]
#remove independent mayors
df <- df[is.na(df$party)==F,]
df <- df[df$party!="(I)",]

#use wget function to download everything
wget(df$campaign_website, "./websites/bigcitymayors2")

#use the fixExtensions function to make sure all the file types (i.e. endings) are correct
#this is necessary because readtext determines file type by extension
fixExtensions("./websites/bigcitymayors2")

#some websites have linux invalid filenames, fix them
#fixFileNames("./websites/bigcitymayors2")

#use the readtext package to convert everything to text (in a new folder)
convertToText("./websites/bigcitymayors2", "./websites/bigcitymayors2TXT")

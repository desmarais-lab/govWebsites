# Pipeline file to do the data collection part of the pipeline

source("wget.R")
source("fixExtensions.R")
source("convertToText.R")

# Example: mayors

#load URLs
load("rfiles/mayors.rdata")

#use wget function to download everything
wget(mayors$website, "./websites/mayors")

#use the fixExtensions function to make sure all the file types (i.e. endings) are correct
#this is necessary because readtext determines file type by extension
fixExtensions("./websites/mayors")

#use the readtext package to convert everything to text (in a new folder)
convertToText("./websites/mayors", "./websites/mayorsTXT")
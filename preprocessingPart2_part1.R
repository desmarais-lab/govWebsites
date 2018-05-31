# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')
library('caret')
library('ranger')
library('quanteda')

# ----

f <- list.files("rfiles/city_chunks_unprocessed", full.names = T)
f_file <- list.files("rfiles/city_chunks_unprocessed")

#loop over cities
for(city in 40:length(f)){
  
  load(f[city])
  a <- a[!a$text=="",]
  a <- a[!a$text==" ",]
  
  # #Seattle has some very large GIS maps which quanteda doesn't like at all
  # if(city==281){
  #   gisWebplots <- which(str_detect(a$path, "\\/home\\/mneumann\\/hd2\\/govWebsites\\/www.seattle.gov\\/dpd\\/Research\\/gis\\/webplots\\/*"))
  #   a <- a[-gisWebplots,]
  # }
  
  #source("preprocessingQuanteda_part1.R")
  #save(tt2, file = paste("rfiles/city_chunks_part1_processed/", f_file[city], sep = ""))
  #rm(list = ls()[!ls()%in%c("city", "f", "f_file")])
  load(file = paste("rfiles/city_chunks_part1_processed/", f_file[city], sep = ""))
  source("preprocessingQuanteda_part2.R")
  save(a, file = paste("rfiles/city_chunks_part2_classifier_processed/", f_file[city], sep = ""))
}
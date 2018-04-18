# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')

# ----

f <- list.files("rfiles/city_chunks_unprocessed", full.names = T)
f_file <- list.files("rfiles/city_chunks_unprocessed")

#loop over cities
for(city in 282:length(f)){
  
  load(f[city])
  a <- a[!a$text=="",]
  a <- a[!a$text==" ",]
  
  #Seattle has some very large GIS maps which quanteda doesn't like at all
  if(city==281){
    gisWebplots <- which(str_detect(a$path, "\\/home\\/mneumann\\/hd2\\/govWebsites\\/www.seattle.gov\\/dpd\\/Research\\/gis\\/webplots\\/*"))
    a <- a[-gisWebplots,]
  }
  
  source("preprocessingQuanteda.R")
  a <- a[!a$text=="",]
  
  save(a, file = paste("rfiles/city_chunks_processed/", f_file[city], sep = ""))
  
  rm(list = ls()[!ls()%in%c("city", "f", "f_file")])
  
}

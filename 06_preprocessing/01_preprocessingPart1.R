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
  
  source("preprocessCityPart1.R")
  a <- a[!a$text=="",]
  
  save(a, file = paste("rfiles/city_chunks_processed/", f_file[city], sep = ""))
  
  rm(list = ls()[!ls()%in%c("city", "f", "f_file")])
  
}

f <- list.files("rfiles/city_chunks_processed", full.names = T)
f_file <- list.files("rfiles/city_chunks_processed")

d <- tibble::tibble()

for(i in 1:length(f)){
  
  load(f[i])
  a$city <- str_split(f_file, ".")[[1]]
  d <- rbind(d, a)
  
}

save(a, file = "rfiles/city_chunks_processed/all.rdata")
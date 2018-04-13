# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')

# ----

f <- list.files("rfiles/city_chunks_unprocessed", full.names = T)
f_file <- list.files("rfiles/city_chunks_unprocessed")

#loop over cities
for(city in 1:length(f)){
  
  load(f[city])
  a <- a[!a$text=="",]
  a <- a[!a$text==" ",]
  
  source("preprocessingQuanteda.R")
  a <- a[!a$text=="",]
  
  save(a, file = paste("rfiles/city_chunks_processed/", f_file[city], sep = ""))
  
  rm(list = ls()[!ls()%in%c("city", "f", "f_file")])
  
}

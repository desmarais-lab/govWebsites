# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')
library('caret')
library('ranger')
library('quanteda')

set.seed(1)

# ----

f <- list.files("rfiles/city_chunks_unprocessed", full.names = T)
f_file <- list.files("rfiles/city_chunks_unprocessed")

#loop over cities
for(city in 1:length(f)){
  
  load(f[city])
  a <- a[!a$text=="",]
  a <- a[!a$text==" ",]
  
  load(file = paste("rfiles/city_chunks_part1_processed/", f_file[city], sep = ""))
  source("preprocessingQuanteda_part2.R")
  save(a, file = paste("rfiles/city_chunks_part2_classifier_processed/", f_file[city], sep = ""))
}
# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')
source("./functions/preprocessingChunked.R")

# ----

f <- list.files("rfiles/city_chunks_unprocessed" ,full.names = T)

#loop over cities
for(city in 1:length(f)){
  
  load(f[i])
  
  # Do all the preprocessing that needs to be done before removing duplicate lines
  names(a)[names(a)=="text"] <- "doc"
  d <- preprocessing_1(a)
  rm(a)
  
  #d$doc2 <- d$doc
  #d$doc <- d$doc2
  
  # Find duplicate lines
  docDuplicates <- proc_city(d)
  # Use the counts of duplicate lines found above to remove them over a certain threshold
  d$doc <- as.character(pbsapply(1:nrow(d), removeDuplicates, 5))
  # Do the rest of the preprocessing
  d <- preprocessing_2(d)
  
  #once the city's documents are fully preprocessed
  #merge them with the metadata
  #and save to a file
  d <- subset(d, select = c(iter, doc))
  d <- merge(d, d_meta, by = "iter")
  save(d, file = paste("rfiles/city_chunks_processed/", d$State_City[1], ".rdata", sep = ""))
  
}
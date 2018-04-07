# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')

# Functions

#extract city from directory
extractCity <- function(path){
  
  city <- str_replace(path, "/home/mneumann/hd2/govWebsites/", "")
  city <- str_extract(city, "^(.*?)\\/")
  city <- str_replace(city, "\\/", "")
  return(city)
  
}

source("./functions/preprocessingChunked.R")

# ----

load("rfiles/citydocs.rdata")
d_meta <- d
cities <- unique(d_meta$city)

#loop over cities
for(ncities in 1:length(cities)){
  
  city <- cities[ncities]
  
  #which chunk files contain the city's documents?
  city_chunkfiles <- unique(d_meta$parsedtextfile[d_meta$city==city])
  
  #combine all the chunks of which a city consists
  combined_chunks <- data.frame(doc_id = character(length = 0),
                                text = character(length = 0),
                                iter = numeric(length = 0),
                                path = character(length = 0),
                                city = character(length = 0))
  
  for(i in 1:length(city_chunkfiles)){
    
    load(city_chunkfiles[i])
    a$city <- extractCity(a$path)
    a <- a[a$city==city,]
    
    combined_chunks <- rbind(combined_chunks, a)
    
  }
  
  rm(a, city_chunkfiles)
  
  # Do all the preprocessing that needs to be done before removing duplicate lines
  names(combined_chunks)[names(combined_chunks)=="text"] <- "doc"
  d <- preprocessing_1(combined_chunks)
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
  save(d, file = paste("rfiles/city_chunks/", d$State_City[1], ".rdata", sep = ""))
  
}
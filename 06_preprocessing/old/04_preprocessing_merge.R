# -------------------
# PREPROCESSING
# iterate over cities
# merge the text with the covariates
# -------------------

library('stringr')

# ----

f <- list.files("rfiles/city_chunks_part3_lemmatized", full.names = T)
f_file <- list.files("rfiles/city_chunks_part3_lemmatized")

d <- list()
for(city in 1:length(f)){
  
  load(f[city])
  a$city <- str_replace(f_file[city], ".rdata", "") 
  d[[city]] <- a
  
}

d <- do.call(rbind, d)


save(d, file = "rfiles/allDocuments.rdata")

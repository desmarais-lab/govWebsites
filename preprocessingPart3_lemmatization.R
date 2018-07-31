# -------------------
# PREPROCESSING
# iterate over cities
# -------------------

library('stringr')
library('quanteda')
library('spacyr')
spacy_initialize()

# ----

f <- list.files("rfiles/city_chunks_part2_classifier_processed", full.names = T)
f_file <- list.files("rfiles/city_chunks_part2_classifier_processed")

for(city in 1:length(f)){
  
  set.seed(1)
  
  load(f[city])
  
  parsedtxt <- spacy_parse(a$text, pos = F, tag = F, lemma = T, entity = F, dependency = F)
  parsedtxt$doc_id <- as.numeric(str_replace(parsedtxt$doc_id, "text", ""))
  parsedtxt <- aggregate(parsedtxt$lemma, by = list(parsedtxt$doc_id), paste, collapse = " ")
  a$text <- parsedtxt$x
  
  save(a, file = paste("rfiles/city_chunks_part3_lemmatized/", f_file[city], sep = ""))
  
}

library('quanteda')
library('spacyr')
spacy_initialize()
library('hunspell')
library('stringr')
library('ff')
library('data.table')
source('PKG_preprocessing.R')
set.seed(1)

#load the extracted texts
load("../04_extractText/out/results_parsed_all.rdata")
#rename because "parsed" is a bit misleading in the context of this script
extracted <- results_parsed
rm(results_parsed)
#load the metadata, find out which id belongs to which city
load("../04_extractText/out/citydocs.rdata")
d <- subset(d, select = c(id, State_City))
extracted$city <- d$State_City[match(extracted$id, d$id)]
all_cities <- unique(extracted$city)

#----
# Parse each city

for(i in 1:length(all_cities)){
  #subset the data table to this city only
  results_extracted_city <- extracted[extracted$city == all_cities[i],]
  #remove duplicates
  results_extracted_city <- results_extracted_city[!duplicated(results_extracted_city$text),]
  #parse
  parsing_result <- spacy_parse_chunk(results_extracted_city$text, results_extracted_city$id)
  #create the directory to save the city's results in
  citydir <- paste0("out/preprocessing_parsed/", all_cities[i])
  if(dir.exists(citydir)==F){dir.create(citydir, recursive = T)}
  #save both the results, and the nouns specifically (for easier loading)
  save(parsing_result, file = paste0(citydir, "/parsed.rdata"))
  nouns <- unique(parsing_result$lemma[parsing_result$pos=="NOUN"])
  save(nouns, file = paste0(citydir, "/parsed_nouns.rdata"))
}

#----
# Combine nouns

f <- list.files(path = "out/preprocessing_parsed", pattern = "parsed_nouns.rdata", full.names = T, recursive = T)
all_nouns <- list()
for(i in 1:length(f)){
  load(f[i])
  all_nouns[[i]] <- nouns
}
all_nouns <- unique(unlist(all_nouns))
save(all_nouns, file = "out/all_nouns.rdata")

#remove unneeded objects
rm(d, extracted, parsing_result, results_extracted_city, nouns)

#----
# Do the rest of the preprocessing

#setting seed again to allow the script to be restarted here
set.seed(1)

f <- list.files(path = "out/preprocessing_parsed", pattern = "parsed.rdata", full.names = T, recursive = T)
for(i in 1:length(f)){
  load(f[i])
  #little hack to allow quanteda to recognize spacyr objects that were pasted together
  if(any(class(parsing_result)=="spacyr_parsed")==F){
    class(parsing_result) <- c("spacyr_parsed", "data.frame")
  }
  tks_city <- preprocessWebsite(parsing_result, custom_nouns = all_nouns)
  save(tks_city, file = str_replace(f[i], "parsed.rdata", "preprocessed.rdata"))
}

#----
# Combine

f <- list.files(path = "out/preprocessing_parsed", pattern = "preprocessed.rdata", full.names = T, recursive = T)
tks_city_l <- list()
for(i in 1:length(f)){
  load(f[i])
  tks_city_l[[i]] <- tks_city
}
tks <- do.call(c, tks_city_l)

#----
# Add metadata

load("../04_extractText/out/citydocs.rdata")
d_reordered <- d[match(names(tks), d$id),]
docvars(tks) <- d_reordered
rm(d, d_reordered)
save(tks, file = "out/preprocessed.rdata")

#----
# Convert to stm object

#use only the states for which we have data on multiple cities
tks <- tokens_subset(tks, State %in% c("Indiana", "Louisiana", "New York", "California", "Washington", "Texas"))
#convert to dfm and then stm
d_dfm <- dfm(tks)
d_stm <- convert(d_dfm, to = "stm")
save(d_stm, file = "out/stm_corpus.rdata")

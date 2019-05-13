library('quanteda')
library('spacyr')
spacy_initialize()
library('hunspell')
library('stringr')
library('ff')
set.seed(1)

#we are very RAM limited here, so only do whats necessary

load("../04_parseText/out/results_parsed_all.rdata")
load("../04_parseText/out/citydocs.rdata")
d <- subset(d, select = c(id, State_City))
results_parsed2 <- results_parsed
results_parsed2$city <- d$State_City[match(results_parsed2$id, d$id)]
results_parsed2$duplicate <- F
all_cities <- unique(results_parsed2$city)
for(i in 1:length(all_cities)){
  results_parsed2$duplicate[results_parsed2$city==all_cities[i]] <- duplicated(results_parsed2$text[results_parsed2$city==all_cities[i]])
}
duplicate_ids <- results_parsed2$id[results_parsed2$duplicate]
save(duplicate_ids, file = "out/duplicate_ids.rdata")

#----
# Parsing

#parse all html documents
# crps <- corpus(results_parsed$text, docnames = results_parsed$id)
# start_time <- Sys.time()
# parsedtxt <- spacy_parse(crps, tag = T, dependency = T)
# end_time <- Sys.time()
# end_time - start_time

# Chunked version
for(i in chunk(from = 1, to = nrow(results_parsed), by = 5000)){
  #create chunked data frame
  results_parsed_chunk <- results_parsed[c(min(i):max(i)),]
  #create chunked quanteda corpus
  crps_chunk <- corpus(results_parsed_chunk$text, docnames = results_parsed_chunk$id)
  #parse with spacy
  parsedtxt_chunk <- spacy_parse(crps_chunk, tag = T, dependency = T)
  #save
  save(parsedtxt_chunk, file = paste0("out/parsing_chunks/chunk_", min(i), "_", max(i), ".rdata"))
}
rm(parsedtxt_chunk, results_parsed, results_parsed_chunk, crps_chunk, i)
gc()

set.seed(1) #setting seed again so session can be restarted here

#save the unique lemmas that are nouns
f <- list.files("out/parsing_chunks", full.names = T)
parsedtxt_nouns <- list()
for(i in 1:length(f)){
  load(f[i])
  current_nouns <- unique(parsedtxt_chunk$lemma[parsedtxt_chunk$pos=="NOUN"])
  parsedtxt_nouns[[i]] <- current_nouns
  #backup
  save(current_nouns, file = str_replace_all(f[i], "chunk", "noun"))
}
parsedtxt_nouns <- unique(unlist(parsedtxt_nouns))
save(parsedtxt_nouns, file = "out/all_nouns.rdata")
#combine the chunks into one file
f <- list.files("out/parsing_nouns/", full.names = T)
files <- list()
for(i in 1:length(f)){
  load(f[i])
  files[[i]] <- current_nouns
  print(i)
}
parsedtxt <- rbindlist(files)
save(parsedtxt, file = "out/results_parsing.rdata")


#combine the chunks into one file
f <- list.files("out/parsing_chunks/", full.names = T)
files <- list()
for(i in 1:length(f)){
  load(f[i])
  files[[i]] <- parsedtxt_chunk
  print(i)
}
parsedtxt <- rbindlist(files)
save(parsedtxt, file = "out/results_parsing.rdata")

#----
# Remove content based on NER

rm(list = ls())
set.seed(1)
load("out/duplicate_ids.rdata")
load("out/all_nouns.rdata")

#entities to remove
entity_rm <- c("CARDINAL_B", "CARDINAL_I", #numbers/counts
               "DATE_B", "DATE_I",
               "EVENT_B", "EVENT_I", #B: Twilight, September, Ordinance, I: Pull, Over, 30, Year, Awards
               "FAC_B", "FAC_I", #B: facility, i.e. McDonalds, Derrick, Sycamore, Taylor, Market, I: Street, Park, Avenue
               "GPE_B", "GPE_I", #Geopolitical Entity -- Attica, Lafayette, Laundromat, Rockville
               "LANGUAGE_B", #B: Department ???
               #"LAW_B", "LAW_I", # Keep -- Resolution, Chapter, Article, I: #, 9
               "LOC_B", "LOC_I", #Crystal, Badlands, West, Perry, I: Attachment, Street
               "MONEY_B", "MONEY_I", #B: 1,487,455, 35,000/yr, I: Million -- this seems very reliable and also interesting, even if not for our purposes
               #"NORP_B", # Keep -- Nationalities or religious or political groups; keep -- Americans, Mexican, British, Jewish
               "ORDINAL_B", #first, second, 3rd
               "ORG_B", "ORG_I", #unsure -- Council, McDonald, ADA, DLZ, I: Indiana, Council, Department for Appropriations
               "PERCENT_B", "PERCENT_I", #15 % - mph a.m.-12
               "PERSON_B", "PERSON_I", #Grimmett, Ouabache, Askren, Wayne
               "PRODUCT_I", #Cole, Danko, Engine, I: St
               "QUANTITY_B", "QUANTITY_I", #54,000, 75/25, ninety
               "TIME_B", "TIME_I")#, #B: 5:00, I: P.M., hours
#"WORK_OF_ART_B", "WORK_OF_ART_I") # Keep -- B: Street, Attica, Confined, Riverboat, I: Ravine, Retirement Fund, Police

f <- list.files("out/parsing_chunks", full.names = T)
for(i in 1:length(f)){
  load(f[i])
  parsedtxt_chunk <- parsedtxt_chunk[!parsedtxt_chunk$doc_id%in%duplicate_ids,]
  #in attica, this removes about 40k out of 240k token instances
  parsedtxt_chunk <- parsedtxt_chunk[!parsedtxt_chunk$entity%in%c(entity_rm),]
  #remove everything except adjectives, nouns, verbs and proper nouns that are also nouns
  parsedtxt_chunk <- parsedtxt_chunk[parsedtxt_chunk$pos%in%c("ADJ", "NOUN", "PROPN", "VERB"),]
  #PROPN_unique <- unique(parsedtxt_chunk$lemma[parsedtxt_chunk$pos=="PROPN"])
  parsedtxt_chunk$PROPN_and_NOUN <- parsedtxt_chunk$pos=="PROPN" & parsedtxt_chunk$lemma%in%parsedtxt_nouns
  parsedtxt_chunk <- parsedtxt_chunk[!(parsedtxt_chunk$pos=="PROPN" & parsedtxt_chunk$PROPN_and_NOUN==F),]
  parsedtxt_chunk <- subset(parsedtxt_chunk, select = -PROPN_and_NOUN)
  
  #----
  # Remove words that contain numbers, because the parsing doesn't get stuff like 14th
  parsedtxt_chunk <- parsedtxt_chunk[!str_detect(parsedtxt_chunk$lemma, "[0-9]"),]
  
  #----
  # Convert to tokens object
  tks <- as.tokens(parsedtxt_chunk, use_lemma = T)
  
  #----
  #remove words that are too short, not english or stopwords
  uniquetokens <- unique(parsedtxt_chunk$lemma)
  
  #create a vector of words to be removed: short words, non-English words, stopwords
  #get words that are too short
  tooShort <- uniquetokens[nchar(uniquetokens)<3]
  
  #spellchecking
  spellingErrors <- sapply(uniquetokens, hunspell_check)
  spellingErrors <- names(spellingErrors)[spellingErrors==F]
  
  #get stopwords
  removeWords <- unique(c(tooShort, spellingErrors, stopwords()))
  
  #remove short words, non-english words and stopwords
  tks = tokens_select(tks, removeWords, "remove", valuetype = "fixed")
  rm(spellingErrors, removeWords)
  
  #----
  # Remove empty docs
  empty_docs <- which(unlist(lapply(tks, length))==0)
  if(length(empty_docs)>0){
    tks <- tks[-empty_docs]
  }
  
  save(tks, file = str_replace(f[i], "parsing_chunks", "preprocessing_chunks_1"))
  
}

f <- list.files("out/preprocessing_chunks_1", full.names = T)
load(f[1])
tks_all <- tks
for(i in 2:length(f)){
  load(f[i])
  tks_all <- tokens(c(tks_all, tks))
}
save(tks_all, file = "out/tokens_all.rdata")

#----
load("../04_parseText/out/citydocs.rdata")
all_cities <- unique(d$State_City)
for(i in 1:length(all_cities)){
  #what are the document ids of this city
  d_city_ids <- d$id[d$State_City == all_cities[i]]
  #what are the indices of this city's document in the tokens file?
  tks_city_ids <- which(names(tks_all)%in%d_city_ids)
  #which among them are duplicated
  which_duplicated <- duplicated(tks_all[tks_city_ids])
  which_duplicated <- tks_city_ids[which_duplicated]
  #kick those out of the tokens file
  if(length(which_duplicated>0)){
    tks_all <- tks_all[-which_duplicated]
  }
  print(i)
  print(length(tks_all))
}
save(tks_all, file = "out/preprocessing_finished.rdata")

#----

#in attica, this removes about 40k out of 240k token instances
parsedtxt <- parsedtxt[!parsedtxt$entity%in%c(entity_rm),]

#----
# Remove content based on POS

#remove everything except adjectives, nouns, verbs and proper nouns that are also nouns
parsedtxt <- parsedtxt[parsedtxt$pos%in%c("ADJ", "NOUN", "PROPN", "VERB"),]
#PROPN_unique <- unique(parsedtxt$lemma[parsedtxt$pos=="PROPN"])
NOUN_unique <- unique(parsedtxt$lemma[parsedtxt$pos=="NOUN"])
parsedtxt$PROPN_and_NOUN <- parsedtxt$pos=="PROPN" & parsedtxt$lemma%in%NOUN_unique
parsedtxt <- parsedtxt[!(parsedtxt$pos=="PROPN" & parsedtxt$PROPN_and_NOUN==F),]
parsedtxt <- subset(parsedtxt, select = -PROPN_and_NOUN)

#----
# Remove words that contain numbers, because the parsing doesn't get stuff like 14th
parsedtxt <- parsedtxt[!str_detect(parsedtxt$lemma, "[0-9]"),]

#----
# Convert to tokens object
tks <- as.tokens(parsedtxt, use_lemma = T)
docvars(tks) <- d[match(names(tks), d$id),]

#----
#remove words that are too short, not english or stopwords
uniquetokens <- unique(parsedtxt$lemma)

#create a vector of words to be removed: short words, non-English words, stopwords
#get words that are too short
tooShort <- uniquetokens[nchar(uniquetokens)<3]

#spellchecking
spellingErrors <- sapply(uniquetokens, hunspell_check)
spellingErrors <- names(spellingErrors)[spellingErrors==F]

#get stopwords
removeWords <- unique(c(tooShort, spellingErrors, stopwords()))

#remove short words, non-english words and stopwords
tks = tokens_select(tks, removeWords, "remove", valuetype = "fixed")
rm(spellingErrors, removeWords)

#----
# Remove duplicated docs
which_duplicated <- duplicated(tks)
tks <- tks[!which_duplicated]

#----
# Remove empty docs
tks <- tks[-which(unlist(lapply(tks, length))==0)]

#----
# Save
save(tks, file = "out/preprocessed_tks.rdata")


#-----
# The above is prettty messy, the following helps with cleanup

#1
library(stringr)
library(data.table)

#clean up the mess
f <- list.files("out/preprocessing_chunks_1", full.names = T)
chunks_ids <- list()
for(i in 1:length(f)){
  load(f[i])
  chunkname <- str_replace(f[i], "out/preprocessing_chunks_1/", "") %>% str_replace(".rdata", "")
  ids <- names(tks)
  chunks_ids[[i]] <- data.table(chunkname, ids)
}
chunks_ids <- rbindlist(chunks_ids)

#2
f <- list.files("out/preprocessing_chunks_1", full.names = T)
load(f[1])
tks_all <- tks
for(i in 2:length(f)){
  load(f[i])
  tks_all <- tokens(c(tks_all, tks))
}

#----
load("../04_parseText/out/citydocs.rdata")
all_cities <- unique(d$State_City)
for(i in 1:length(all_cities)){
  d_city_ids <- d$id[d$State_City == all_cities[i]]
  tks_city <- tks_all[which(names(tks_all)%in%d_city_ids)]
  which_duplicated <- duplicated(tks_city)
  tks_city <- tks_all[which(names(tks_all)%in%d_city_ids)][!which_duplicated]
}


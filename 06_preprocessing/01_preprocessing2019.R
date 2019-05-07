library('quanteda')
library('spacyr')
spacy_initialize()
library('hunspell')
library('stringr')
library('ff')
set.seed(1)

#we are very RAM limited here, so only do whats necessary

load("../04_parseText/out/results_parsed_all.rdata")

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
#combine the chunks into one file
f <- list.files("out/parsing_chunks/", full.names = T)
files <- list()
for(i in 1:length(f)){
  load(f[i])
  files[[i]] <- parsedtxt_chunk
}
parsedtxt <- rbindlist(files)
save(parsedtxt, file = "out/results_parsing.rdata")

#----
# Remove content based on NER

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

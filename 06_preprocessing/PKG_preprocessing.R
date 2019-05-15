
#----
# Function 1

spacy_parse_chunk <- function(texts, ids, chunk = NULL){
  
  #put into data table
  dt <- data.table::data.table(text = texts, id = ids)
  
  #decide whether to chunk
  if(is.null(chunk)==T){
    if(nrow(dt)>=5000){
      chunk <- T
    }else{
      chunk <- F
    }
  }
  
  #if no chunking
  if(chunk==F){
    #create quanteda corpus
    crps <- quanteda::corpus(dt$text, docnames = dt$id)
    #parse with spacy
    parsed <- spacyr::spacy_parse(crps)
  }
  
  #if chunking
  if(chunk==T){
    files <- list()
    files_int <- 1
    for(i in chunk(from = 1, to = nrow(dt), by = 5000)){
      #create chunked data frame
      dt_chunk <- dt[c(min(i):max(i)),]
      #create chunked quanteda corpus
      crps_chunk <- quanteda::corpus(dt_chunk$text, docnames = dt_chunk$id)
      #parse with spacy
      parsed_chunk <- spacyr::spacy_parse(crps_chunk)
      #save
      fname <- paste0("tmp_spacy_chunk_", min(i), "_", max(i), ".rdata")
      files[[files_int]] <- fname
      save(parsed_chunk, file = fname)
      files_int <- files_int+1
    }
    parsed_chunks <- list()
    for(i in 1:length(files)){
      load(files[[i]])
      parsed_chunks[[i]] <- parsed_chunk
    }
    parsed <- rbindlist(parsed_chunks)
    #little hack to allow quanteda to recognize spacyr objects that were pasted together
    if(any(class(parsed)=="spacyr_parsed")==F){
      class(parsed) <- c("spacyr_parsed", "data.frame")
    }
    file.remove(unlist(files))
  }
  
  #return the resultant spacy parsed data.table
  return(parsed)
  
}

#----
# Function 2

preprocessWebsite <- function(parsed, custom_nouns = NULL){

  #----
  # Entity removal
  
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
                 "TIME_B", "TIME_I")#"WORK_OF_ART_B", "WORK_OF_ART_I") # Keep -- B: Street, Attica, Confined, Riverboat, I: Ravine, Retirement Fund, Police
  
  #actual removal here
  #in attica, this removes about 40k out of 240k token instances
  parsed <- parsed[!parsed$entity%in%c(entity_rm),]
  
  #----
  # Remove POS
  
  #remove everything except adjectives, nouns, verbs and proper nouns that are also nouns
  parsed <- parsed[parsed$pos%in%c("ADJ", "NOUN", "PROPN", "VERB"),]
  #deal with proper nouns
  if(is.null(custom_nouns)==F){
    nouns_to_keep <- custom_nouns
  }else{
    nouns_to_keep <- unique(parsed$lemma[parsed$pos=="NOUN"])
  }
  parsed$PROPN_and_NOUN <- parsed$pos=="PROPN" & parsed$lemma%in%nouns_to_keep
  parsed <- parsed[!(parsed$pos=="PROPN" & parsed$PROPN_and_NOUN==F),]
  parsed <- subset(parsed, select = -PROPN_and_NOUN)
  
  #----
  # Remove words that contain numbers, because the parsing doesn't get stuff like 14th
  # This is done in ADDITION to the step below (which would remove the numbers too)
  parsed <- parsed[!stringr::str_detect(parsed$lemma, "[0-9]"),]
  
  #----
  # Remove any character that is not part of the English alphabet
  parsed$lemma <- str_remove_all(parsed$lemma, "[^A-Za-z]")
  
  #----
  # Remove words if they are entirely empty
  parsed <- parsed[parsed$lemma != "",]
  
  #----
  # Convert to tokens object
  tks <- quanteda::as.tokens(parsed, use_lemma = T)
  
  #----
  #remove words that are too short, not english or stopwords
  uniquetokens <- unique(parsed$lemma)
  
  #create a vector of words to be removed: short words, non-English words, stopwords
  #get words that are too short
  tooShort <- uniquetokens[nchar(uniquetokens)<3]
  
  #spellchecking
  spellingErrors <- sapply(uniquetokens, hunspell::hunspell_check)
  spellingErrors <- names(spellingErrors)[spellingErrors==F]
  
  #get stopwords
  removeWords <- unique(c(tooShort, spellingErrors, quanteda::stopwords()))
  
  #remove short words, non-english words and stopwords
  tks = quanteda::tokens_select(tks, removeWords, "remove", valuetype = "fixed")
  rm(spellingErrors, removeWords)
  
  #----
  # Remove empty docs
  empty_docs <- which(unlist(lapply(tks, length))==0)
  if(length(empty_docs)>0){
    tks <- tks[-empty_docs]
  }
  
  #----
  # Remove duplicated docs
  which_duplicated <- duplicated(tks)
  tks <- tks[!which_duplicated]
  
  return(tks)

}

#----
# Wrapper for functions 1 and 2

preprocessingWrapper <- function(texts, ids, chunk = NULL, custom_nouns = NULL){
  parsed <- spacy_parse_chunk(texts = texts, ids = ids, chunk = chunk)
  tks <- preprocessWebsite(parsed = parsed, custom_nouns = custom_nouns)
  return(tks)
}

##How to use:
# result <- preprocessingWrapper(df$text, df$id)

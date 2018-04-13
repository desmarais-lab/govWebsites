library('quanteda')

#tokenize, removing numbers, punctuations, symbols, twitter symbols, hyphens and urls
# we do NOT remove separators, because we need the \n newline indicators
# (note that this is because what = "word" and remove_punct = T, which renders
# remove_separators = F redundant; it is here only for clarity)
tt = tokens(a$text, what = "word",
       remove_numbers = T, remove_punct = T,
       remove_symbols = T, remove_separators = F,
       remove_twitter = T, remove_hyphens = T, remove_url = T)

#since we didn't remove separators, remove spaces
tt = tokens_select(tt, " ", "remove")

#remove date-related words
date_words <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
date_words <- c(date_words, "January","February","March","April","May","June","July","August","September","October","November","December")
date_words <- c(date_words, "Mon","Tue","Wed","Thu","Fri","Sat","Sun")
date_words <- c(date_words, "am", "pm")
date_words <- c(date_words, tolower(date_words))
tt = tokens_select(tt, date_words, "remove")
rm(date_words)

#removing words containing underscores
tt = tokens_select(tt, "\\w*_\\w*", "remove", valuetype = "regex")

#remove all remaining non-character tokens
tt = tokens_select(tt, "[^A-Za-z\n]", "remove", valuetype = "regex")

#set everything to lowercase
tt = tokens_tolower(tt)

## Duplicate line removal

#convert from tokens object to list of character vectors
tt2 <- lapply(tt, paste, collapse = " ")
#split on newlines; creates nested list (unnecessarily)
tt2 <- lapply(tt2, str_split, "\n")
#unnest - back to a list of character vectors
tt2 <- lapply(tt2, unlist)
#remove empty spaces at the ends
tt2 <- lapply(tt2, str_trim)

#concatenate all lines of a city's documents
linesRemove <- as.character(unlist(tt2))
#create table
linesRemove <- table(linesRemove)
#get the lines that occur more than duplicateLinesThreshold times
duplicateLinesThreshold <- 10
linesRemove <- linesRemove[linesRemove>duplicateLinesThreshold]
#linesRemove <- sort(linesRemove, decreasing = T)
linesRemove <- names(linesRemove)
#remove them from each of the documents in the list of character vectors
tt2 <- lapply(tt2, function(x){x[!x%in%linesRemove]})

#convert back to a single character vector
#(i.e. one character vector element per document)
tt2 <- lapply(tt2, paste, collapse = " ")
tt2 <- as.character(unlist(tt2))

#((lemmatization)) - too slow
#((entity removal)) - doesn't seem to work at the moment
#remove words under 3 characters


# library(spacyr)
# spacy_initialize()
# parsedtxt <- spacy_parse(tt2)
# parsedtxt2 <- spacy_parse(tt2, pos = F, tag = F, lemma = T, entity = F, dependency = F)


#convert back to a quanteda tokens object
tt3 <- tokens(tt2)

uniquetokens <- unique(unlist(tt3))

#create a vector of words to be removed: short words, non-English words, stopwords
#get words that are too short
tooShort <- uniquetokens[nchar(uniquetokens)<3]

#get words that are not in an english dictionary
library('hunspell')
library('parallel')
ncores <- detectCores() - 1
cl <- makeForkCluster(ncores)
spellingErrors <- parSapply(cl, uniquetokens, FUN = hunspell_check)
stopCluster(cl)
spellingErrors <- names(spellingErrors)[spellingErrors==F]
#Note that this also tends to get rid of the proper nouns because they are not capitalized anymore
#this suits us just fine

#get stopwords
removeWords <- unique(c(spellingErrors, tooShort, stopwords()))

#remove short words, non-english words and stopwords
tt3 = tokens_select(tt3, removeWords, "remove")
rm(spellingErrors, tooShort, removeWords)

#everything that removes entire documents should be at the end

#calculate token statistics
ntokens <- unlist(lapply(tt3, length))
nuniquetokens <- unlist(lapply(tt3, function(x){length(unique(x))}))
tokenratio <- nuniquetokens/ntokens
#remove documents in which too many words are the same
#remove documents with too few unique tokens
removeDocs <- which(as.numeric(tokenratio)<0.15)
#remove documents with too few tokens
removeDocs <- c(removeDocs, which(as.numeric(ntokens)<50))

#there are still some documents that are exact duplicates of one another
tt4 <- unlist(lapply(tt3, paste, collapse = " "))
duplicatedDocs <- which(duplicated(tt4)==T)
removeDocs <- unique(c(duplicatedDocs, removeDocs))

a$text <- tt4
a <- a[-removeDocs,]

library('tibble')
library('stringr')
library('tools')
library('pbapply')
library('tm')
library('dplyr')
library('quanteda')
library('hunspell')
library('ggplot2')
library('hashmap')
library('profvis')

#preprocessingTesting <- function(K){

#function to decide whether a line in a document is kept
#currently, this happens if it occurs no more than 10 times in other city docs
#also concatenates the documents into character vectors (since they were lists of lines so far)
cleanup <- function(i, k = cutoff){
  keep <- which(docDuplicates[[i]]<=k)
  cleanedDoc <- str_c(d$doc[[i]][keep], collapse = " ")
  return(cleanedDoc)
}

#use the function (with a progress bar) on the whole corpus
d$doc <- as.character(pbsapply(1:nrow(d), cleanup))

#remove empty
d <- d[d$doc!="character(0)",]

### END OF INTERESTING STUFF

d$xml <- str_detect(d$doc, "<?xml version=") #note which docs are xml
d <- filter(d, d$xml==F) #Drop rows that are xml docs
d <- select(d, -xml) #Drop xml variable

#Remove bullet points
d$doc <- str_replace_all(d$doc, "  o ", " ")

#Remove extraneous whitespaces
d$doc <- stripWhitespace(d$doc)

#Convert text to UTF-8
d$doc <- iconv(d$doc, "latin1", "UTF-8")

#remove empty files
d <- d[!d$doc=="",]
d <- d[!d$doc==" ",]

#remove extra whitespaces, trim, remove empty documents, remove robots.txt files
d$doc <- gsub("\\s+"," ", d$doc)
d$doc <- str_trim(d$doc, side = "both")
d <- d[d$doc!="",]
d <- d[d$filename!="robots.txt",]

#remove files that cause problems
d <- d[d$filename!="indypotholeviewer.txt",]

#Hunspell
source("hunspellParallel.R")

#Everything to lowercase
d$doc <- tolower(d$doc)

#Remove cities with only one document
d <- d[d$Name%in%names(table(d$Name))[table(d$Name)>1],]

#remove documents with nonsensical texts
d <- d[d$doc!="character",]
d <- d[!nchar(d$doc)<50,]

#save results
#save(d, file = "./rfiles/d_noduplicates2.Rdata")

#remove weird non-utf-8 characters
## ... by removing anything that's not a regular word
d$doc <- str_replace_all(d$doc, "[^0-9A-Za-z ]", "")
d$doc <- gsub("\\s+"," ", d$doc)
source("hunspellParallel.R")

#remove individual letters
individual_letters <- c("b","c","d","e","f","g","h","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
d$doc <- removeWords(d$doc, individual_letters)
d$doc <- gsub("\\s+"," ", d$doc)

#remove documents in which too many words are the same
countTokens <- function(doc){
  doc <- tokenize(doc, simplify = T)
  l <- length(doc)
}
countUniqueTokens <- function(doc){
  doc <- tokenize(doc, simplify = T)
  l <- length(unique(doc))
}
d$ntokens <- sapply(d$doc, countTokens)
d$nuniquetokens <- sapply(d$doc, countUniqueTokens)
d$tokenratio <- d$nuniquetokens/d$ntokens
#remove documents with too few unique tokens
d <- d[d$tokenratio>0.15,]
#remove documents with too few tokens
d <- d[d$ntokens>50,]

#remove a few empty documents that ended up in there at some point
d <- d[is.na(d$ext)==F,]

#remove city names
citynames <- unique(d$Name)
citynames <- c(citynames, tolower(citynames))
d$doc <- removeWords(d$doc, citynames)

#there are still some documents that are exact duplicates of one another
#remove all but one
d <- d[!duplicated(d$doc)==T,]

#remove stopwords with quanteda
#stopwords()
d$doc <- removeWords(d$doc, stopwords())

#update information on tokens
d$ntokens <- sapply(d$doc, countTokens)
d$nuniquetokens <- sapply(d$doc, countUniqueTokens)
d$tokenratio <- d$nuniquetokens/d$ntokens

#save
save(d, file = paste("./rfiles/d_test_K", cutoff, ".rdata", sep = ""))

#}
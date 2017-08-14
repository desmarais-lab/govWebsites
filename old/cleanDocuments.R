library('SpeedReader')
library('quanteda')
library('tibble')
library('dplyr')
library('stringr')

load("rfiles/d.Rdata")
#d <- d[d$Name%in%c("Attica", "Auburn", "Brazil", "Gary"),]


#tf-idf for the whole corpus

#create corpus
crps <- corpus(d$doc)
docvars(crps, "City") <- d$Name
cities <- unique(docvars(crps, "City"))
remove_words <- list()

for(i in 1:length(cities)){
  city <- corpus_subset(crps, City == cities[i])
  city <- dfm(city, ngrams = 3)
  #binarize document-term matrix
  city@x <- rep(1, length(city@x))
  #column sums of binarized dtm
  binTF <- colSums(city)
  #how many documents does the city have
  cityDocs <- dim(city)[1]
  #remove n-grams which occur in more than 10% of city documents
  remove <- names(binTF[binTF>(0.1*cityDocs)])
  remove_words <- c(remove_words, list(remove))
}


#################

#get rid of underscores
remove_words2 <- lapply(remove_words, function(X){str_replace_all(X, "_", " ")})

#################

citytable <- sapply(unique(d$Name), function(x){length(d$doc[d$Name==x])})
citynames <- names(citytable)

##############################################################

cityindex <- rep(names(citytable), each = citytable)
remove_words2 <- lapply(remove_words, function(X){str_replace_all(X, "_", " ")})
d$doc2 <- d$doc
#loop over cities
for(k in 1:nrow(d)){
  
  cityname <- cityindex[k]
  cityind <- match(cityname, names(citytable))
  
  #loop over words to be removed for this document
  a <- d$doc[k]
  for(i in 1:length(remove_words2[[cityind]])){
    a <- clean_document_text(a, regex = remove_words2[[cityind]][i])
    a <- str_c(a, collapse = " ")
  }
  
  d$doc2[k] <- a
  
}

save(d, file = "rfiles/d_cleaned.Rdata")

#ptm <- proc.time()
#a <- a1
#for(i in 1:length(remove_words2[[cityind]])){
#a <- str_replace_all(a, remove_words2[[cityind]][i], "")
#a <- str_replace_all(a, "\\s+", " ")
#a <- str_trim(a)
#}
#proc.time() - ptm
#aMN <- a

#ptm <- proc.time()
#a <- a1
#for(i in 1:length(remove_words2[[cityind]])){
#  a <- clean_document_text(a, regex = remove_words2[[cityind]][i])
#  a <- str_c(a, collapse = " ")
#}
#proc.time() - ptm
#aMD <- a

#identical(aMD,aMN)
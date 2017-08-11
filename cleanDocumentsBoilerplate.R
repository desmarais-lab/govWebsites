library('SpeedReader')
library('quanteda')
library('tibble')
library('dplyr')
library('stringr')

load("rfiles/d.Rdata")
d <- d[d$Name%in%c("Attica", "Auburn", "Brazil", "Gary"),]


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

##############################################################

#create a new document variable
d$doc2 <- d$doc

#loop over cities
for(k in 1:length(cities)){
  #if not the first city, start in this row
  if(k>1){
    citystart <- sum(citytable[1:(k-1)])+1
  }
  #if the first city, start on the first row
  else{
    citystart <- 1
  }
  
  #how many documents does the current city have?
  citylength <- length(d$doc[d$Name==citynames[k]])
  
  #loop over city documents
  for(j in citystart:(citystart+citylength-1)){
    #loop over words to be removed for this city
    for(i in 1:length(remove_words[[k]])){
      #use clean_document_text() from SpeedReader
      #supply one character vector (n-gram)
      #paste the result back together again
      #and assign it to the appropriate document
      d$doc2[j] <- str_c(clean_document_text(d$doc2[j], regex = remove_words[[k]][i]), collapse = " ")
    }
  }
}


### PARALLELIZED VERSION OF THE ABOVE:

library(foreach)
library(doParallel)

ncores <- detectCores()-1
cl <- makeForkCluster(ncores)
registerDoParallel(cl)

d$doc2 <- d$doc
#loop over cities
foreach(k = 1:length(cities)) %dopar% {
  #if not the first city, start in this row
  #if the first city, start on the first row
  if(k>1){
    citystart <- sum(citytable[1:(k-1)])+1
  }
  else{
    citystart <- 1
  }
  
  #how many documents does the current city have?
  citylength <- length(d$doc[d$Name==citynames[k]])
  
  #loop over city documents
  for(j in citystart:(citystart+citylength-1)){
    #loop over words to be removed for this city
    for(i in 1:length(remove_words[[k]])){
      #use clean_document_text() from SpeedReader
      #supply one character vector (n-gram)
      #paste the result back together again
      #and assign it to the appropriate document
      d$doc2[j] <- str_c(clean_document_text(d$doc2[j], regex = remove_words[[k]][i]), collapse = " ")
    }
  }
}

stopCluster(cl)

######################################################################
### more efficient implementation (or more embarassingly parallel) ###
######################################################################

cityindex <- rep(names(citytable), each = citytable)
remove_words2 <- lapply(remove_words, function(X){str_replace_all(X, "_", " ")})

library(foreach)
library(doParallel)

ncores <- detectCores()-1
cl <- makeForkCluster(ncores)
registerDoParallel(cl)

d$doc2 <- d$doc
#loop over cities
foreach(k = 1:nrow(d)) %dopar% {
  
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

stopCluster(cl)


##################################################################


#-----------------------------------------------------------------------------#
# File description:
# Reading in the city documents
# Input: 
# Output:
#-----------------------------------------------------------------------------#

library("stringr")
library("tools")
library("tibble")
library('pbapply')
library('parallel')
library('hunspell')

readCityDocuments <- function(filepath){

  ## Reading in the data
  #create a list of all files in all subdirectories
  f <- list.files(path = filepath, recursive = T)
  
  #file types
  ext <- file_ext(f) #get file extension
  folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]
  
  #store objects in tibble
  d <- tibble(path = str_c(filepath, f, sep = "/"), 
              folder = str_c(filepath, folder, sep = "/"),
              filename,
              ext)
  
  #read in file text as list of lines
  d$doc <- pbsapply(d$path, function(x){list(readLines(x))})
  
  #extract city from directory
  d$city <- str_split_fixed(d$path, str_c(filepath, "/"), 2)[,2]
  d$city <- str_split_fixed(d$city, "/", 2)[,1]
  
  return(d)

}

mergeCityCoefficients <- function(doc.data.frame, coeff.file){
  # load the city coefficients from file
  coefficients <- readRDS(coeff.file)
  # merge city coefficients with the documents
  doc.data.frame <- merge(doc.data.frame, 
                          coefficients, 
                          by.x = "city", by.y = "Website")
  return(doc.data.frame)
}


library('tm')
library('dplyr')

# Do all the preprocessing that needs to be done before removing duplicate lines
# remove any words relating to dates (since they can easily make lines unique)
# remove words containing underscores
# remove punctuation
# remove numbers
preprocessing_1 <- function(doc.data.frame, coeff.file){

  #convert everything to UTF-8
  print("Converting the documents to UTF-8:")
  doc.data.frame$doc <- pbsapply(doc.data.frame$doc, iconv, "latin1", "UTF-8")
  
  #remove date-related words
  print("Removing dates and times:")
  date_words <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  date_words <- c(date_words, "January","February","March","April","May","June","July","August","September","October","November","December")
  date_words <- c(date_words, "Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  date_words <- c(date_words, "am", "pm")
  date_words <- c(date_words, tolower(date_words))
  doc.data.frame$doc <- pbsapply(doc.data.frame$doc, removeWords, words = date_words)
  
  #Removing words containing underscores
  print("Removing words containing underscores:")
  doc.data.frame$doc <- pbsapply(doc.data.frame$doc, str_replace_all, "\\w*_\\w*", "")
  
  #Removing punctuation
  print("Removing punctuation:")
  doc.data.frame$doc <- pbsapply(doc.data.frame$doc, removePunctuation)
  
  #Removing numbers
  print("Removing numbers:")
  doc.data.frame$doc <- pbsapply(doc.data.frame$doc, removeNumbers)
  
  return(doc.data.frame)

}

library('hashmap')

#iterate over cities, creating a list for each
#each of these lists contains one numerical vector for each document
#each vector contains one element for each line in the document
#each of these elements denominates the number of times that line occurs within the city
proc_city <- function(df){
  
  #concatenate documents of the current city
  #lines <- unlist(d$doc[cityindices])
  lines <- unlist(str_split(df$doc, "\n"))
  names(lines) <- 1:length(lines)
  
  #initialize a hashmap
  #the key/value generated here doesn't matter, I never use it
  HH <- hashmap('INIT', -1)
  
  #for each in the city's documents...
  for(i in 1:length(lines)){
    #look for the key and see if it already has a value (i.e. the count)
    value <- HH[[lines[i]]]
    if(is.na(value)) {
      value = 1L #if not, make it 1
    } else {
      value = value + 1L #if yes, add 1
    }
    #then add the result back into the hashmap
    HH$insert(lines[i], value)
    
  }
  
  #make a quick and dirty function to find the lines...
  #... of one document and get the respective values from the hashmap
  duplicateLinesHH <- function(k){
    return(HH$find(unlist(str_split(d$doc[[k]], "\n"))))
  }

  #do this for all the documents, creating a list of vectors
  #one vector for each document
  #each of which contains the number of times a line in that document is present in the city
  docDuplicatesHH <- pblapply(1:nrow(d), duplicateLinesHH)
  
  return(docDuplicatesHH)
  
}

# Use the function defined above to carry out the removal of duplicate lines
# Does NOT return the document data frame
# Returns a list consisting of numerical vectors for each document...
# ... which describe how many times each line within that document is...
# ... duplicated across that city

#source('functions/getCores.R')

# findDuplicates <- function(d){
# 
#   #citytable <- table(d$City)
#   #citynames <- d$City
#   
#   cl <- makeForkCluster(getCores(3500))
#   docDuplicates <- pbsapply(1:length(citytable), proc_city, citytable, cl = cl)
#   stopCluster(cl)
#   
#   #unlist only the outer list
#   docDuplicates <- unlist(docDuplicates, recursive = F)
#   
#   return(docDuplicates)
# 
# }

removeDuplicates <- function(i, k = 10){
  keep <- which(docDuplicates[[i]]<=k)
  cleanedDoc <- str_c(unlist(str_split(d$doc[[i]], "\n"))[keep], collapse = " ")
  return(cleanedDoc)
}

#use the function (with a progress bar) on the whole corpus
#d$doc <- as.character(pbsapply(1:nrow(d), removeDuplicates))



# Do all the preprocessing after removing duplicate lines
preprocessing_2 <- function(d){
  
  #Remove bullet points
  print("Removing bullet points:")
  d$doc <- str_replace_all(d$doc, "  o ", " ")
  
  #Remove extraneous whitespaces
  print("Removing extraneous whitespaces:")
  d$doc <- stripWhitespace(d$doc)
  
  #remove empty files
  print("Removing empty files:")
  d <- d[!d$doc=="",]
  d <- d[!d$doc==" ",]
  
  #remove extra whitespaces, trim, remove empty documents, remove robots.txt files
  print("Removing extraneous whitespaces:")
  d$doc <- gsub("\\s+"," ", d$doc)
  print("Removing whitespaces at the beginning and end of strings:")
  d$doc <- str_trim(d$doc, side = "both")
  print("Removing empty files:")
  d <- d[d$doc!="",]
  d <- d[!d$doc==" ",]
  #print("Removing robots.txt files:")
  #d <- d[d$filename!="robots.txt",]
  
  #Hunspell
  #print("Use Hunspell spellchecking to remove non-English words:")
  source("hunspellParallel.R")
  
  #Everything to lowercase
  print("Setting everything to lowercase.")
  d$doc <- tolower(d$doc)
  
  #remove documents with nonsensical texts
  print("Removing documents with nonsensical texts.")
  d <- d[d$doc!="character",]
  d <- d[!nchar(d$doc)<50,]
  
  #remove weird non-utf-8 characters
  ## ... by removing anything that's not a regular word
  print("Removing non-UTF-8 characters.")
  d$doc <- str_replace_all(d$doc, "[^0-9A-Za-z ]", "")
  d$doc <- gsub("\\s+"," ", d$doc)
  #print("Use Hunspell spellchecking to remove non-English words AGAIN:")
  source("hunspellParallel.R")
  
  #remove individual letters
  print("Removing individual letters.")
  individual_letters <- c("b","c","d","e","f","g","h","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  d$doc <- removeWords(d$doc, individual_letters)
  d$doc <- gsub("\\s+"," ", d$doc)
  
  #remove documents in which too many words are the same
  print("Removing documents in which too many words are the same.")
  countTokens <- function(doc){
    doc <- as.character(tokens(doc))
    l <- length(doc)
    return(l)
  }
  countUniqueTokens <- function(doc){
    doc <- as.character(tokens(doc))
    l <- length(unique(doc))
    return(l)
  }
  d$ntokens <- sapply(d$doc, countTokens)
  d$nuniquetokens <- sapply(d$doc, countUniqueTokens)
  d$tokenratio <- d$nuniquetokens/d$ntokens
  
  #remove documents with too few unique tokens
  print("Removing documents with too few unique tokens.")
  d <- d[d$tokenratio>0.15,]
  #remove documents with too few tokens
  print("Removing documents with too few tokens.")
  d <- d[d$ntokens>50,]
  
  #remove a few empty documents that ended up in there at some point
  #d <- d[is.na(d$ext)==F,]
  
  #remove city names
  print("Removing city names.")
  d$doc <- removeWords(d$doc, city)
  
  #there are still some documents that are exact duplicates of one another
  #remove all but one
  print("Removing documents that are still duplicates of one another.")
  d <- d[!duplicated(d$doc)==T,]
  
  #remove stopwords with quanteda
  #stopwords()
  print("Removing stopwords with quanteda.")
  d$doc <- removeWords(d$doc, stopwords())
  
  #update information on tokens
  print("Re-calculate token statistics.")
  d$ntokens <- sapply(d$doc, countTokens)
  d$nuniquetokens <- sapply(d$doc, countUniqueTokens)
  d$tokenratio <- d$nuniquetokens/d$ntokens
  
  #remove terms that occur in only 1 document
  source('functions/occuranceRemove.R')
  
  #remove terms with less than 3 characters
  tokenLenRemove <- function(charstring, lenCutoff = 3){
    tokenObj <- tokens(charstring)
    tokenObjChar <- as.character(tokenObj)
    tokenLen <- nchar(tokenObjChar)
    errors <- tokenObjChar[tokenLen<lenCutoff]
    if(length(errors)>0){
      output <- tokens_remove(tokens(charstring), errors)
      output <- str_c(output[[1]], collapse = " ")
    }
    else{
      output <- charstring
    }
    #in case everything is removed, make an empty string
    if(identical(output, character(0))){
      output <- ""
    }
    return(output)
  }
  
  ncores <- detectCores() - 1
  cl <- makeCluster(ncores, type = "FORK")
  d$doc <- parSapply(cl, d$doc, FUN = tokenLenRemove)
  stopCluster(cl)
  
  return(d)
  
}

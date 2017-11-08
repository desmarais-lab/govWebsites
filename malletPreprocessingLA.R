library('tibble')
library('stringr')
library('tools')
library('pbapply')
library('tm')
library('dplyr')
library('quanteda')
library('hunspell')
library('hashmap')
library('profvis')

filepath <- "./websites/scraping/LA/websites/"

## Reading in the data
f <- list.files(path = filepath, recursive = T) #create a list of all files in all subdirectories
f <- f[-which(f=="URLs.txt")] #remove URLs.txt

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

#import and merge in city coeffs
load("./data/louisiana.rdata")
louisiana$Website <- str_extract(louisiana$Website, "//(.*)") %>%
  str_replace_all("/", "")
d <- merge(d, louisiana, by.x = "city", by.y = "Website")

save(d, file = "rfiles/dLA1.rdata")
load("rfiles/dLA1.rdata")

#Some preprocessing has to be done before cleaning, because otherwise dates mess it up:

#Convert text to UTF-8
#d$doc <- pbsapply(d$doc, iconv, "latin1", "UTF-8")
#d$doc <- pbsapply(d$doc, iconv, "cp1252", "UTF-8")
#For some BIZARRE reason, this works too sometimes:
#d$doc <- pbsapply(d$doc, iconv, "UTF-8", "UTF-8")

#This is the only one that works here in the sense that it doesnt cause problems later:
d$doc <- pbsapply(d$doc, iconv, "latin1", "UTF-8")

#remove date-related words
date_words <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
date_words <- c(date_words, "January","February","March","April","May","June","July","August","September","October","November","December")
date_words <- c(date_words, "Mon","Tue","Wed","Thu","Fri","Sat","Sun")
date_words <- c(date_words, "am", "pm")
date_words <- c(date_words, tolower(date_words))
d$doc <- pbsapply(d$doc, removeWords, words = date_words)

#Remove words containing underscores
d$doc <- pbsapply(d$doc, str_replace_all, "\\w*_\\w*", "")

#Remove punctuation
d$doc <- pbsapply(d$doc, removePunctuation)

#Remove numbers
d$doc <- pbsapply(d$doc, removeNumbers)


### REMOVING DUPLICATE LINES:

#remove cities with only 1 document (since that breaks the whole hashtable thing)
d <- d[!d$City %in% names(table(d$City)[table(d$City)<2]),]

#save
save(d, file = "rfiles/dLA2.rdata")
load("rfiles/dLA2.rdata")

#create a table of the number of documents for each city
citytable <- table(d$City)
citynames <- d$City

#iterate over cities, creating a list for each
#each of these lists contains one numerical vector for each document
#each vector contains one element for each line in the document
#each of these elements denominates the number of times that line occurs within the city
proc_city <- function(j){
  
  #what's the name of the current city
  cityname <- names(citytable)[j]
  #what is the index of the city in the list of cities
  citytableindex <- as.numeric(which(names(citytable)==cityname))
  #determine which row the city starts on
  ifelse(citytableindex>1, citystart <- sum(citytable[1:(citytableindex-1)])+1, citystart <- 1)
  #how many documents does the current city have?
  citylength <- as.numeric(citytable[citytableindex])
  #which indices belong to this city?
  cityindices <- citystart:(citystart+citylength-1)
  
  #---
  #up until here, we basically just get the indices for the city within the data frame
  #the next part is the hashmap
  
  #concatenate documents of the current city
  lines <- unlist(d$doc[cityindices])
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
    return(HH$find(d$doc[cityindices][[k]]))
  }
  
  #do this for all the documents, creating a list of vectors
  #one vector for each document
  #each of which contains the number of times a line in that document is present in the city
  docDuplicatesHH <- lapply(1:nrow(d[cityindices,]), duplicateLinesHH)
  
  return(docDuplicatesHH)
  
}

#iterate over all cities; parallelize
#this creates a list of lists, one for each city
library('parallel')
cl <- makeForkCluster(11) #detectCores()-1
docDuplicates <- pbsapply(1:length(citytable), proc_city, cl = cl)
stopCluster(cl)

#unlist only the outer list
docDuplicates <- unlist(docDuplicates, recursive = F)


#This takes about half a day (without parallelization), so back up the results
save(docDuplicates, file = "rfiles/docDuplicatesLAHash.Rdata")
load("rfiles/docDuplicatesLAHash.Rdata")

#function to decide whether a line in a document is kept
#currently, this happens if it occurs no more than 10 times in other city docs
#also concatenates the documents into character vectors (since they were lists of lines so far)
cleanup <- function(i){
  keep <- which(docDuplicates[[i]]<=10)
  cleanedDoc <- str_c(d$doc[[i]][keep], collapse = " ")
  return(cleanedDoc)
}
#sapply said function (with a progress bar) on the whole corpus
d$doc <- as.character(pbsapply(1:nrow(d), cleanup))


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
#d <- d[d$filename!="indypotholeviewer.txt",]

#Hunspell
source("hunspellParallel.R")

#Everything to lowercase
d$doc <- tolower(d$doc)

#Remove cities with only one document
d <- d[d$City%in%names(table(d$City))[table(d$City)>1],]

#remove documents with nonsensical texts
d <- d[d$doc!="character",]
d <- d[!nchar(d$doc)<50,]

#save
#save(d, file = "rfiles/dLA3.rdata")
#load("rfiles/dLA3.rdata")

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

#save
#save(d, file = "rfiles/dLA4.rdata")
#load("rfiles/dLA4.rdata")

#merge in original file extension
#d <- merge(d, d2, "path", all.x = T, all.y = F)

#remove a few empty documents that ended up in there at some point
d <- d[is.na(d$ext)==F,]

#save
#save(d, file = "rfiles/dLA5.rdata")
#load("rfiles/dLA5.rdata")

#remove city names
citynames <- unique(d$City)
citynames <- c(citynames, tolower(citynames))
d$doc <- removeWords(d$doc, citynames)

#removing token types that disproportionately occur in one city's corpus
#source('malletPreprocessingCityWords.R')

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
save(d, file = "rfiles/dLA.rdata")
load("rfiles/dLA.rdata")

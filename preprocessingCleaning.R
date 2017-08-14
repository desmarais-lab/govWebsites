library('tibble')
library('stringr')
library('tools')
library('pbapply')
library('tm')
library('dplyr')
library('quanteda')
library('hunspell')

corpus <- "current" #"current", "before", or "after"
filepath <- str_c("./websites/", corpus)

## Reading in the data

f <- list.files(path = filepath, recursive = T) #create a list of all files in all subdirectories

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
d$doc <- sapply(d$path, function(x){list(readLines(x))})

#extract city from directory
d$city <- str_split_fixed(d$path, str_c(filepath, "/"), 2)[,2]
d$city <- str_split_fixed(d$city, "/", 2)[,1]

#import and merge in city coeffs
load("./data/URLs_IN.rdata")
URLs$foldername[URLs$foldername=="www.batesvilleindiana.us"] <- "batesvilleindiana.us"
URLs$foldername[URLs$foldername=="www.bloomington.in.gov"] <- "bloomington.in.gov"
URLs$foldername[URLs$foldername=="www.brazil.in.gov"] <- "brazil.in.gov"
URLs$foldername[URLs$foldername=="www.elwoodcity-in.org"] <- "elwoodcity-in.org"
d <- merge(d, URLs, by.x = "city", by.y = "foldername")

### INTERESTING STUFF AFTER THIS

citytable <- sapply(unique(d$Name), function(x){length(d$doc[d$Name==x])})
citynames <- d$Name

#create a list to store the duplicated row indicies for each document in
#ever list element is a numeric vector, and each element of this vector indicates
## how many times this line in the document occurs in other documents of this city
docDuplicates <- list()

# --- I didn't actually paralellize this, but it is written in a manner that is
# ----- embarassingly parallel, so it would be easy to implement if required

for(k in 1:nrow(d)){ #documents loop begins here ##### 
  
  #what's the name of the current city
  cityname <- citynames[k]
  #what is the index of the city in the list of cities
  citytableindex <- as.numeric(which(names(citytable)==cityname))
  #determine which row the city starts on
  ifelse(citytableindex>1, citystart <- sum(citytable[1:(citytableindex-1)])+1, citystart <- 1)
  #how many documents does the current city have?
  citylength <- as.numeric(citytable[citytableindex])
  #which indices belong to this city?
  cityindices <- citystart:citylength
  
  #for each document, create the vector indicating the number of times a line occurs elsewhere
  b <- numeric(length = length(d$doc[[k]]))
  #loop over all other documents
  for(i in cityindices[-k]){
    #check against one other document
    a <- match(d$doc[[k]], d$doc[[i]])
    #if there is a match, advance duplicate counter by one
    b[is.na(a)==F] <- b[is.na(a)==F]+1
  }
  #assign duplicates vector to list
  docDuplicates[[k]] <- b
  #output progress
  cat(k, "\n")

}

#This takes about half a day, so back up the results
save(docDuplicates, file = "rfiles/docDuplicates.Rdata")
load("rfiles/docDuplicates.Rdata")

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

#Remove words containing underscores
d$doc <- str_replace_all(d$doc, "\\w*_\\w*", "")

#Remove bullet points
d$doc <- str_replace_all(d$doc, "  o ", " ")

#Remove punctuation
d$doc <- removePunctuation(d$doc)

#Remove numbers
d$doc <- removeNumbers(d$doc)

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
save(d, file = "./rfiles/d_noduplicates.Rdata")

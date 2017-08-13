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

#determine which row to start on
#ifelse(k>1, citystart <- sum(citytable[1:(k-1)])+1, citystart <- 1)
#how many documents does the current city have?
#citylength <- as.numeric(citytable[k])
#which indices belong to this city?
#cityindices <- citystart:citylength


#j = 2

#b <- numeric(length = length(a))
#i = 8
#a <- match(d$doc[[2]], d$doc[[i]])
#b[is.na(a)==F] <- b[is.na(a)==F]+1
#b

#a <- ifelse(is.na(a)==T, a <- 1, a <- 0)
#b <- a+b
#b

#b <- a


############

docDuplicates <- list()

for(k in 1:nrow(d)){ #documents loop begins here ##### 
  
  cityname <- citynames[k]
  citytableindex <- as.numeric(which(names(citytable)==cityname))
  #determine which row the city starts on
  ifelse(citytableindex>1, citystart <- sum(citytable[1:(citytableindex-1)])+1, citystart <- 1)
  #how many documents does the current city have?
  citylength <- as.numeric(citytable[citytableindex])
  #which indices belong to this city?
  cityindices <- citystart:citylength

  b <- numeric(length = length(d$doc[[k]]))
  for(i in cityindices[-k]){
    a <- match(d$doc[[k]], d$doc[[i]])
    b[is.na(a)==F] <- b[is.na(a)==F]+1
  }
  
  docDuplicates[[k]] <- b
  
  cat(k, "\n")

}

docDuplicates

save(docDuplicates, file = "rfiles/docDuplicates.Rdata")


cleanup <- function(i){
  keep <- which(docDuplicates[[i]]<=10)
  cleanedDoc <- str_c(d$doc[[i]][keep], collapse = " ")
  return(cleanedDoc)
}

d$doc_cleaned <- pbsapply(1:nrow(d), cleanup)
d$doc_cleaned <- unlist(d$doc_cleaned)

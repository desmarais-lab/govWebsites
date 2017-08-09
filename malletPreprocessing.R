library('tibble')
library('stringr')
library('tools')
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

#read in file text
d$doc <- sapply(d$path, function(x){str_c(readLines(x), sep = " ", collapse = " ")})
d$doc <- as.character(d$doc)

## Pre-processing

#Remove xml files
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

#Everything to lowercase
#d$doc <- tolower(d$doc)

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

#save, for use with hunspell
save(d, file = "./rfiles/dd.Rdata")

#Hunspell
source("hunspellParallel.R")

#reload the data after spellchecking
load("rfiles/dd_spellchecked.Rdata")

#Everything to lowercase
d$doc <- tolower(d$doc)

#Remove cities with only one document
d <- d[d$Name%in%names(table(d$Name))[table(d$Name)>1],]

#remove non-spellchecked text and some other stuff
save(d, file = "./rfiles/d.Rdata")

#Stemming
#d$doc2 <- stemDocument(d$doc)

#stopwords list from the tm package
stopwrds <- stopwords("english")
#additional words for the tm package
stopwrds <- append(stopwrds, c("will","e"))

#write  to a text file
fileConn <- file("./rfiles/stopwords.txt")
writeLines(stopwrds, fileConn)
close(fileConn)

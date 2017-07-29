#This option has to be set BEFORE loading the rJava package (loaded by mallet)
#Gives rJava about 4GB to work with
#So far, I've only needed 2.3GB

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
d$doc <- tolower(d$doc)

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


#################
#setwd("/home/markus/govWebsites/websites2/websites_backup/html/")
#a <- scan("index", what = "raw")
#a <- readLines("index")


save(d, file = "./rfiles/dd.Rdata")
load(file = "./rfiles/dd.Rdata")

#Hunspell
#source("hunspellParallel.R")
load(file = str_c("./rfiles/docs_", corpus, ".Rdata"))

#remove documents where spellchecking failed
d <- d[d$spell_fail==0,]

#remove non-spellchecked text
d$doc <- d$doc2
d <- select(d, -doc2)

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

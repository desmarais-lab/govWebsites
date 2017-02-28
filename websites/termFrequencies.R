#This script extracts the top term frequencies from the pdfs in the 10 test websites
#Script is pretty much entirely based on:
#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/pdfsfolder/")

require(tm)
docs <- Corpus(DirSource(pattern = ".*.txt"))

#Preprocessing
docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #remove numbers
docs <- tm_map(docs, tolower) #converting to lowercase
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords

save(docs, file="docs.rdata")
load(file="docs.rdata")

#library(SnowballC)
#docs <- tm_map(docs, stemDocument) #stemming

#create document term matrix
dtm <- DocumentTermMatrix(docs)
#create term document matrix
#tdm <- TermDocumentMatrix(docs)   

#top 30 term frequencies
sort(apply(dtm,2,sum),decreasing = T)[1:30]

library('readtext')
library('doParallel')
library("stringr")
library("data.table")
library("ff")
set.seed(1)

#function to convert every pdf, html, doc, or docx in a folder to text
# and copy to a different folder
# this is basically a wrapper for readtext::readtext
# with two additions: (1) error handling, and (2) parallelization
# readtext will do multiple files, but can't handle errors
# so this does one file at a time

convertToText <- function(paths, id){
  
  #register 11 parallel threads
  registerDoParallel(cores=11)
  #parallel loop over the documents in the current website
  a <- foreach(j=(1:length(paths))) %dopar% {
    
    #error handling, in case readtext can't handle a specific document
    try({
      #readtext
      b <- readtext(paths[j])
      #record the file id
      b$id <- id[j]

      return(b)
      
    })

  }
  
  #kick the data frames out of the list that don't have three columns
  broken <- -which(unlist(lapply(a, length))!=3)
  if(length(broken)!=0){
    a <- a[broken]
  }
  #combine the list of data frames from the foreach loop into one big data frame
  a <- rbindlist(a)
  a <- subset(a, select = -doc_id)
  
  return(a)
  
}

load("out/citydocs.rdata")

d <- d[d$ext!="html",]

#----
# Large files

#As an example, the largest file is in:
#/home/mneumann/hd2/govWebsites/www.cityofchesapeake.net/Assets/documents/departments/city_attorney/battlefield_golf_club/1881-1883-UP-01-03-Etheridge-Greens.pdf
#This file is over 600Mb and contains schematics for a golf course

#Flag files that are too large (>10Mb)
too_big <- d$filesize>1e+7
print(sum(d$filesize[which(too_big)])) #combined size of the file we kick out
print(sum(d$filesize[which(!too_big)])) #combined size of the file we keep
#remove them
d <- d[!too_big,]

#sort by size
#the advantage of this should be that we don't have 10 cores sitting idle while one works on a huge file
d <- d[order(d$filesize),]

#remove robots.txt files
d <- d[d$filename!="robots.txt",]

#use ff to make 5k row chunks of the dataframe
#iterate over them and read in and convert to text all of its documents
#using the function above
#the downside to this approach is that it often seems to get stuck on one thing while all the other cores are already done
for(i in chunk(from = 1, to = nrow(d), by = 5000)){
  d_chunk <- d[min(i):max(i), ]
  text_chunk <- convertToText(d_chunk$path, d_chunk$id)
  save(text_chunk, file = paste0("out/non_html_chunks/chunk_", min(i), "_", max(i), ".rdata"))
}

#combine the chunks into one file
f <- list.files("out/non_html_chunks/", full.names = T)
files <- list()
for(i in 1:length(f)){
  load(f[i])
  files[[i]] <- text_chunk
}
results_nonhtml <- rbindlist(files)
save(results_nonhtml, file = "out/results_readtext.rdata")

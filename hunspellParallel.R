library('hunspell')
library('tm')
library('parallel')
library('quanteda')
library('stringr')



#setwd("./websites/current")
#load(file = "./rfiles/dd.Rdata")

#function to apply spellchecking
#on each text, apply hunspell, and save the character vector of incorrect words
hunRemove <- function(charstring){
  charstring_split <- strsplit(charstring, " ")[[1]]
  errors <- unlist(hunspell(charstring_split))
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

#for testing purposes, only spellcheck some of the documents
#d <- d[1:10000,]

# Calculate the number of cores
ncores <- detectCores() - 1
# Initiate cluster
# type="FORK" is necessary to load all the libraries
# supposedly only works on Unix systems
cl <- makeCluster(ncores, type = "FORK")

#check duration of running the spellchecking
#ptm <- proc.time()
#then use parSapply to use it on everything in parallel
d$doc <- parSapply(cl, d$doc, FUN = hunRemove)
#proc.time() - ptm
#On 6 cores (i.e. 12 threads, of which I used 11)
#it ended up taking 95 seconds
#previously it took about 24 hours

# shut down the workers
stopCluster(cl)

#save
#save(d, file = "rfiles/dd_spellchecked.Rdata")
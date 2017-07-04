library('hunspell')
library('tm')
library('parallel')
#library('profvis')

# Calculate the number of cores
ncores <- detectCores() - 1
# Initiate cluster
# type="FORK" is necessary to load all the libraries
# supposedly only works on Unix systems
cl <- makeCluster(ncores, type = "FORK")

#setwd("./websites/current")
load(file = "./rfiles/dd.Rdata")


#function to apply spellchecking
#on each text, apply hunspell, and save the character vector of incorrect words
#since grep has a limit for how many words it can process,
#only use removeWords (from the tm package) if the number of errors is less than 1000
#generally this only happens if a document is completely messed up, in which case
#it is probably best to discard the whole thing
#in this case, just insert ERROR instead of the document
hunRemove <- function(charstring){
  errors <- unlist(hunspell(charstring))
  if(length(errors)<1000){
    output <- removeWords(charstring, errors)
    return(output)
  }
  else{
    return("ERROR")
  }
}

#for testing purposes, only spellcheck some of the documents
#d <- d[1:200,]

#check duration of running the spellchecking
ptm <- proc.time()
#then use parSapply to use it on everything in parallel
d$doc2 <- parSapply(cl, d$doc, FUN = hunRemove)
proc.time() - ptm
#On 6 cores (i.e. 12 threads, of which I used 11)
#it ended up taking 14 hours

# shut down the workers
stopCluster(cl)

#record when it did encounter more than 1000 errors
d$spell_fail <- 0
d$spell_fail[d$doc2=="ERROR"] <- 1

#save
save(d, file = "../../docs.Rdata")

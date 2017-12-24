library(quanteda)

#only for debugging:
#load("/home/markus/govWebsites/rfiles/d.Rdata")
#d <- d[1:500,]

# Remove terms that only occur in [cutoff] documents

dm <- corpus(d$doc)
dm <- dfm(dm) 
dm <- slam::as.simple_triplet_matrix(dm)

#binarize dtm
dm$v <- ifelse(dm$v>0, 1, 0)
#sum over documents -- i.e. in how many documents does a term occur
dmColSums <- apply(dm, 2, sum)
#choose the cutoff
cutoff <- 1

occuranceRemove <- function(charstring){
  charstring_split <- strsplit(charstring, " ")[[1]]
  # terms that occur in equal or less than the cutoff value documents
  errors <- names(dmColSums[dmColSums<=cutoff])
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

#apply to every document (parallelized)
ncores <- detectCores() - 1
cl <- makeCluster(ncores, type = "FORK")
d$doc <- parSapply(cl, d$doc, FUN = occuranceRemove)
stopCluster(cl)

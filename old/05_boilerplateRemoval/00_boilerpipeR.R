library(boilerpipeR)
data(content)
extract <- DefaultExtractor("https://www.cityofdenhamsprings.com/mayor-landry.html")

library(boilerpipeR)
library(RCurl)
url <- "https://www.cityofdenhamsprings.com/mayor-landry.html"
content <- getURL(url)
extract1 <- DefaultExtractor(content)
extract2 <- ArticleExtractor(content)
cat(substr(extract, 1, 120))

#----
load("../04_parseText/out/citydocs.rdata")
d_html <- d[d$ext=="html",]
content <- rawHTML <- paste(readLines(d_html$path[1]), collapse="\n")
extract <- DefaultExtractor(content)
extract2 <- ArticleExtractor(content)

extracts <- list()
for(i in 1:1000){
  content <- paste(readLines(d_html$path[i]), collapse="\n")
  extracts[[i]] <- ArticleExtractor(content)
  print(i)
}

extractArticle <- function(filepath){
  content <- paste(readLines(filepath), collapse="\n")
  content <- ArticleExtractor(content)
  return(content)
}


library('doParallel')
#register 11 parallel threads
registerDoParallel(cores=11)
#parallel loop over the documents in the current website
a <- foreach(j=(1:100)) %dopar% {
  #error handling, in case readtext can't handle a specific document
  try({
    #readtext
    b <- extractArticle(d_html$path[j])
    return(b)
  })
}

library(parallel)
ncores <- detectCores() - 1
library(pbapply)
cl <- makeCluster(ncores, type = "FORK")
answerComparisons <- pblapply(d_html$path[1:1000], extractArticle, cl = cl)
stopCluster(cl)

test = pblapply(d_html$path[1:10000], extractArticle)



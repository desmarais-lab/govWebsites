options(java.parameters="-Xmx12g")
library(boilerpipeR)
library(stringr)
set.seed(1)

load("out/citydocs.rdata")
d <- d[d$ext=="html",]

#Function to extract the article text with boilerpipeR
#Can fail if there is something wrong with the HTML, so wrapped in try()
extractArticle <- function(filepath){
  try({
  content <- paste(readLines(filepath, warn = F), collapse="\n")
  content <- KeepEverythingExtractor(content)
  return(content)
  })
}

#Apply the function
#No parallelization, seems to work better without
extracts <- list()
for(i in 1:nrow(d)){
  extracts[[i]] <- extractArticle(d$path[i])
  #Do some occasional garbage collection to ensure that nothing breaks
  if(i%%1000 == 0) {
    gc()
    print(i)
  }
}
#put the results, along with ids, into a file
extracts <- unlist(extracts)
ids <- d$id
results_html <- data.frame(text = extracts, id = ids, stringsAsFactors = F)

#remove documents with embedded javascript, json, other html things, etc. 
html_docs <- which(str_detect(results_html$text, "/*! jQuery"))
html_docs <- c(html_docs, which(str_detect(results_html$text, ".className")))
html_docs <- c(html_docs, which(str_detect(results_html$text, "\\{\\\""))) #json
html_docs <- c(html_docs, which(str_detect(results_html$text, "wp-embedded-content")))
html_docs <- c(html_docs, which(str_detect(results_html$text, "blockquote")))
html_docs <- unique(html_docs)
if(length(html_docs)>0){
  results_html <- results_html[-html_docs,]
}
rm(html_docs)


#save the results
save(results_html, file = "out/results_boilerpipe_KeepEverything.rdata")
#save.image("out/boilerpipe_img.rdata")

#----
# Somewhat unsuccessful attempts at parallelization

# library('doParallel')
# #register 11 parallel threads
# registerDoParallel(cores=11)
# #parallel loop over the documents in the current website
# a <- foreach(j=(1:100)) %dopar% {
#   #error handling, in case readtext can't handle a specific document
#   try({
#     #readtext
#     b <- extractArticle(d_html$path[j])
#     return(b)
#   })
# }
# 
# library(parallel)
# ncores <- detectCores() - 1
# library(pbapply)
# cl <- makeCluster(ncores, type = "FORK")
# answerComparisons <- pblapply(d_html$path[1:1000], extractArticle, cl = cl)
# stopCluster(cl)
# 
# test = pblapply(d_html$path[1:10000], extractArticle)

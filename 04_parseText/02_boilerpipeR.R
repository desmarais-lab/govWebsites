options(java.parameters="-Xmx12g")
library(boilerpipeR)

load("out/citydocs.rdata")
d <- d[d$ext=="html",]

extractArticle <- function(filepath){
  try({
  content <- paste(readLines(filepath, warn = F), collapse="\n")
  content <- ArticleExtractor(content)
  return(content)
  })
}

extracts <- list()
for(i in 1:nrow(d)){
  extracts[[i]] <- extractArticle(d$path[i])
  print(i)
  if(i%%1000 == 0) {
    gc()
  }
}
save.image("out/boilerpipe_img.rdata")

#----

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



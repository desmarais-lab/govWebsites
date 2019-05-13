library(quanteda)
library(spacyr)
#spacy_initialize()

load("../04_parseText/out/boilerpipe_img.rdata")

extracts <- unlist(extracts)
#parse all html documents for attica
crps <- corpus(extracts[which(d$city==unique(d$city)[1])])
docvars(crps) <- d[which(d$city==unique(d$city)[1]),]
parsedtxt <- spacy_parse(crps, tag = T, dependency = T)

#make a dataframe which shows off the words per POS tag, sorted by count
#put everything in a list
testt <- list()
test = aggregate(parsedtxt$token, by = list(parsedtxt$pos), FUN = paste, collapse = " ")
test <- test[!test$Group.1=="SPACE",]
for(i in 1:nrow(test)){
  testt[[i]] <- data.frame(sort(table(unlist(tokens(test$x[i]))), decreasing = T))
  testt[[i]]$a <- as.character(test$Group.1[i])
}
#rbindlist
ee <- data.table::rbindlist(testt)
#cbind, filling empty rows with na
ff <- cbind.fill(ee[ee$a==unique(ee$a)[1]], ee[ee$a==unique(ee$a)[2]], fill = NA)
for(i in 3:(length(unique(ee$a)))){
  ff <- cbind.fill(ff, ee[ee$a==unique(ee$a)[i]], fill = NA)
  
}

#make it prettier and print to html
library(stringr)
library(xtable)
names(ff) <- sort(c(unique(ee$a), paste(unique(ee$a), "count", sep = "_"), paste(unique(ee$a), "name", sep = "_")))
gg = ff[,!str_detect(colnames(ff), "name")]
print(xtable(gg), type = "html", file = "out/posTaggingTest.html")
print(xtable(parsedtxt[parsedtxt$doc_id%in%paste0("text", 1:10),]), type = "html", file = "out/taggingTest.html")

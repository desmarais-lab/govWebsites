setwd("govWebsites")

load("rfiles/d.Rdata")

library("dplyr")

table(d$Name)

d <- d[d$Name%in%c("Attica","Auburn","Batesville"),]

library('quanteda')

#toks <- tokens(crps)
#trigrs <- tokens_ngrams(toks, n = 3)

crps <- corpus(d$doc)
summary(crps)
docvars(crps, "Party") <- d$winner

ndoc(crps)

#summarize corpus
#object is a "tidy" dataframe and can be used for other stuff
crps_sum <- summary(crps)

dfm_in <- dfm(crps)

dfm_in[1:5,1:5]

#get most frequent features (i.e. tokens)
topfeatures(dfm_in)

#corpus to dfm, tokenize as trigrams
dfm_in3 <- dfm(crps, ngrams = 3)

dfm_in3[1000:1005,1:5]

dim(dfm_in3)

tfidf_in3 <- tfidf(dfm_in3)

tfidf_in3[901:905,1:5]

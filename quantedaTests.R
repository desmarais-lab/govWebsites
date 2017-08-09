library('quanteda')
library('dplyr')

load(file = "./rfiles/d.Rdata")

# create a corpus from the immigration texts from UK party platforms
indianaCorpus <- corpus(d$doc[1:400])#,
#                        docvars = select(d, -doc))

indianaDfm <- dfm(indianaCorpus, remove = readLines("rfiles/stopwords.txt"), remove_punct = TRUE)
indianaDfm

indianaDfm_3 <- dfm(indianaCorpus, ngrams = 3, remove = readLines("rfiles/stopwords.txt"), remove_punct = TRUE)
indianaDfm_3


b <- tfidf(indianaDfm_3)

b[1:5,1:5]
head(b)[1:5,1:5]

sum(b[,1])

termfreq <- sum(indianaDfm[,1])
idf <- log(nrow(indianaDfm)/termfreq)

tfidf1 <- termfreq*idf

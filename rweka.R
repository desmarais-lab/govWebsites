options(java.parameters = "-Xmx5000m")
library("RWeka")
library("tm")

load(file = "./rfiles/d.Rdata")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(Corpus(VectorSource(d$doc)), control = list(tokenize = BigramTokenizer))

inspect(tdm[340:345,1:10])

data("crude")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))

inspect(tdm[340:345,1:10])
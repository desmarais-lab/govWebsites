#https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

library("ldatuning")
library("topicmodels")
library('quanteda')

#load data
load(file = "./rfiles/d.Rdata")

#get the document-term matrix
crps <- corpus(d$doc)
dtm <- dfm(crps)

dtm2 <- dtm[1:1000,]

result <- FindTopicsNumber(
  dtm2,
  topics = seq(from = 10, to = 100, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 11,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

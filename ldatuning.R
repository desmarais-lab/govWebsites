#Code is basically from:
#https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

##
## NOTE: Running this script as it is takes about 50 hours on 11 cores
##

library("ldatuning")
library('quanteda')

#load data
load(file = "./rfiles/d.Rdata")

#get the document-term matrix
crps <- corpus(d$doc)
dtm <- dfm(crps)

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 25, to = 500, by = 25),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 11,
  verbose = TRUE
)

pdf("paper/figures/topicnumber_ldatuning.pdf", width = 9, height = 8, onefile = F)
FindTopicsNumber_plot(result)
dev.off()

#save.image("rfiles/topicmumber_ldatuning2.Rdata")

save(result, file = "rfiles/ldatuning_results.Rdata")

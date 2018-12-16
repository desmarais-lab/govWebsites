library('quanteda')
library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')

set.seed(1)

load(file = "./rfiles/websiteMetadata.rdata")
load(file = "rfiles/allDocuments.rdata")

d <- merge(d, websiteMeta, by.x = "city", by.y = "State_City")
d <- subset(d, State %in% c("Indiana", "Louisiana", "New York", "California", "Washington", "Texas"))

d$doc_id <- paste("doc", 1:nrow(d))
crps <- corpus(d)
dtm <- dfm(crps)

#Number of topics
numtopics <- 60

#Train the model
stmFit <- stm(documents = dtm,
              K = numtopics, 
              prevalence =~ party + State + B01001_001E + B19013_001E,
              max.em.its = 9999,
              init.type = "Spectral")

#save.image("rfiles/stmSession2, _Party.rdata")
save.image(paste("rfiles/stmSession_model_", numtopics, ".rdata", sep = ""))

#set seed again in case the script was restarted from here
set.seed(1)

#Estimate effects from the model outputs
sims <- 1000

#re-estimate the effects, this time do 1000 draws from the posterior
prep <- estimateEffect(formula = 1:numtopics ~ party + State + B01001_001E + B19013_001E, 
                       stmobj = stmFit,
                       meta = d, 
                       uncertainty = "Global",
                       nsims = sims)

save.image(paste("rfiles/stmSession_sim_", numtopics, ".rdata", sep = ""))

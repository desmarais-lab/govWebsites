library('quanteda')
library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')
set.seed(1)

# load(file = "../06_preprocessing/out/preprocessing_finished.rdata")
# #merge in census covariates
# load("../04_extractText/out/citydocs.rdata")
# # load("../00_scrapeCovariates/out/websiteMetadata_Census.rdata")
# # websiteMeta <- subset(websiteMeta, select = c(State_City, B01001_001E, B19013_001E))
# # d <- merge(d, websiteMeta, by = "State_City")
# d_reordered <- d[match(names(tks_all), d$id),]
# docvars(tks_all) <- d_reordered
# rm(websiteMeta, d, d_reordered)
# #use only the states for which we have data on multiple cities
# tks_all <- tokens_subset(tks_all, State %in% c("Indiana", "Louisiana", "New York", "California", "Washington", "Texas"))
# #convert to dfm and then stm
# d_dfm <- dfm(tks_all)
# d_stm <- convert(d_dfm, to = "stm")
# save(d_stm, file = "out/ready_for_stm.rdata")
load("../06_preprocessing/out/stm_corpus_KeepEverything.rdata")

#Number of topics
numtopics <- 60

#Train the model
stmFit <- stm(documents = d_stm$documents,
              vocab = d_stm$vocab,
              data = d_stm$meta,
              K = numtopics, 
              prevalence =~ party + State + B01001_001E + B19013_001E,
              max.em.its = 9999,
              init.type = "Spectral")

#save.image("rfiles/stmSession2, _Party.rdata")
save.image(paste("out/stmSession_model_", numtopics, "_KeepEverything.rdata", sep = ""))

#set seed again in case the script was restarted from here
set.seed(1)

#Estimate effects from the model outputs
sims <- 1000

#re-estimate the effects, this time do 1000 draws from the posterior
prep <- estimateEffect(formula = 1:numtopics ~ party + State + B01001_001E + B19013_001E, 
                       stmobj = stmFit,
                       metadata = d_stm$meta, 
                       uncertainty = "Global",
                       nsims = sims)

save.image(paste("out/stmSession_sim_", numtopics, "_KeepEverything.rdata", sep = ""))

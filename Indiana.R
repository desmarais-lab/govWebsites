#setwd("govWebsites")

state <- "Indiana"
stateAbb <- "IN"

# ------------- #
# Preprocessing #
# ------------- #

source("./functions/preprocessing.R")
# Read in the data from txt files
d <- readCityDocuments("./websites/IN")
# merge city coefficients (i.e. election data) with the documents
d <- mergeCityCoefficients(d, "rfiles/city_coefficients_indiana.rds")
# Do all the preprocessing that needs to be done before removing duplicate lines
d <- preprocessing_1(d)
# Find duplicate lines
docDuplicates <- findDuplicates(d)
save(docDuplicates, file = paste0("rfiles/docDuplicates", stateAbb, ".rdata"))
# Use the counts of duplicate lines found above to remove them over a certain threshold
d$doc <- as.character(pbsapply(1:nrow(d), removeDuplicates))

#save.image("rfiles/backup_d.rdata")

# Do the rest of the preprocessing
d <- preprocessing_2(d)

save(d, file = paste0("rfiles/d_", stateAbb, ".rdata"))
#file.copy(paste0("rfiles/d_", stateAbb, ".rdata"), paste0("rfiles/d_", stateAbb, "_backup.rdata"))
#load(paste0("rfiles/d_", stateAbb, ".rdata"))

#lemmatization
#Note: due to memory constraints, it may be necessary to close R here, then run
# python3 lemmatization.py rfiles/d_IN.rdata
#from the console instead, open R again and continue on the next line (i.e. load)
#but don't forget to set the following againa after restarting R
#state <- "Indiana"
#stateAbb <- "LA"
system(paste0("python3 lemmatization.py ", paste0("rfiles/d_", stateAbb, ".rdata")))
load(paste0("rfiles/d_", stateAbb, ".rdata"))
#spacy puts -PRON- in place of pronouns since they can't be lemmatized
#we don't really care about pronouns, so we just remove them
d$doc <- str_replace_all(d$doc, "-PRON- ", "")

#Hunspell and removal of terms occuring in only one document
#these were originally in preprocessing.R, but were not working correctly
#functionally there is no nownside to them being here, it just doesn't look as nice
source("hunspellParallel.R")
source('functions/occuranceRemove.R')

# merge in city population
source("mergeDocsWithCensusData.R")
names(d)[names(d)==paste0("POPESTIMATE", unique(d$Year))] <- "POPESTIMATE"

#save the finished data frame
save(d, file = paste0("rfiles/d_", stateAbb, ".rdata"))

# ------------- #
#      LDA      #
# ------------- #

# Train LDA
#source("functions/malletTraining.R")

# 6 (mostly diagnostic) figures:
#Densities of topic weights for documents in Republican and Democratic cities.
#\label{topicweights_density}
# Densities of topic weights for documents in Republican and Democratic cities.
#\label{doctopics_density}
#Word-topic probabilities for topics with big partisan differences
#\label{diffsize}
#Word-topic probabilities for topics with big partisan differences
#\label{plotWTP_bigdiffs}
#Word-topic probabilities for topics with big partisan differences
#\label{doctopics_density_bigdiffs}
#Word-topic probabilities for topics with big partisan differences, across documents
#\label{heatmaps_weights}
#source("malletAnalysis.R")

#source("malletAnalysisPartisanTopicsLA.R")
#source("malletAnalysisPartisanTopWordsLA.R")
#source("descriptiveStatisticsPartisan.R")

# -----------------------#
# Structural Topic Model #
# -----------------------#

source("stm.R")

# ------------- #
# Fightin Words #
# ------------- #

source("fightinWords.R")

# ------------- #
#  Clustering   #
# ------------- #

#source("hierarchicalClustering.R")

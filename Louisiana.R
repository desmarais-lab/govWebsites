setwd("govWebsites")

state <- "Louisiana"
stateAbb <- "LA"

# ------------- #
# Preprocessing #
# ------------- #

source("./functions/preprocessing.R")
# Read in the data from txt files
d <- readCityDocuments("./websites/LA")
# merge city coefficients (i.e. election data) with the documents
d <- mergeCityCoefficients(d, "rfiles/city_coefficients_louisiana.rds")
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
system(paste0("python3 lemmatization.py ", paste0("rfiles/d_", stateAbb, ".rdata")))
load(paste0("rfiles/d_", stateAbb, ".rdata"))

# ------------- #
#      LDA      #
# ------------- #

# Train LDA
source("functions/malletTraining.R")

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
source("malletAnalysis.R")

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

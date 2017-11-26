setwd("govWebsites")

# ------------- #
# Preprocessing #
# ------------- #

source("./functions/preprocessing.R")
# Read in the data from txt files
d <- readCityDocuments("./websites/current")
# merge city coefficients (i.e. election data) with the documents
d <- mergeCityCoefficients(d, "rfiles/city_coefficients_indiana.rds")
# Do all the preprocessing that needs to be done before removing duplicate lines
d <- preprocessing_1(d)
# Find duplicate lines
docDuplicates <- findDuplicates(d)
# Use the counts of duplicate lines found above to remove them over a certain threshold
d$doc <- as.character(pbsapply(1:nrow(d), removeDuplicates))

save.image("rfiles/backup_d.rdata")

# Do the rest of the preprocessing
d <- preprocessing_2(d)

# ------------- #
#      LDA      #
# ------------- #

source("malletTraining.R")

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

library("stringr")
library("tools")
library("tibble")
library('pbapply')

# Read in the data from txt files
d <- readCityDocuments("websites/mayorsTXT", "mayor_site")

# Do all the preprocessing that needs to be done before removing duplicate lines
d <- preprocessing_1(d, "mayor_site")

# Find duplicate lines
docDuplicates <- findDuplicates(d, "mayor_site", nthreads = 6)

# Use the counts of duplicate lines found above to remove them over a certain threshold
d$doc <- as.character(pbsapply(1:nrow(d), removeDuplicates))

# Do the rest of the preprocessing
d <- preprocessing_2(d, remove.city.names = T)

d <- as.data.frame(d)
save(d, file = "rfiles/d_mayors.rdata")

#lemmatization
#Note: due to memory constraints, it may be necessary to close R here, then run
# python3 lemmatization.py rfiles/d_IN.rdata
#from the console instead, open R again and continue on the next line (i.e. load)
#but don't forget to set the following againa after restarting R
#state <- "Indiana"
#stateAbb <- "LA"
system(paste0("python3 lemmatization.py ", "rfiles/d_mayors.rdata"))
load("rfiles/d_mayors.rdata")
#spacy puts -PRON- in place of pronouns since they can't be lemmatized
#we don't really care about pronouns, so we just remove them
d$doc <- str_replace_all(d$doc, "-PRON- ", "")

#Hunspell and removal of terms occuring in only one document
#these were originally in preprocessing.R, but were not working correctly
#functionally there is no nownside to them being here, it just doesn't look as nice
source("hunspellParallel.R")
source('functions/occuranceRemove.R')

# merge in partisanship
load("./rfiles/mayors.rdata")
mayors$website <- str_replace(mayors$website, "https?://", "")
mayors$website <- str_replace(mayors$website, "/", "")
d <- merge(d, mayors, by.x = "mayor_site", by.y = "website")

#save the finished data frame
save(d, file = "rfiles/d_mayors.rdata")



# Analysis
load("rfiles/d_mayors.rdata")

dem.groundtruth <- paste0(d$doc[d$Party=="Democratic"], collapse = " ")
rep.groundtruth <- paste0(d$doc[d$Party=="Republican"], collapse = " ")

nchar(dem.groundtruth)
nchar(rep.groundtruth)

load("rfiles/d_LA.rdata")
dem.LA <- paste0(d$doc[d$Party=="Democratic"], collapse = " ")
rep.LA <- paste0(d$doc[d$Party=="Republican"], collapse = " ")

load("rfiles/d_IN.rdata")
dem.LA <- paste0(d$doc[d$Party=="Democratic"], collapse = " ")
rep.LA <- paste0(d$doc[d$Party=="Republican"], collapse = " ")

dem.cities <- paste0(dem.LA, dem.IN, collapse = " ")
rep.cities <- paste0(rep.LA, rep.IN, collapse = " ")

library(quanteda)
library(tm)
library(lsa)
library(corrplot)

# create corpus, then term-document matrix
crps <- Corpus(VectorSource(c(dem.groundtruth, rep.groundtruth, dem.cities, rep.cities)))
tdm <- TermDocumentMatrix(crps)
tdm <- as.matrix(tdm)

# cosine similarity
cosine.mat <- cosine(tdm)
colnames(cosine.mat) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
rownames(cosine.mat) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')

# correlation plot
png('./paper/figures/groundtruth_corrplot.png', width = 500, height = 500)
corrplot(cosine.mat, diag = F, order = "FPC", tl.col = "black", addCoef.col = "black", cl.pos = "n")
dev.off()

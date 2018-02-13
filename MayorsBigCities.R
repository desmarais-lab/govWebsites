library("stringr")
library("tools")
library("tibble")
library('pbapply')

# ------------- #
# Preprocessing #
# ------------- #

source("./preprocessing.R")

# Read in the data from txt files
d <- readCityDocuments("websites/bigcitymayors2TXT", "mayor_site")

# Do all the preprocessing that needs to be done before removing duplicate lines
d <- preprocessing_1(d, "mayor_site")

# Find duplicate lines
docDuplicates <- findDuplicates(d, "mayor_site", nthreads = 6)

# Use the counts of duplicate lines found above to remove them over a certain threshold
d$doc <- as.character(pbsapply(1:nrow(d), removeDuplicates))

# Do the rest of the preprocessing
d <- preprocessing_2(d, remove.city.names = T)

d <- as.data.frame(d)
save(d, file = "rfiles/d_bigcitymayors.rdata")

#lemmatization
#Note: due to memory constraints, it may be necessary to close R here, then run
# python3 lemmatization.py rfiles/d_IN.rdata
#from the console instead, open R again and continue on the next line (i.e. load)
#but don't forget to set the following againa after restarting R
#state <- "Indiana"
#stateAbb <- "LA"
system(paste0("python3 lemmatization.py ", "rfiles/d_bigcitymayors.rdata"))
load("rfiles/d_bigcitymayors.rdata")
#spacy puts -PRON- in place of pronouns since they can't be lemmatized
#we don't really care about pronouns, so we just remove them
d$doc <- str_replace_all(d$doc, "-PRON- ", "")

#Hunspell and removal of terms occuring in only one document
#these were originally in preprocessing.R, but were not working correctly
#functionally there is no downside to them being here, it just doesn't look as nice
source("hunspellParallel.R")
source('functions/occuranceRemove.R')

save(d, file = "rfiles/d_bigcitymayors.rdata")

# merge in partisanship
load("./rfiles/campaign_websites.rdata")
df$campaign_website <- str_replace(df$campaign_website, "https?://", "")
df$campaign_website <- str_replace(df$campaign_website, "/", "")
#get rid of incorrect endings
df$campaign_website <- str_replace(df$campaign_website, ".com.*$", ".com")
#merge
d2 <- merge(d, df, by.x = "mayor_site", by.y = "campaign_website")

#some websites did not get merged
#which?
missing <- setdiff(unique(d$mayor_site), unique(df$campaign_website))
#make a function to get a proximity match for the likely correct name
strdistmatch <- function(str1, strs2){
  a <- stringdist::stringdist(str1, strs2, method="jaccard")
  a <- which.min(a)
  a <- strs2[a]
  return(a)
}
missing_found <- sapply(missing, strdistmatch, unique(df$campaign_website))

#adjust a few by hand
missing_found[names(missing_found)=="jimgraycongress.com"] <- ""
missing_found[names(missing_found)=="paulsoglinforgovernor.com"] <- ""
missing_found[names(missing_found)=="vilyles.com"] <- "www.votevialexanderlyles.com"
missing_found[names(missing_found)=="stantonforarizona.com"] <- "" #"stantonforphoenix.com"
missing_found[names(missing_found)=="kenneyforphiladelphia.com"] <- "kenney2015.com"

df <- df[is.na(df$campaign_website)==F,]

for(i in 1:length(missing_found)){
  df$campaign_website[df$campaign_website==missing_found[i]] <- names(missing_found)[i]
}

#merge again
d2 <- merge(d, df, by.x = "mayor_site", by.y = "campaign_website")

#some websites did not get merged
#which?
missing <- setdiff(unique(d$mayor_site), unique(d2$mayor_site))
#only the ones running for statewide offices --> everything is fine now
d <- d2

#save the finished data frame
save(d, file = "rfiles/d_bigcitymayors.rdata")


# Analysis
load("rfiles/d_bigcitymayors.rdata")

dem.groundtruth <- paste0(d$doc[d$party=="(D)"], collapse = " ")
rep.groundtruth <- paste0(d$doc[d$party=="(R)"], collapse = " ")

nchar(dem.groundtruth)
nchar(rep.groundtruth)

load("rfiles/d_LA.rdata")
dem.LA <- paste0(d$doc[d$Party=="Democratic"], collapse = " ")
rep.LA <- paste0(d$doc[d$Party=="Republican"], collapse = " ")

load("rfiles/d_IN.rdata")
dem.IN <- paste0(d$doc[d$Party=="Democratic"], collapse = " ")
rep.IN <- paste0(d$doc[d$Party=="Republican"], collapse = " ")

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
png('./paper/figures/groundtruth_bigcities_corrplot.png', width = 500, height = 500)
corrplot(cosine.mat, diag = F, order = "FPC", tl.col = "black", addCoef.col = "black", cl.pos = "n")
dev.off()



## bootstrap CI

load("rfiles/d_bigcitymayors.rdata")
mayors <- subset(d, select = c("doc","party"))
load("rfiles/d_LA.rdata")
LA <- subset(d, select = c("doc","Party"))
load("rfiles/d_IN.rdata")
IN <- subset(d, select = c("doc","Party"))
cities <- rbind(IN,LA)
rm(list = ls()[!ls()%in%c("mayors","cities")])

nsamples <- 1000
results <- array(NA, dim = c(4,4,nsamples))

for(i in 1:nsamples){
  dem.cities.sample <- sample(cities$doc[cities$Party=="Democratic"], length(cities$doc[cities$Party=="Democratic"])*0.75)
  dem.cities.sample <- paste0(dem.cities.sample, collapse = " ")
  rep.cities.sample <- sample(cities$doc[cities$Party=="Republican"], length(cities$doc[cities$Party=="Republican"])*0.75)
  rep.cities.sample <- paste0(rep.cities.sample, collapse = " ")
  dem.groundtruth.sample <- sample(mayors$doc[mayors$party=="(D)"], length(mayors$doc[mayors$party=="(D)"])*0.75)
  dem.groundtruth.sample <- paste0(dem.groundtruth.sample, collapse = " ")
  rep.groundtruth.sample <- sample(mayors$doc[mayors$party=="(R)"], length(mayors$doc[mayors$party=="(R)"])*0.75)
  rep.groundtruth.sample <- paste0(rep.groundtruth.sample, collapse = " ")
  
  
  #dem.groundtruth <- paste0(mayors$doc[mayors$party=="(D)"], collapse = " ")
  #rep.groundtruth <- paste0(mayors$doc[mayors$party=="(R)"], collapse = " ")
  #dem.cities <- paste0(cities$doc[cities$Party=="Democratic"], collapse = " ")
  #rep.cities <- paste0(cities$doc[cities$Party=="Republican"], collapse = " ")
  
  
  library(quanteda)
  library(tm)
  library(lsa)
  library(corrplot)
  
  # create corpus, then term-document matrix
  crps <- Corpus(VectorSource(c(dem.groundtruth.sample, rep.groundtruth.sample, dem.cities.sample, rep.cities.sample)))
  tdm <- TermDocumentMatrix(crps)
  tdm <- as.matrix(tdm)
  
  # cosine similarity
  cosine.mat <- cosine(tdm)
  #colnames(cosine.mat) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
  #rownames(cosine.mat) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
  results[,,i] <- cosine.mat
}

save(results, file = "rfiles/groundtruth_bigcities_bootstrap.rdata")

results.mean <- apply(results, c(1,2), mean)
results.low <- apply(results, c(1,2), quantile, 0.025)
results.up <- apply(results, c(1,2), quantile, 0.975)

colnames(results.mean) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
rownames(results.mean) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
colnames(results.low) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
rownames(results.low) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
colnames(results.up) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')
rownames(results.up) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')

png('./paper/figures/groundtruth_bs_bigcities_corrplot.png', width = 500, height = 500)
corrplot(results.mean, diag = F, order = "FPC", tl.col = "black", addCoef.col = "black", cl.pos = "n")
dev.off()

results.both <- data.frame(matrix(NA, nrow = 4, ncol = 4))
for(i in 1:nrow(results.both)){
  for(j in 1:ncol(results.both)){
    results.both[i,j] <- paste(as.character(round(c(results.low[i,j], results.up[i,j]),3)), collapse = ", ")
  }
}

library(xtable)

colnames(results.both) <- rownames(results.both) <- c('dem.groundtruth', 'rep.groundtruth', 'dem.cities', 'rep.cities')

xt <- xtable(results.both,
             caption = paste0("Ground truth test, comparing campaign websites of mayors of the 100 largest cities in the US and cities in Indiana and Louisiana. The values are bootstrapped confidence bounds for cosine similarities between concatenated document collections."),
             label = "groundtruth_bootstrapped")

xt <- print.xtable(xt, 
                   include.rownames = T)#,
                   #size = "\\fontsize{9pt}{10pt}\\selectfont")

writeLines(xt, 
           con = paste0("paper/tables/groundtruth_bootstrapped.tex"))


### corrplot again'
# correlation plot
png('./paper/figures/groundtruth_bs_bigcities_corrplot.png', width = 500, height = 500)
corrplot(results.mean, diag = F, order = "original", tl.col = "black", addCoef.col = "black", cl.pos = "n")
dev.off()

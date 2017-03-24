#This script explors topic models just for Indianapolis
#To this end, folders are created
#and the websites for Indianapolis are scraped at 4 points in time
#at the end, a plot with the word-topic probabilities is written to /paper/figures

#required data: indiana2015.rdata, produced by govWebsitesIndiana2015.R

library(topicmodels)
library(tidytext) #tidy()
library(ggplot2)
library(dplyr)
library(jsonlite) #for waybackmachine API
require(tm) #text mining
require(cowplot)

setwd("~/Dropbox/4_RA/govWebsites") #Linux
load(file="data/indiana2015.rdata")

API_base <- 'http://archive.org/wayback/available?url='
indianapolis <- subset(indiana, District=="Indianapolis")

test <- indianapolis$redirect

setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/")

#system("mkdir oct15")
#system("mkdir nov15")
#system("mkdir dec15")
#system("mkdir jan16")
#system("mkdir feb16")
#system("mkdir mar16")
#system("mkdir apr16")
#setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/feb16/")

#loop through websites, results automatically get saved into 'websites' folder inside wd

#the following functions takes two inputs:
#test - vector of website urls
#date "YearMonth", i.e. "201610"
#there is no return() because the output (i.e. the downloaded websites) is directly written to the HD
waybackDownloader <- function(test, date){
  setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/")
  system(paste("mkdir", date, sep=" "))
  setwd(paste("/home/markus/Dropbox/4_RA/govWebsites/websites/",date,sep=""))
  
  for (i in 1:length(test)){
    
    website <- test[i] #loop through websites
    
    #the following three lines aren't actually needed when using the Ruby package
    API_URL <- paste(API_base,website,sep = "")
    wayback <- fromJSON(API_URL)
    waybackURL <- wayback$archived_snapshots$closest$url
    
    #pasting input for Ruby package, then executing it
    #--concurrency 20 causes 20 items to be downloaded at the same time
    #the default is 1, this takes WAY too long (i.e. one hour for a website...)
    #--from 201510 downloads a snapshot from October 2015, or, if not available, later
    
    #The mayoral elections in IN happened on November 3
    WBMD_base <- paste("wayback_machine_downloader --concurrency 40 --from", date, sep=" ")
    WBMD_site <- paste(WBMD_base,website)
    system(WBMD_site, intern = T) #just ignore the printout if running outside of loop
  }
}
##############################################################

###OCTOBER 2015
setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/oct15/txtfolder/")

docs <- Corpus(DirSource(pattern = ".*.txt"))

#Preprocessing
docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #remove numbers
docs <- tm_map(docs, tolower) #converting to lowercase
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
dtmOCT15 <- DocumentTermMatrix(docs)

#remove empty docs from dtm
rowTotals <- apply(dtmOCT15, 1, sum) #Find the sum of words in each Document
dtmOCT15   <- dtmOCT15[rowTotals> 0, ]

#LDA October 2015
ap_ldaOCT15 <- LDA(dtmOCT15, k = 4, control = list(seed = 1234))
ap_topics <- tidy(ap_ldaOCT15, matrix = "beta")

#Word-topic probabilities - Indianapolis October 2015
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

plotOCT15 <- ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title="Word-topic probabilities - Indianapolis October 2015")

###MARCH 2016
setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/mar16/txtfolder/")

docs <- Corpus(DirSource(pattern = ".*.txt"))

#Preprocessing
docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #remove numbers
docs <- tm_map(docs, tolower) #converting to lowercase
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
dtmMAR16 <- DocumentTermMatrix(docs)

#remove empty docs from dtm
rowTotals <- apply(dtmMAR16, 1, sum) #Find the sum of words in each Document
dtmMAR16   <- dtmMAR16[rowTotals> 0, ]

#LDA March 2016
ap_ldaMAR16 <- LDA(dtmMAR16, k = 4, control = list(seed = 1234))
ap_topics <- tidy(ap_ldaMAR16, matrix = "beta")

#Word-topic probabilities - Indianapolis March 2016
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

plotMAR16 <- ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title="Word-topic probabilities - Indianapolis March 2016")

#Plot the two results together
resultIndy <- plot_grid(plotOCT15,plotMAR16,nrow=2)
resultIndy
ggsave(resultIndy, file="../../../paper/figures/wtpIndy.pdf",width=10,height=10)

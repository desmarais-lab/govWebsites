#This option has to be set BEFORE loading the rJava package (loaded by mallet)
#Gives rJava about 4GB to work with
#So far, I've only needed 2.3GB
options(java.parameters = "-Xmx3000m")

library('mallet')
library('tibble')
library('stringr')
library('tools')
library('tm')
library('ggplot2')
library('cowplot')
library('dplyr')
library('quanteda')
library('hunspell')

corpus <- "current" #"current", "before", or "after"
filepath <- str_c("./websites/", corpus)

## Reading in the data

f <- list.files(path = filepath, recursive = T) #create a list of all files in all subdirectories

#file types
ext <- file_ext(f) #get file extension
folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]

#store objects in tibble
d <- tibble(path = str_c(filepath, f, sep = "/"), 
            folder = str_c(filepath, folder, sep = "/"),
            filename,
            ext)

#read in file text
d$doc <- sapply(d$path, function(x){str_c(readLines(x), sep = " ", collapse = " ")})
d$doc <- as.character(d$doc)

## Pre-processing

#Remove xml files
d$xml <- str_detect(d$doc, "<?xml version=") #note which docs are xml
d <- filter(d, d$xml==F) #Drop rows that are xml docs
d <- select(d, -xml) #Drop xml variable

#Remove words containing underscores
d$doc <- str_replace_all(d$doc, "\\w*_\\w*", "")

#Remove bullet points
d$doc <- str_replace_all(d$doc, "  o ", " ")

#Remove punctuation
d$doc <- removePunctuation(d$doc)

#Remove numbers
d$doc <- removeNumbers(d$doc)

#Remove extraneous whitespaces
d$doc <- stripWhitespace(d$doc)

#Convert text to UTF-8
d$doc <- iconv(d$doc, "latin1", "UTF-8")

#Everything to lowercase
d$doc <- tolower(d$doc)

#extract city from directory
d$city <- str_split_fixed(d$path, str_c(filepath, "/"), 2)[,2]
d$city <- str_split_fixed(d$city, "/", 2)[,1]

#import and merge in city coeffs
load("./data/URLs_IN.rdata")
URLs$foldername[URLs$foldername=="www.batesvilleindiana.us"] <- "batesvilleindiana.us"
URLs$foldername[URLs$foldername=="www.bloomington.in.gov"] <- "bloomington.in.gov"
URLs$foldername[URLs$foldername=="www.brazil.in.gov"] <- "brazil.in.gov"
URLs$foldername[URLs$foldername=="www.elwoodcity-in.org"] <- "elwoodcity-in.org"
d <- merge(d, URLs, by.x = "city", by.y = "foldername")

#remove empty files
d <- d[!d$doc=="",]
d <- d[!d$doc==" ",]


#################
#setwd("/home/markus/govWebsites/websites2/websites_backup/html/")
#a <- scan("index", what = "raw")
#a <- readLines("index")


save(d, file = "./rfiles/dd.Rdata")
load(file = "./rfiles/dd.Rdata")

#Hunspell
#source("hunspellParallel.R")
load(file = str_c("./rfiles/docs_", corpus, ".Rdata"))

#remove documents where spellchecking failed
d <- d[d$spell_fail==0,]

#remove non-spellchecked text
d$doc <- d$doc2
d <- select(d, -doc2)

#Stemming
#d$doc2 <- stemDocument(d$doc)

#stopwords list from the tm package
stopwrds <- stopwords("english")
#additional words for the tm package
stopwrds <- append(stopwrds, c("will","e"))

#write  to a text file
fileConn <- file("./rfiles/stopwords.txt")
writeLines(stopwrds, fileConn)
close(fileConn)

################################################################################

## Mallet

#Import website text
mallet.instances <- mallet.import(id.array = make.unique(d$folder),
                                  text.array = d$doc,
                                  stoplist.file = "./rfiles/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#Create a Mallet topic model trainer
ntopics <- 100
topic.model <- MalletLDA(num.topics = ntopics)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary (i.e. list of all the words)
vocabulary <- topic.model$getVocabulary()
vocabulary

## word frequencies
## also contains a column with the total number of documents a word appears in
word.freqs <- mallet.word.freqs(topic.model)
word.freqs[order(word.freqs$term.freq, decreasing = T),]

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

#R seems to be using about 2.3GB while training the model
#takes about 5 minutes

## NEW: run through a few iterations where we pick the best topic for each token, 
##  rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

## What are the top words in topic 10?
##  Notice that R indexes from 1, so this will be the topic that mallet called topic 10.
#mallet.top.words(topic.model, topic.words[10,])

## Show the first few documents with at least 5
#head(documents[ doc.topics[7,] > 0.05 & doc.topics[10,] > 0.05, ])

#Plot top words for topics
df.words <- tibble()

for(i in 1:18){
  df.words2 <- as.tibble(mallet.top.words(topic.model, topic.words[i,]))
  df.words2$topic <- str_c("topic_", str_pad(i, 2, pad = "0"))
  df.words <- rbind(df.words, df.words2)
}



#Word-topic probabilities
plotWTP_01 <- df.words %>% ggplot(aes(words, weights, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  labs(title="Word-topic probabilities - wget")
plotWTP_01

ggsave(plotWTP_01, file = str_c("./paper/figures/wtp_", corpus, ".pdf"), width=7, height=9)



## Subsets of the corpus

## How do topics differ across different sub-corpora?
dem.topic.words <- mallet.subset.topic.words(topic.model, d$winner == "Democratic",
                                             smoothed=T, normalized=T)
rep.topic.words <- mallet.subset.topic.words(topic.model, d$winner == "Republican",
                                             smoothed=T, normalized=T)

## How do they compare? (for the 10th topic)
#mallet.top.words(topic.model, dem.topic.words[10,])
#mallet.top.words(topic.model, rep.topic.words[10,])

### Plot the results of the subsets

#Plot top words for topics
df.words.dem <- tibble()
df.words.rep <- tibble()

for(i in 1:ntopics){
  df.words.dem2 <- as.tibble(mallet.top.words(topic.model, dem.topic.words[i,]))
  df.words.rep2 <- as.tibble(mallet.top.words(topic.model, rep.topic.words[i,]))
  df.words.dem2$topic <- str_c("topic_", str_pad(i, 2, pad = "0"))
  df.words.rep2$topic <- str_c("topic_", str_pad(i, 2, pad = "0"))
  df.words.dem <- rbind(df.words.dem, df.words.dem2)
  df.words.rep <- rbind(df.words.rep, df.words.rep2)
}

#order the topics by their mean weight
topic.order <- group_by(df.words.dem, topic) %>% 
  summarise(meanweights = mean(weights)) %>%
  arrange(-meanweights) %>%
  select(topic)

#save objects
#df.words.dem3 <- df.words.dem
#df.words.rep3 <- df.words.rep

#keep only the top 5 (of the Democrats, for both parties)
df.words.dem2 <- df.words.dem[df.words.dem$topic%in%topic.order$topic[1:5],]
df.words.rep2 <- df.words.rep[df.words.rep$topic%in%topic.order$topic[1:5],]

#Plot word-topic probabilities
plotWTP_dem <- df.words.dem2 %>% ggplot(aes(words, weights, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 1) +
  coord_flip() +
  labs(title = "Democrat")
plotWTP_rep <- df.words.rep2 %>% ggplot(aes(words, weights, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 1) +
  coord_flip() +
  labs(title = "Republican")

#arrange both plots next to each other
plotWTP_dem_rep <- plot_grid(plotWTP_dem, plotWTP_rep)
plotWTP_dem_rep

#save
ggsave(plotWTP_dem_rep, file = str_c("./paper/figures/wtp_", corpus, "_dem_rep.pdf"), width=7, height=9)



#Sort topics by the ratio of the mean prevalence of the first ten words between Dems and Reps
df.words.dem3 <- group_by(df.words.dem, topic) %>% 
  summarise(meanweights = mean(weights))

df.words.rep3 <- group_by(df.words.rep, topic) %>% 
  summarise(meanweights = mean(weights))

df.words.compare <- cbind(df.words.dem3, df.words.rep3$meanweights)
df.words.compare$ratio <- df.words.compare$meanweights/df.words.compare$`df.words.rep3$meanweights`

df.words.compare <- df.words.compare[order(df.words.compare$ratio),]

df.words.dem4 <- df.words.dem[df.words.dem$topic%in%df.words.compare$topic[1:5],]
df.words.rep4 <- df.words.rep[df.words.rep$topic%in%df.words.compare$topic[1:5],]

#Plot word-topic probabilities
plotWTP_dem <- df.words.dem4 %>% ggplot(aes(words, weights, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 1) +
  coord_flip() +
  labs(title = "Democrat")
plotWTP_rep <- df.words.rep4 %>% ggplot(aes(words, weights, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 1) +
  coord_flip() +
  labs(title = "Republican")

#arrange both plots next to each other
plotWTP_dem_rep <- plot_grid(plotWTP_dem, plotWTP_rep)
plotWTP_dem_rep

#save
ggsave(plotWTP_dem_rep, file = str_c("./paper/figures/wtp_", corpus, "_dem_rep_large_differences.pdf"), width=7, height=9)



###What proportion of tokens in docs in Dem-led cities are assigned to topic X, for all X

#Which topic is a word most likely to be assigned to?
topicnames <- unique(select(df.words.dem, topic))
topicnames$tokenfreq_dem <- table(apply(dem.topic.words, 2, which.max))
topicnames$tokenfreq_rep <- table(apply(rep.topic.words, 2, which.max))

#Strikingly, the #1 topic for Republicans includes 17660 tokens (more than half)
#whereas the #1 topic for Democrats includes only 1324
#the question is: is this a meaningful diffference, or just a result of the number of topics,
#which at the moment is only 100

#Create variable to appear as the title in the plot at the end ("topic_number (# tokens)")
topicnames$topic_tokenfreq_dem <- str_c(topicnames$topic, " (", topicnames$tokenfreq_dem, " tokens)")
topicnames$topic_tokenfreq_rep <- str_c(topicnames$topic, " (", topicnames$tokenfreq_rep, " tokens)")

#reorder so the topics with the most words are on top
topic.order.dem <- arrange(topicnames, -tokenfreq_dem)
topic.order.rep <- arrange(topicnames, -tokenfreq_rep)

#get only the top five of topics are in the plot
df.words.dem5 <- df.words.dem[df.words.dem$topic%in%topic.order.dem$topic[1:5],]
df.words.rep5 <- df.words.rep[df.words.rep$topic%in%topic.order.rep$topic[1:5],]
#include variable with topic title created above
df.words.dem5$topics <- topic.order.dem$topic_tokenfreq_dem[as.numeric(match(df.words.dem5$topic, topic.order.dem$topic))]
df.words.rep5$topics <- topic.order.rep$topic_tokenfreq_rep[as.numeric(match(df.words.rep5$topic, topic.order.rep$topic))]
#include variable with token number in topic, to sort by
df.words.dem5$tokenfreq <- topic.order.dem$tokenfreq_dem[as.numeric(match(df.words.dem5$topic, topic.order.dem$topic))]
df.words.rep5$tokenfreq <- topic.order.rep$tokenfreq_rep[as.numeric(match(df.words.rep5$topic, topic.order.rep$topic))]
#sort by token number
df.words.dem5 <- df.words.dem5[order(df.words.dem5$tokenfreq, decreasing = T),]
df.words.rep5 <- df.words.rep5[order(df.words.rep5$tokenfreq, decreasing = T),]
#change topics (i.e. the title variable) to a factor,
#whose levels are in the order in which they appear in the data frame
#this is done with mutate (dplyr); at the end, unique() is necessary because the topics appear multiple times
df.words.dem5 <- mutate(df.words.dem5, topics = factor(topics, unique(topics)))
df.words.rep5 <- mutate(df.words.rep5, topics = factor(topics, unique(topics)))

#Plot word-topic probabilities
plotWTP_dem <- df.words.dem5 %>% ggplot(aes(words, weights, fill = topics)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topics, scales = "free", ncol = 1) +
  coord_flip() +
  labs(title = "Democrat")
plotWTP_rep <- df.words.rep5 %>% ggplot(aes(words, weights, fill = topics)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topics, scales = "free", ncol = 1) +
  coord_flip() +
  labs(title = "Republican")

#arrange both plots next to each other
plotWTP_dem_rep <- plot_grid(plotWTP_dem, plotWTP_rep)
plotWTP_dem_rep

#save
ggsave(plotWTP_dem_rep, file = str_c("./paper/figures/wtp_", corpus, "_dem_rep_order.pdf"), width=7, height=9)

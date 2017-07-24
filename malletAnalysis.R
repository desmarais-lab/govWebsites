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

#Hunspell
load(file = str_c("./rfiles/docs_", corpus, ".Rdata"))

#remove documents where spellchecking failed
d <- d[d$spell_fail==0,]

#remove non-spellchecked text
d$doc <- d$doc2
d <- select(d, -doc2)

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

####################################################################
####################################################################
####################################################################
####################################################################

library(ggplot2)
library(dplyr)
library(tidyr)

## Retrieve a matrix of topic weights for every document
## Rows are documents
## Columns are topics
## If normalized is set to FALSE, this is the actual number of words of each topic in the documents
# For example, document 2 (second row) contains 37 words of topic 2 (second column)
# Note that in order for actual integers to be displayed, smoothed also needs to be set to false
## If normalized is set to TRUE, each document sums to one
# For example, 6.855605e-02 of all words in document 2 (second row) belong to topic 2 (second column)
doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)
#dim(doc.topics) #12397 documents; 100 topics
#as.tibble(doc.topics)

## Retrieve a matrix of words weights for topics
## Rows are topics
## Columns are word types
## If normalized is set to FALSE, this is the actual number of words of each type in the topics
# For example, topic 1 (first row) contains 4379 token instances of word type 2 (second column)
# Note that in order for actual integers to be displayed, smoothed also needs to be set to false
## If normalized is set to TRUE, each topic (row) sums to one
# For example, 3.105702e-02 of all token instances in topic 1 (first row) are made up by word type 2 (second column)
topic.words <- mallet.topic.words(topic.model, smoothed = T, normalized = T)
#dim(topic.words) #100 topics; 31472 word types
#as.tibble(topic.words)

## Subset the matrix into two matrices, one containing
# documents from Democratic cities, the other from Republican cities
republican <- doc.topics[which(d$winner == "Republican"),]
democratic <- doc.topics[which(d$winner == "Democratic"),]

republican <- as.tibble(republican)
democratic <- as.tibble(democratic)


## Plot the densities of weights across documents for each topic
# i.e. each line is a topic
doc.topics.dens.plot.dem <- gather(republican)
doc.topics.dens.plot.rep <- gather(democratic)

doc.topics.dens.plot.dem$reptopicprop <- rep(rep.topic.prop, each = nrow(republican))
doc.topics.dens.plot.rep$demtopicprop <- rep(dem.topic.prop, each = nrow(democratic))

doc.topics.dens.plot.dem.g <- ggplot(doc.topics.dens.plot.dem, aes(x = value, group = key, colour = reptopicprop)) + 
  geom_density(size=.2, alpha=0.1) + 
  theme(legend.position="none") + 
  xlim(0, 0.001) + ylim(0, 30000) +
  ggtitle("Republican") +
  scale_color_gradient(low = "indianred", high = "black")
doc.topics.dens.plot.rep.g <- ggplot(doc.topics.dens.plot.rep, aes(x = value, group = key, colour = demtopicprop)) + 
  theme(legend.position="none") + 
  geom_density(size=.2, alpha=0.1) + 
  xlim(0, 0.001) + ylim(0, 30000) +
  ggtitle("Democratic") + 
  scale_color_gradient(low = "dodgerblue", high = "black")

doc.topics.dens.plot.g <- plot_grid(doc.topics.dens.plot.dem.g, doc.topics.dens.plot.rep.g)
doc.topics.dens.plot.g

ggsave(doc.topics.dens.plot.g, file = str_c("./paper/figures/doctopics_density.pdf"), width=9, height=7)

## The way I am reading this is that documents in Democratic cities are more clearly dedicated to one purpose,
# as each topic has more very low values (i.e. most documents don't contain this topic), but also longer tails,
# (i.e. a document that belongs to a specific topic)


## For each topic, calculate the median (or mean) weight across documents
# i.e., on average (or median), documents contain x percent of words belonging to topic y
rep.topic.prop <- apply(republican, 2, median)
dem.topic.prop <- apply(democratic, 2, median)
#rep.topic.prop <- apply(republican, 2, mean)
#dem.topic.prop <- apply(democratic, 2, mean)

## Plot distribution of both
weights.tibble <- tibble(Republican = rep.topic.prop, Democratic = dem.topic.prop) %>%
  gather(Party, weight)

topicweights_density <- ggplot(weights.tibble, aes(weight, color = Party)) + 
  geom_density() + theme(legend.position="top")
topicweights_density

ggsave(topicweights_density, file = str_c("./paper/figures/topicweights_density.pdf"), width=9, height=7)


## For each topic, calculate the absolute difference
# between the weight of this topic in Democratic and Republican cities/documents
topicdiffs <- abs(dem.topic.prop-rep.topic.prop) %>% tibble(diffs = .)

## Plot
ggplot(topicdiffs, aes(diffs)) + geom_histogram(bins = 100)

#hist(topicdiffs)
#plot(density(topicdiffs))

#which(topicdiffs>.05)

bigdiffs <- which(topicdiffs > quantile(topicdiffs$diffs, .95))

#dem.topic.prop[bigdiffs]
#rep.topic.prop[bigdiffs]

#Plot top words for topics
df.words <- tibble()

for(i in 1:length(bigdiffs)){
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




######

doc.topics.dens.plot.dem <- gather(republican[,c(bigdiffs)])
doc.topics.dens.plot.rep <- gather(democratic[,c(bigdiffs)])

doc.topics.dens.plot.dem$reptopicprop <- rep(rep.topic.prop[bigdiffs], each = nrow(republican))
doc.topics.dens.plot.rep$demtopicprop <- rep(dem.topic.prop[bigdiffs], each = nrow(democratic))

doc.topics.dens.plot.dem.g <- ggplot(doc.topics.dens.plot.dem, aes(x = value, colour = key)) + 
  geom_density(size=.5, alpha=0.5) + 
  xlim(0, 0.01) + ylim(0, 6000) +
  ggtitle("Republican")
doc.topics.dens.plot.rep.g <- ggplot(doc.topics.dens.plot.rep, aes(x = value, colour = key)) + 
  theme(legend.position="none") + 
  geom_density(size=.5, alpha=0.5) + 
  xlim(0, 0.01) + ylim(0, 6000) +
  ggtitle("Democratic")

doc.topics.dens.plot.g <- plot_grid(doc.topics.dens.plot.dem.g, doc.topics.dens.plot.rep.g)
doc.topics.dens.plot.g

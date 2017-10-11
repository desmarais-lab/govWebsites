source("malletTrain.R")

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
library('tidyr')

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

## For each topic, calculate the median (or mean) weight across documents
# i.e., on average (or median), documents contain x percent of words belonging to topic y
rep.topic.prop <- apply(republican, 2, median)
dem.topic.prop <- apply(democratic, 2, median)

## Plot distribution of both
weights.tibble <- tibble(Republican = rep.topic.prop, Democratic = dem.topic.prop) %>%
  gather(Party, weight)
topicweights_density <- ggplot(weights.tibble, aes(weight, color = Party)) + 
  geom_density(size=1) + 
  theme(legend.position="top") +
  scale_color_manual(values=c("#4286f4", "#f44441")) +
  ggtitle("Median document")
#topicweights_density
#ggsave(topicweights_density, file = str_c("./paper/figures/topicweights_density.pdf"), width=9, height=7)

#Do the same thing, but with the mean instead of the median
rep.topic.prop2 <- apply(republican, 2, mean)
dem.topic.prop2 <- apply(democratic, 2, mean)
## Plot distribution of both
weights.tibble2 <- tibble(Republican = rep.topic.prop2, Democratic = dem.topic.prop2) %>%
  gather(Party, weight)
topicweights_density2 <- ggplot(weights.tibble2, aes(weight, color = Party)) + 
  geom_density(size=1) + 
  theme(legend.position="top") +
  scale_color_manual(values=c("#4286f4", "#f44441")) +
  ggtitle("Mean document")
#topicweights_density2
#ggsave(topicweights_density2, file = str_c("./paper/figures/topicweights_density2.pdf"), width=9, height=7)

#Plot them together
topicweights_density3 <- plot_grid(topicweights_density, topicweights_density2)
topicweights_density3
ggsave(topicweights_density3, file = str_c("./paper/figures/topicweights_density.pdf"), width=9, height=5)

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

## For each topic, calculate the absolute difference
# between the weight of this topic in Democratic and Republican cities/documents
topicdiffs <- abs(dem.topic.prop-rep.topic.prop) %>% tibble(diffs = .)
#topicdiffs <- abs(dem.topic.prop2-rep.topic.prop2) %>% tibble(diffs = .)

## Plot
diffsize <- ggplot(topicdiffs, aes(diffs)) + 
  geom_histogram(bins = 100) +
  labs(x = "Size of differences")
diffsize
ggsave(diffsize, file = str_c("./paper/figures/diffsize.pdf"), width=9, height=7)

#get the biggest differences
#>95th percentile
bigdiffs <- which(topicdiffs > quantile(topicdiffs$diffs, .95))
#10 biggest
bigdiffs <- match(topicdiffs[order(topicdiffs$diffs, decreasing = T),]$diffs[1:10], topicdiffs$diffs) %>%
  .[is.na(.)==F]

#dem.topic.prop[bigdiffs]
#rep.topic.prop[bigdiffs]

#Plot top words for topics
df.words <- tibble()

for(i in 1:length(bigdiffs)){
  df.words2 <- as.tibble(mallet.top.words(topic.model, topic.words[bigdiffs[i],]))
  df.words2$topic <- str_c("topic_", str_pad(bigdiffs[i], 2, pad = "0"), " (", round(topicdiffs$diffs[bigdiffs[i]], digits = 6),")")
  df.words <- rbind(df.words, df.words2)
}

#Word-topic probabilities
plotWTP_bigdiffs <- df.words %>% ggplot(aes(words, weights, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip() #+
  #labs(title="Word-topic probabilities - wget")
plotWTP_bigdiffs

ggsave(plotWTP_bigdiffs, file = str_c("./paper/figures/plotWTP_bigdiffs.pdf"), width=6, height=9)


## How do the topic weight density plots from above
# look like just for the topics with big differences?
doc.topics.dens.plot.dem <- gather(republican[,c(bigdiffs)])
doc.topics.dens.plot.rep <- gather(democratic[,c(bigdiffs)])

doc.topics.dens.plot.dem$reptopicprop <- rep(rep.topic.prop[bigdiffs], each = nrow(republican))
doc.topics.dens.plot.rep$demtopicprop <- rep(dem.topic.prop[bigdiffs], each = nrow(democratic))

doc.topics.dens.plot.dem.g <- ggplot(doc.topics.dens.plot.dem, aes(x = value, colour = key)) + 
  geom_density(size=.5, alpha=0.5) + 
  xlim(0, 0.0021) + ylim(0, 6000) +
  ggtitle("Republican")
doc.topics.dens.plot.rep.g <- ggplot(doc.topics.dens.plot.rep, aes(x = value, colour = key)) + 
  theme(legend.position="none") + 
  geom_density(size=.5, alpha=0.5) + 
  xlim(0, 0.0021) + ylim(0, 6000) +
  ggtitle("Democratic")

doc.topics.dens.plot.g <- plot_grid(doc.topics.dens.plot.dem.g, doc.topics.dens.plot.rep.g)
doc.topics.dens.plot.g
ggsave(doc.topics.dens.plot.g, file = str_c("./paper/figures/doctopics_density_bigdiffs.pdf"), width=10, height=6)



##Heatmaps of Democratic/Republican matrix
library(reshape2)

democratic.melted <- melt(as.matrix(democratic))
names(democratic.melted) <- c("Document", "Topic", "Weight")
heatmap_democratic <- ggplot(democratic.melted, aes(x = Topic, y = Document, fill = Weight)) + 
  geom_tile() + 
  scale_y_reverse() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Democratic")

republican.melted <- melt(as.matrix(republican))
names(republican.melted) <- c("Document", "Topic", "Weight")
heatmap_republican <- ggplot(republican.melted, aes(x = Topic, y = Document, fill = Weight)) + 
  geom_tile() + 
  scale_y_reverse() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Republican")

heatmaps_g <- plot_grid(heatmap_democratic, heatmap_republican, nrow = 2)

ggsave(heatmaps_g, file = str_c("./paper/figures/heatmaps_weights.png"), width=5, height=8)

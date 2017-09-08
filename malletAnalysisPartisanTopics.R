source('malletTraining.R')

library('tibble')
library('stringr')
library('ggplot2')
library('cowplot')
source('functions/mallet_helper_functions.R')
source('functions/topic_order.R')
source('functions/coherence_order.R')
source('functions/comparing_proportions.R')
source('functions/city_entropy.R')

#doc-topic and topic-word matrices
doc.topics <- mallet.doc.topics(topic.model, smoothed = F, normalized = F)
topic.words <- mallet.topic.words(topic.model, smoothed = F, normalized = F)

#How many of the topics with the highest differences should be displayed?
n_topics <- 24
#In order to consider only a restricted set of topics, enter a vector of their indices here
#Otherwise, set to NULL
limited_topics <- NULL
#important.topics <- topic_order()
#coherent.topics <- coherence_order(d$doc)
#high.entropy.topics <- city_entropy_order(city_entropy(mallet.doc.topics(topic.model, smoothed = F, normalized = F), d))
#limited_topics <- intersect(important.topics[1:50], coherent.topics[1:50], high.entropy.topics[1:50])
#if(n_topics<length(limited_topics))n_topics <- length(limited_topics)

## FIND THE TOPICS WITH THE HIGHEST PROPORTIONAL DIFFERENCES

#Split document-topic matrix into Republican and Democratic parts
republican <- doc.topics[which(d$winner == "Republican"),]
democratic <- doc.topics[which(d$winner == "Democratic"),]

#Democratic words per topic, as a proportion of Republican words per topic
prop.dem <- (colMeans(democratic)/sum(colMeans(democratic)))/(colMeans(republican)/sum(colMeans(republican)))
#Republican words per topic, as a proportion of Democraticepublican words per topic
prop.rep <- (colMeans(republican)/sum(colMeans(republican)))/(colMeans(democratic)/sum(colMeans(democratic)))
#Create an object in which each value is the proportion of the party which has a higher
#value for this topic
props <- prop.dem
props[prop.dem<1] <- prop.rep[prop.dem<1]
props <- tibble(topic = 1:length(props), prop.diff = props)
props$party <- NA
props$party[prop.dem<1] <- "Republican"
props$party[prop.dem>1] <- "Democratic"
props$D <- round(colSums(democratic)/sum(colSums(democratic)), 3)
props$R <- round(colSums(republican)/sum(colSums(republican)), 3)
props$p <- 2*pnorm(-abs(zcomparison()))
props$sig <- ifelse(props$p<=0.01, T, F)

#Which topics are owned by which party?
dem.topics <- which(prop.dem>1)
rep.topics <- which(prop.rep>1)

#Get the indices of the 10 topics with the highest proportional differences
if(is.null(limited_topics)==F){
  props <- props[props$topic%in%limited_topics,]
}
topicnumbers <- props$topic[order(props$prop.diff, decreasing = T)][1:n_topics]
#Get the same number of topics for both parties
#topicnumbers <- props$topic[order(props$prop.diff, decreasing = T)][props$party=="Democratic"][1:(n_topics/2)]
#topicnumbers <- c(topicnumbers,props$topic[order(props$prop.diff, decreasing = T)][props$party=="Republican"][1:(n_topics/2)])

## DISPLAY THE TOPICS WITH THE HIGHEST PROPORTIONAL DIFFERENCES

#Make a dataframe containing the word-topic probabilities (or frequencies)
df.words <- lapply(topicnumbers, returnWTP, nwords = 8, topic.word.matrix = topic.words)
df.words <- do.call(rbind, df.words)

#What is the index of the topic?
df.words$topic.num <- str_replace(df.words$topic, "Topic ", "")

#Which party does the topic 'belong' to?
## (Measured by which party has more words in the documents for the topic)
#df.words$party <- NA
#df.words$party[df.words$topic.num %in% dem.topics] <- "Democratic"
#df.words$party[df.words$topic.num %in% rep.topics] <- "Republican"

#Merge in the proportion
df.words <- merge(df.words, props, by.x = "topic.num", by.y = "topic", all.x = T)

#The name of the topic in the plot, consisting of its index and by how many times
#more it appears for one party as opposed to the other
#df.words$topic <- str_c(df.words$topic, " (", round(df.words$prop.diff, 2), " times more)")

#Changed this to instead include the proportion of tokens written by Dems/Reps in this topic:
df.words$topic <- str_c(df.words$topic, " (", "D", df.words$D, "/", "R", df.words$R, ifelse(df.words$sig==T, "*", ""), ")")

# Word-topic-probability plot
partisanTopics <- df.words %>% ggplot(aes(words, weights, group = factor(topic), fill = party)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  coord_flip() +
  ylab("") + xlab("") +
  scale_fill_manual(values = c("dodgerblue", "indianred")) #+
  #theme(strip.text.x = element_text(size = 8, colour = "orange"))
partisanTopics

#Save
#ggsave(partisanTopics, file = "paper/figures/partisanTopics.pdf", width = 8, height = 9)
#ggsave(partisanTopics, file = "paper/figures/partisanTopics_all_noweigts.pdf", width = 16, height = 9)
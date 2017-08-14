# Mallet helper functions

#given the index of a topic, 
## the number of top words
## a topic-word matrix (produced with mallet.topic.words())
## and a topic model object
### return a dataframe with its top words and probabilities
returnWTP <- function(topic.index, nwords = 10, topic.word.matrix, topicm = topic.model){
  wtp <- mallet.top.words(topicm, topic.word.matrix[topic.index,], num.top.words = nwords)
  wtp$topic <- paste("Topic", topic.index)
  return(wtp)
}

#given a vector of topic indices,
## the number of words to be displayed per topic,
## and a topic-word matrix (produced with mallet.topic.words())
### produce a plot with word-topic probabilities 
plotWTP <- function(topic.indices, n.words = 10, topic.word.m){
  
  df.words <- lapply(topic.indices, returnWTP, nwords = n.words, topic.word.matrix = topic.word.m)
  df.words <- do.call(rbind, df.words)
  
  #Word-topic probabilities
  plotWTP_bigdiffs <- df.words %>% ggplot(aes(words, weights, fill = factor(topic))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 2) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    coord_flip() +
    ylab("") + xlab("")
  
  return(plotWTP_bigdiffs)
  
}

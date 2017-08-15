#Function to return topics in order of their coherence

source("functions/topic_coherence2.R")

coherence_order <- function(docvector, topicmodel = topic.model, topicwords = topic.words, only_indices = T){

  crps <- quanteda::corpus(docvector)
  dtm <- quanteda::dfm(crps)
  
  topic.words <- mallet::mallet.topic.words(topicmodel)
  df.words <- tibble::tibble()
  for(i in 1:ntopics){
    df.words2 <- tibble::as.tibble(mallet::mallet.top.words(topicmodel, topicwords[i,]))
    df.words2$topic <- str_c("topic_", str_pad(i, 2, pad = "0"))
    df.words <- rbind(df.words, df.words2)
  }
  
  #calculate topic coherence for every topic
  topic.coherence <- sapply(unique(df.words$topic), function(X){
    topic_coherence2(df.words$words[df.words$topic==X], dtm, vocabulary = colnames(dtm))})
  
  topic.coherence <- sort(topic.coherence, decreasing = T)
  
  if(only_indices == T){
    return(as.numeric(stringr::str_replace(names(topic.coherence), "topic_", "")))
  }
  else{
    return(topic.coherence)
  }

}

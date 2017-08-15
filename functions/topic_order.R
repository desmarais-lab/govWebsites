#Function to return topic indices in order of their importance (measured by words in topics)

topic_order <- function(topicmodel = topic.model, only_indices = T){
  #get doc-topic matrix
  doc.topics <- mallet.doc.topics(topicmodel, smoothed = F, normalized = F)
  #how many terms does a topic have across all documents?
  topicSums <- tibble::tibble(value = colSums(doc.topics), index = 1:dim(doc.topics)[2])
  #sort
  topicSums <- topicSums[order(topicSums$value, decreasing = T),]
  
  
  if(only_indices == T){
    return(topicSums$index)
  }
  else{
    return(topicSums)
  }
}
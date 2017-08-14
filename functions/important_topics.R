important_topics <- function(topicmodel, n){
  #get doc-topic matrix
  doc.topics <- mallet.doc.topics(topicmodel, smoothed = F, normalized = F)
  #how many terms does a topic have across all documents?
  topicSums <- colSums(doc.topics)
  #get the n highest
  highestValues <-  sort(topicSums)[1:n]
  #get only n again, in case of a tie
  highestTopics <- which(topicSums%in%highestValues)[1:n]
  
  return(highestTopics)
}
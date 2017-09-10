#Function to calculate entropy between cities for each topic
#based on token assignment

document_entropy <- function(doc.topic.matrix){
  
  #Shannon-Entropy
  entropy <- function(freqs){
    -sum(freqs * log2(freqs))
  }
  
  #calculate entropy for each column of the doc-topic matrix
  return(apply(doc.topic.matrix, 1, entropy))
  
}

#Use:
#document_entropy(mallet.doc.topics(topic.model, smoothed = T, normalized = T))

#city_entropy_order <- function(city_entropy_vector){
#  
#  return(sort.int(city_entropy(doc.topics, d), decreasing = T, index.return = T)$ix)
#  
#}

#Use:
#city_entropy_order(city.entropy(doc.topics, d))

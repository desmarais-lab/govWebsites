#Function to calculate entropy between cities for each topic
#based on token assignment

city_entropy <- function(doc.topic.matrix, doc.data.frame){

  #Calculate token assignments per city
  #The function is called rowsum; BUT it provides
  #"column sums across rows of a numeric matrix-like 
  #object for each level of a grouping variable."
  city.tokens <- rowsum(doc.topic.matrix, group = factor(doc.data.frame$Name))
  city.tokens.prop <- apply(city.tokens, 2, function(X){X/sum(X)})
  
  #add just a little bit to a all values that are equal to zero
  #so that taking their log won't break the entropy algorithm
  city.tokens.prop[city.tokens.prop==0] <- 0.00000000001
  
  #Shannon-Entropy
  entropy <- function(freqs){
   -sum(freqs * log2(freqs))
  }
  
  #calculate entropy for each column of the doc-topic matrix
  return(apply(city.tokens.prop, 2, entropy))

}

#Use:
#city.entropy(doc.topics, d)

city_entropy_order <- function(city_entropy_vector){

  return(sort.int(city_entropy(doc.topics, d), decreasing = T, index.return = T)$ix)
  
}

#Use:
#city_entropy_order(city.entropy(doc.topics, d))

## Topic coherence function from SpeedReader
## Changed to work with sparse doc-topic matrices

topic_coherence2 <- function(top_words,
                            document_term_matrix,
                            vocabulary = NULL,
                            numeric_top_words = FALSE,
                            K = length(top_words)){

  # make sure the data is the right format
  vocabulary <- as.character(vocabulary)
  
  # perform some basic checks and throw errors if we see something weird.
  if(is.null(vocabulary) & !numeric_top_words){
    stop("You must provide a vocabulary vector!")
  }
  if(K > length(top_words)){
    K <- length(top_words)
    warning(paste("You must select a value for K that is less than length(top_words). K has automatically been set to :",K,sep = " "))
  }
  if(length(vocabulary) != ncol(document_term_matrix)){
    stop("The vocaublary vector must have the same number of entries as the number of columns in the document_term_matrix, and the word indicated by entries in the i'th column of document_term_matrix must correspond to the i'th entry in vocabulary.")
  }
  
  #if we are only using the K top words then reduce our top words vector
  top_words <- top_words[1:K]
  
  coherence_score <- 0
  for(i in 2:length(top_words)){
    for(j in 1:(i-1)){
      # we can either look up against vocab or just use indexes
      if(numeric_top_words){
        jindex <- top_words[j]
        iindex <- top_words[i]
      }else{
        jindex <- which(vocabulary == top_words[j])
        iindex <- which(vocabulary == top_words[i])
      }
      
      document_frequency <- length(document_term_matrix[,jindex])
      j_positive <- document_term_matrix[,jindex]@i
      i_positive <- document_term_matrix[,iindex]@i
      co_document_frequency <- sum(i_positive %in% j_positive)
      
      coherence_score <- coherence_score + log((co_document_frequency + 1)/document_frequency)
      
    }
  }
  if(is.infinite(coherence_score)){
    coherence_score <- NA
    warning("The coherence score was not finite. Make sure that all words in your vocabulary appear atleast once.")
  }
  return(coherence_score)

}
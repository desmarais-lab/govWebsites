library('SpeedReader')

#calculate tf-idf for the entire corpus, with words as tokens
dtv <- generate_document_term_vectors(d$doc, tokenization_method = 'RegEx')
dtm <- generate_document_term_matrix(document_term_vector_list = dtv$document_term_vector_list,
                                     document_term_count_list = dtv$document_term_count_list)

#calculate top words
topic.words <- mallet.topic.words(topic.model)
df.words <- tibble()
for(i in 1:ntopics){
  df.words2 <- as.tibble(mallet.top.words(topic.model, topic.words[i,]))
  df.words2$topic <- str_c("topic_", str_pad(i, 2, pad = "0"))
  df.words <- rbind(df.words, df.words2)
}


#calculate topic coherence for every topic
topic.coherence <- sapply(unique(df.words$topic), function(X){
  topic_coherence(df.words$words[df.words$topic==X], dtm, vocabulary = colnames(dtm))})

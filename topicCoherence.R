options(java.parameters="-Xmx3g")   # optional, but more memory for Java helps
library("dfrtopics")
library("mallet")

mallet.instances <- mallet.import(id.array = make.unique(d$Name),
                                  text.array = d$doc,
                                  stoplist.file = "./rfiles/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#train LDA with dfrtopics wrapper
m <- train_model(mallet.instances, 
                 n_topics=100,
                 n_iters=300,
                 seed=1066)

#write diagnostics to file and load them
write_diagnostics(m)
mallet_diag <- read_diagnostics("diagnostics.xml")

#topic coherence
mallet_diag$topics$coherence

#tokens per topic
mallet_diag$topics$tokens

#distribution of topic coherence values
plot(density(mallet_diag$topics$coherence))

#correlations between tokens per topic and topic coherence
plot(mallet_diag$topics$tokens, mallet_diag$topics$coherence)

#100% correlation between number of tokens and mean topic "weight" across documents
plot(mallet_diag$topics$tokens, apply(doc_topics(m), 2, mean))


#in dfrtopics, doc-topic matrix is unnormalized and unsmoothed
#mat1 <- doc_topics(m) %>% normalize_rows()



#check if SpeedReader produces comparable topic coherence results:
topwords <- top_words(m, n=10)

library('SpeedReader')

#calculate tf-idf for the entire corpus, with words as tokens
dtv <- generate_document_term_vectors(d$doc, tokenization_method = 'RegEx')
dtm <- generate_document_term_matrix(document_term_vector_list = dtv$document_term_vector_list,
                                     document_term_count_list = dtv$document_term_count_list)

#calculate topic coherence for every topic
topic.coherence <- sapply(unique(topwords$topic), function(X){
  topic_coherence(topwords$word[topwords$topic==X], dtm, vocabulary = colnames(dtm))})

#plot the two against each other
plot(topic.coherence, mallet_diag$topics$coherence)
#there is some correlation but it's hardly perfect
cor(topic.coherence, mallet_diag$topics$coherence)
#0.5371108
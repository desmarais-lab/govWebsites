library('dplyr')
library('stringr')
library('SpeedReader')

#load data
load(file = str_c("./rfiles/d.Rdata"))

#calculate tf-idf for the entire corpus, with words as tokens
dtv_corpus <- generate_document_term_vectors(d$doc, tokenization_method = 'RegEx')
dtm_corpus <- generate_document_term_matrix(document_term_vector_list = dtv_corpus$document_term_vector_list,
                                     document_term_count_list = dtv_corpus$document_term_count_list)
tf_idf_corpus <- tfidf(dtm_corpus, colnames(dtm_corpus))
#works!


#TRYING to calculate tf-idf for n-grams
#use corenlp


#table(d$Name), but not sorted by name
citytable <- sapply(unique(d$Name), function(x){length(d$doc[d$Name==x])})
ncities <- length(citytable)
citynames <- names(citytable)

for(k in 1:ncities){

  a <- corenlp(d$doc[d$Name==citynames[k]])
  #save(a, file='rfiles/corenlp_attica.rdata')
  #get ngrams from corenlp output
  b <- ngrams(a)
  #save(b, file='rfiles/corenlp_ngrams_attica.rdata')
  
  NGrams <- b
  
  document_term_vector_list <- vector(mode = "list",
                                      length = length(NGrams))
  
  #modified function from SpeedReader package
  for (i in 1:length(NGrams)) {
    num <- 3
    index <- grep(num, names(NGrams[[i]]$ngrams))[1]
    document_term_vector_list[[i]] <- NGrams[[i]]$ngrams[[index]]
  }
  
  #get tf-idf for the corpus of one city
  dtm2 <- generate_document_term_matrix(document_term_vector_list)
  tf_idf2 <- tfidf(dtm2, colnames(dtm2))
  tf_idf3 <- tf_idf2$tfidf_rankings
  
  #the decision for cutting off certain n-grams occurs here
  tf_idf3 <- tf_idf3[order(tf_idf3$idf),]
  tf_idf3 <- tf_idf3[tf_idf3$idf<1.5,]
  tf_idf3$term <- str_replace_all(tf_idf3$term, "_", " ")
  
  citylength <- length(d$doc[d$Name==citynames[k]])

  citystart <- 1
  if(k>1){
    citystart <- sum(citytable[1:(k-1)])+1
  }
  
  for(j in citystart:(citystart+citylength-1)){
    for(i in 1:length(tf_idf3$term)){
      d$doc[j] <- str_c(clean_document_text(d$doc[j], regex = tf_idf3$term[i]), collapse = " ")
    }
  }

}

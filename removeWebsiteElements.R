library('dplyr')
library('stringr')
library('SpeedReader')

#load data
corpus <- "current" #"current", "before", or "after"
#Hunspell
load(file = str_c("./rfiles/docs_", corpus, ".Rdata"))
#remove documents where spellchecking failed
d <- d[d$spell_fail==0,]
#remove non-spellchecked text
d$doc <- d$doc2
d <- select(d, -doc2)


#calculate tf-idf for the entire corpus, with words as tokens
dtv <- generate_document_term_vectors(d$doc[d$Name=="Attica"], tokenization_method = 'RegEx')
dtm <- generate_document_term_matrix(document_term_vector_list = dtv$document_term_vector_list,
                                     document_term_count_list = dtv$document_term_count_list)
tf_idf <- tfidf(dtm, colnames(dtm))
#works!


#TRYING to calculate tf-idf for n-grams
#use corenlp
a <- corenlp(d$doc[d$Name=="Attica"])
save(a, file='rfiles/corenlp_attica.rdata')
#get ngrams from corenlp output
b <- ngrams(a)
save(b, file='rfiles/corenlp_ngrams_attica.rdata')

#get document term vectors
#what is the expected input from the above?
#dtv2 <- generate_document_term_vectors('rfiles/corenlp_ngrams_attica.rdata', 
#                                       data_type = "ngrams", 
#                                       ngram_type = "3_grams")
source("speadReaderFix.R")

dtm2 <- generate_document_term_matrix(document_term_vector_list)
tf_idf2 <- tfidf(dtm2, colnames(dtm2))
tf_idf3 <- tf_idf2$tfidf_rankings
tf_idf3 <- tf_idf3[order(tf_idf3$idf),]
tf_idf3 <- tf_idf3[tf_idf3$idf<1.5,]
tf_idf3$term <- str_replace_all(tf_idf3$term, "_", " ")

d$doc <- gsub("\\s+"," ", d$doc)
d$doc <- str_trim(d$doc, side = "both")

for(i in 1:length(tf_idf3$term)){
d$doc[1] <- str_c(clean_document_text(d$doc[1], regex = tf_idf3$term[i]), collapse = " ")
}


#sapply(tf_idf3$term, function(X){str_c(clean_document_text(d$doc[1], regex = X), collapse = " ")})

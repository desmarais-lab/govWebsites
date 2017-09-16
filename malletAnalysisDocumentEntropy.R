library(ggplot2)
source('malletTraining.R')
source('functions/entropy.R')

#use document entropy function
doc_entr <- document_entropy(mallet.doc.topics(topic.model, smoothed = T, normalized = T))
doc_entr <- as.data.frame(doc_entr)

#density plot of entropy across topics for documents
ggplot(doc_entr, aes(doc_entr)) + geom_density() + xlab('Entropy')
ggsave('paper/figures/document_cross_topic_entropy.pdf', width = 8, height = 6)

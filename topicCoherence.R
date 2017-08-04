#DON'T RUN THIS ON A MACHINE WITH LESS THAN 12GB MEMORY


options(java.parameters = "-Xmx5000m")

library('SpeedReader')
library('mallet')
library('dplyr')
library('tibble')
library('stringr')
library('ggplot2')

#load data
load(file = "./rfiles/d.Rdata")

#get the document-term matrix
dtv <- generate_document_term_vectors(d$doc, tokenization_method = 'RegEx')
dtm <- generate_document_term_matrix(document_term_vector_list = dtv$document_term_vector_list,
                                     document_term_count_list = dtv$document_term_count_list)

#train model
#Import website text
mallet.instances <- mallet.import(id.array = make.unique(d$folder),
                                  text.array = d$doc,
                                  stoplist.file = "./rfiles/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

topicsizes <- seq(5, 50, by = 5)
coherence_values <- vector(mode = "numeric", length = length(topicsizes))
coherence_values <- list()

for(j in 1:length(topicsizes)){
  
  #train LDA
  ntopics <- topicsizes[j]
  topic.model <- MalletLDA(num.topics = ntopics)
  topic.model$loadDocuments(mallet.instances)
  topic.model$train(200)
  topic.model$maximize(10)
  
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
  
  coherence_values <- c(coherence_values, list(topic.coherence))
  
}

save(coherence_values, file = "rfiles/coherence_5_50.rdata")

coh <- as.tibble(unlist(coherence_values))
coh$numtopic <- rep(as.character(topicsizes),
                    topicsizes)
#make the number of topics a factor and sort it accordingly, 
##otherwise ggplot might mess up the order
coh$numtopic <- factor(coh$numtopic, levels = unique(coh$numtopic))

p <- ggplot(coh, aes(factor(numtopic), value)) + 
  labs(title = "", x = "Number of topics", y = "Topic coherence") +
  geom_boxplot() + 
  stat_summary(fun.y = mean, colour = "red", geom = "line", aes(group = 1))  + 
  stat_summary(fun.y = mean, colour = "red",  geom = "point")
p

ggsave(p, file = str_c("./paper/figures/coherence_numberoftopics.pdf"), width = 8, height = 5)

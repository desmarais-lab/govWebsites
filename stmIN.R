library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')

load("rfiles/d.Rdata")

#get the data ready for preprocessing
docsIN <- d$doc
meta <- subset(d, select = c('Name', 'winner'))
processed <- textProcessor(docsIN, metadata = meta)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#the stm package has its own preprocessing function
out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 15)

#Number of topics
numtopics <- 60

#Train the model
stmFit <- stm(documents = out$documents, vocab = out$vocab,
              K = numtopics, prevalence =~ winner,
              max.em.its = 75, data = out$meta,
              init.type = "Spectral")

#Estimate effects from the model outputs
prep <- estimateEffect(1:numtopics ~ winner, stmFit,
                       meta = out$meta, uncertainty = "Global")

#save the results, since training takes some time
save.image('rfiles/stmSession2.RData')

# Analyze results
a <- summary(prep)

#reshape the results into a format that can be used with ggplot
ab <- lapply(a$tables, function(X){X[,4]})
abc <- do.call(cbind, ab)
abc <- as.data.frame(abc)
abc$Variable <- rownames(abc)
abc <- gather(abc, key = topic, value = pvalue, -Variable)
abc$Variable <- str_replace(abc$Variable, "Name", "")
abc$Variable <- str_replace(abc$Variable, "winnerRepublican", "_party (Republican)")

#ggplot heatmap
ggplot(abc, aes(x = topic, y = as.factor(Variable), fill = pvalue)) + 
  geom_tile() +
  scale_y_discrete(limits = rev(levels(as.factor(abc$Variable)))) +
  theme(axis.text.x=element_blank()) +
  labs(x = "Topic", y = "Variable") +
  labs(fill='p-value')
ggsave("paper/figures/stm_results.pdf", width = 8, height = 6)

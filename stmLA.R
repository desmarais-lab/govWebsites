library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')

load("rfiles/dLA.rdata")

#get the data ready for preprocessing
docsLA <- d$doc
meta <- subset(d, select = c('City', 'winner'))
processed <- textProcessor(docsLA, metadata = meta)
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
save.image('rfiles/stmSessionLA_Party.RData')
load('rfiles/stmSessionLA_Party.RData')

# Analyze results
a <- summary(prep)

#coefficient is in Republican direction
coefs <- data.frame(topic = 1:60, coefficient = 0, pval = 0)
for(i in 1:nrow(coefs)){
  coefs$coefficient[i] <- a$tables[[i]][2]
  coefs$pval[i] <- a$tables[[i]][8]
}

#order by coefficient size
coefs <- coefs[order(coefs$coefficient, decreasing = T),]

#only keep coefficients that are statistically significant at 95% level
coefs <- coefs[coefs$pval<0.05,]

#most Republican/Democratic topics, determined by coefficient size
repTopics <- coefs$topic[1:5]
demTopics <- coefs$topic[(nrow(coefs)-4):nrow(coefs)]

#topicLabels
repWords <- labelTopics(stmFit, n = 10)
repWords <- repWords$prob[repTopics,]
repWords <- as.vector(repWords)

demWords <- labelTopics(stmFit, n = 10)
demWords <- demWords$prob[demTopics,]
demWords <- as.vector(demWords)

library("xtable")

xt <- data.frame(Democratic = demWords, Republican = repWords)

xt <- xtable(xt,
             caption = "Top 50 Democratic and Republican words (Louisiana), according to STM. 
             The words are the top words for the most Democratic/Republican topic, determined
             by the size (and significance) of the coefficient of the party covariate.",
             label = "tabSTMLA")

xt <- print.xtable(xt, 
                   include.rownames = F,
                   size = "\\fontsize{9pt}{10pt}\\selectfont")

writeLines(xt, 
           con = 'paper/tables/stmTopWordsLA.tex')


#reshape the results into a format that can be used with ggplot
#ab <- lapply(a$tables, function(X){X[,4]})
#abc <- do.call(cbind, ab)
#abc <- as.data.frame(abc)
#abc$Variable <- rownames(abc)
#abc <- gather(abc, key = topic, value = pvalue, -Variable)
#abc$Variable <- str_replace(abc$Variable, "Name", "")
#abc$Variable <- str_replace(abc$Variable, "winnerRepublican", "_party (Republican)")

#ggplot heatmap
#ggplot(abc, aes(x = topic, y = as.factor(Variable), fill = pvalue)) + 
#  geom_tile() +
#  scale_y_discrete(limits = rev(levels(as.factor(abc$Variable)))) +
#  theme(axis.text.x=element_blank()) +
#  labs(x = "Topic", y = "Variable") +
#  labs(fill='p-value')
#ggsave("paper/figures/stm_results.pdf", width = 8, height = 6)

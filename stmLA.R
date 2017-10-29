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
                       meta = out$meta, uncertainty = "Global",
                       nsims = 500)

#save the results, since training takes some time
rm(list = ls()[!ls()%in% c("stmFit", "prep", "out", "numtopics")])
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


#Credible interval

#1. Take 1000 draws from the posterior
#2. Decide on a level of confidence, here 1%
#3. For each topic, from the 1000 estimated coefficients, take the ones
## lying between its 0.005 and .995 quantile
#4. If this range of values does not include 0, it the coefficient is statistically significant
#5. Take the mean from these values to get the estimated coefficient?

sims <- 1000

#re-estimate the effects, this time do 1000 draws from the posterior
prep <- estimateEffect(formula = 1:numtopics ~ winner, 
                       stmobj = stmFit,
                       meta = out$meta, 
                       uncertainty = "Global",
                       nsims = sims)

#make a dataframe to store the results in
df <- data.frame(coef = rep(0, numtopics), sig = rep(FALSE, numtopics),
                 lower = rep(0, numtopics), upper = rep(0, numtopics),
                 topic = 1:numtopics)

conf_level <- 1 #in percent
conf_level <- conf_level/100 #as a fraction
conf_level <- conf_level/2 #two-sided

conf_lower <- 0 + conf_level
conf_upper <- 1 - conf_level

for(j in 1:numtopics){
  
  coefs <- list()
  
  for(i in 1:sims){
    coefs[[i]] <- prep$parameters[[j]][[i]]$est[2]
  }
  
  coefs <- do.call(c, coefs)
  coefs <- quantile(coefs, c(conf_lower, conf_upper))
  
  df$lower[j] <- coefs[1]
  df$upper[j] <- coefs[2]
  df$sig[j] <- all(coefs>0) | all(coefs<0)
  df$coef[j] <- mean(coefs)
  
}

#number of significant topics
sum(df$sig)

#Get the indices of the highest/lowest values
#whichMinMax <- function(x, n = 5, minmax = "max"){
#  dframe <- data.frame(x = x, ind = 1:length(x))
#  if(minmax == "max"){
#    dframe <- dframe[order(dframe$x, decreasing = T),]
#    return(dframe$ind[1:n])
#  }
#  if(minmax == "min"){
#    dframe <- dframe[order(dframe$x, decreasing = F),]
#    return(dframe$ind[1:n])
#  }
#}

#sort data frame by coefficient size
df <- df[order(df$coef, decreasing = T),]

#get the 5 topics with the highest/lowest significant coefficients
repTopics <- df$topic[df$sig==T][1:5]
demTopics <- rev(df$topic[df$sig==T][(nrow(df[df$sig==T,])-4):nrow(df[df$sig==T,])])

#sort data frame by topic number
df <- df[order(df$topic, decreasing = F),]

#number of top words to be shown
nTopWords <- 10

#topicLabels
repWords <- labelTopics(stmFit, n = nTopWords)
repWords <- repWords$prob[repTopics,]
repWords <- c(apply(repWords, 1, c))

demWords <- labelTopics(stmFit, n = nTopWords)
demWords <- demWords$prob[demTopics,]
demWords <- c(apply(demWords, 1, c))

#create data frame for xtable
xt <- data.frame(demTopic = rep(demTopics, each = nTopWords),
                 demTopicCoeff = round(rep(df$coef[demTopics], each = nTopWords), 3),
                 Democratic = demWords,
                 repTopic = rep(repTopics, each = nTopWords),
                 repTopicCoeff = round(rep(df$coef[repTopics], each = nTopWords), 3),
                 Republican = repWords)

#save results
save.image('rfiles/stmLA.RData')

library("xtable")

xt <- xtable(xt,
             caption = "Top 50 Democratic and Republican words (Louisiana), according to STM. 
             The words are the top words for the most Democratic/Republican topic, determined
             by the size (and significance) of the coefficient of the party covariate.",
             label = "tabSTMLA",
             digits = 3)

xt <- print.xtable(xt, 
                   include.rownames = F,
                   size = "\\fontsize{9pt}{10pt}\\selectfont")

writeLines(xt, 
           con = 'paper/tables/stmTopWordsLA.tex')

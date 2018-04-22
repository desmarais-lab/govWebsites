library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')

load(file = "./rfiles/websiteMetadata.rdata")
load(file = "rfiles/allDocuments.rdata")

d <- merge(d, websiteMeta, by.x = "city", by.y = "State_City")
d <- subset(d, State %in% c("Indiana", "Louisiana", "New York", "California", "Washington", "Texas"))

#get the data ready for preprocessing
docs <- d$text
meta <- subset(d, select = c(city, party, State, B01001_001E, B19013_001E))
processed <- textProcessor(docs, metadata = meta, 
                           lowercase = F,
                           removestopwords = F, 
                           removenumbers = F, 
                           removepunctuation = F,
                           stem = F, 
                           wordLengths = c(1, Inf))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = NULL)

#Number of topics
numtopics <- 60

#Train the model
stmFit <- stm(documents = out$documents, vocab = out$vocab,
              K = numtopics, prevalence =~ party + State + B01001_001E + B19013_001E,
              max.em.its = 9999, data = out$meta,
              init.type = "Spectral")

#Estimate effects from the model outputs
sims <- 1000

#re-estimate the effects, this time do 1000 draws from the posterior
prep <- estimateEffect(formula = 1:numtopics ~ party + State + B01001_001E + B19013_001E, 
                       stmobj = stmFit,
                       meta = out$meta, 
                       uncertainty = "Global",
                       nsims = sims)

#save the results, since training takes some time
#the results are so large, we only retain the
#objects necessary for the rest of the analysis
#rm(list = ls()[!ls()%in% c("stmFit", "prep", "out", "numtopics")])
save.image("rfiles/stmSession2, _Party.rdata")
load("rfiles/stmSession2, _Party.rdata")

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

#sort data frame by coefficient size
df <- df[order(df$coef, decreasing = T),]

#get the topics with the highest/lowest significant coefficients
tableTopicNum <- 8
repTopics <- df$topic[df$sig==T][1:tableTopicNum]
demTopics <- rev(df$topic[df$sig==T][(nrow(df[df$sig==T,])-(tableTopicNum-1)):nrow(df[df$sig==T,])])

#sort data frame by topic number
df <- df[order(df$topic, decreasing = F),]

#number of top words to be shown
nTopWords <- 10

#topicLabels
repWords <- labelTopics(stmFit, n = nTopWords)
repWords <- repWords$prob[repTopics,]
repWords <- t(repWords)
colnames(repWords) <- round(df$coef[repTopics], 3)

demWords <- labelTopics(stmFit, n = nTopWords)
demWords <- demWords$prob[demTopics,]
demWords <- t(demWords)
colnames(demWords) <- round(df$coef[demTopics], 3)

xtRep <- repWords
xtDem <- demWords

strCaptionRep <- "Top Republican topics and words, according to STM. 
The words are the top words for the most Democratic/Republican topic, determined
by the size (and significance) of the coefficient (see table header) of the party covariate."

strCaptionDem <- "Top Democratic topics and words, according to STM. 
The words are the top words for the most Democratic/Republican topic, determined
by the size (and significance) of the coefficient (see table header) of the party covariate."

xtRep <- print(xtable(repWords,
                      digits = 3, 
                      caption = strCaptionRep, 
                      label = paste0("tabSTM_Rep")),
                      #size = "footnotesize",
                      include.rownames = FALSE)

xtDem <- print(xtable(demWords,
                      digits = 3, 
                      caption = strCaptionDem, 
                      label = paste0("tabSTM_Dem")),
                      #size = "footnotesize",
                      include.rownames = FALSE)

writeLines(xtRep, con = paste0('paper/tables/stmTopWords_Rep.tex'))
writeLines(xtDem, con = paste0('paper/tables/stmTopWords_Dem.tex'))

#save results
save.image(paste0('rfiles/stm.rdata'))

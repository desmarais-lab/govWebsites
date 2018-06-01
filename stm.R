library('quanteda')
library('stm')
library('tidyr')
library('ggplot2')
library('scales')
library('stringr')

load(file = "./rfiles/websiteMetadata.rdata")
load(file = "rfiles/allDocuments.rdata")

d <- merge(d, websiteMeta, by.x = "city", by.y = "State_City")
d <- subset(d, State %in% c("Indiana", "Louisiana", "New York", "California", "Washington", "Texas"))

d$doc_id <- paste("doc", 1:nrow(d))
crps <- corpus(d)
dtm <- dfm(crps)

#Number of topics
numtopics <- 60

#Train the model
stmFit <- stm(documents = dtm,
              K = numtopics, 
              prevalence =~ party + State + B01001_001E + B19013_001E,
              max.em.its = 9999, 
              #data = out$meta,
              init.type = "Spectral")

#save.image("rfiles/stmSession2, _Party.rdata")
save.image("rfiles/stmSession_model.rdata")

#Estimate effects from the model outputs
sims <- 1000

#re-estimate the effects, this time do 1000 draws from the posterior
prep <- estimateEffect(formula = 1:numtopics ~ party + State + B01001_001E + B19013_001E, 
                       stmobj = stmFit,
                       meta = d, 
                       uncertainty = "Global",
                       nsims = sims)

#save.image("rfiles/stmSession3.rdata")
save.image("rfiles/stmSession_sim.rdata")

#make a dataframe to store the results in
# df <- data.frame(coef = rep(0, numtopics), sig = rep(FALSE, numtopics),
#                  lower = rep(0, numtopics), upper = rep(0, numtopics),
#                  topic = 1:numtopics)
# 
# conf_level <- 1 #in percent
# conf_level <- conf_level/100 #as a fraction
# conf_level <- conf_level/2 #two-sided
# 
# conf_lower <- 0 + conf_level
# conf_upper <- 1 - conf_level
# 
# for(j in 1:numtopics){
#   
#   coefs <- list()
#   
#   for(i in 1:sims){
#     coefs[[i]] <- prep$parameters[[j]][[i]]$est[2]
#   }
#   
#   coefs <- do.call(c, coefs)
#   coefs <- quantile(coefs, c(conf_lower, conf_upper))
#   
#   df$lower[j] <- coefs[1]
#   df$upper[j] <- coefs[2]
#   df$sig[j] <- all(coefs>0) | all(coefs<0)
#   df$coef[j] <- mean(coefs)
#   
# }
# 
# #number of significant topics
# sum(df$sig)
# 
# #sort data frame by coefficient size
# df <- df[order(df$coef, decreasing = T),]
# 
# #get the topics with the highest/lowest significant coefficients
# tableTopicNum <- 8
# repTopics <- df$topic[df$sig==T][1:tableTopicNum]
# demTopics <- rev(df$topic[df$sig==T][(nrow(df[df$sig==T,])-(tableTopicNum-1)):nrow(df[df$sig==T,])])
# 
# #sort data frame by topic number
# df <- df[order(df$topic, decreasing = F),]
# 
# #number of top words to be shown
# nTopWords <- 10
# 
# #topicLabels
# repWords <- labelTopics(stmFit, n = nTopWords)
# repWords <- repWords$prob[repTopics,]
# repWords <- t(repWords)
# colnames(repWords) <- round(df$coef[repTopics], 3)
# 
# demWords <- labelTopics(stmFit, n = nTopWords)
# demWords <- demWords$prob[demTopics,]
# demWords <- t(demWords)
# colnames(demWords) <- round(df$coef[demTopics], 3)
# 
# xtRep <- repWords
# xtDem <- demWords
library('quanteda')
library('mlr')

#load data
load(file = "./rfiles/d.Rdata")

#create corpus
crps <- corpus(d$doc)

#set party docvar
#docvars(crps, "Party") <- d$winner
#docvars(crps, "City") <- d$Name

#convert to dfm
dfmIN <- dfm(crps)

#calculate tf-idf
#tf_idf <- quanteda::tfidf(dfmIN)
#tf_idf <- as.data.frame(tf_idf)
#tf_idf <- cbind(PARTY = as.factor(d$winner), tf_idf)

#task <- makeClassifTask(data = tf_idf, target = 'PARTY')

## Generate the learner
#lrn = makeLearner("classif.lda")

## Train the learner
#mod = train(lrn, task)
#mod

#-----------------------------------------------------------------

#dtm as data frame
dtm <- as.data.frame(dfmIN)
dtm <- cbind(PARTY = as.factor(d$winner), dtm)
rm(list = ls()[ls()!="dtm"])

task <- makeClassifTask(data = dtm, target = 'PARTY')

## Generate the learner
lrn = makeLearner("classif.svm")

## Train the learner
mod = train(lrn, task)

save.image(file = 'rfiles/svmMLRmod.Rdata')
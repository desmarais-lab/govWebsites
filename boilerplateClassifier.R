library(stargazer)
library(quanteda)

trainingData <- read.csv("data/classifierTrainingDataCoded.csv")
trainingData$linesRemove <- as.character(trainingData$linesRemove)
trainingData$nchar <- nchar(trainingData$linesRemove)
#count the number of tokens
tks <- tokens(trainingData$linesRemove)
trainingData$nwords <- unlist(lapply(tks, length))
rm(tks)
names(trainingData) <- c("id", "text", "freq", "class", "city", "prob", "medianDocMidDist", "nchars", "nwords")
#

# Classifier

#new model

trainingData$folds <- sample(rep(1:5, 100), 500, replace = F)
crossval_results <- list()
for(i in 1:5){
  m1 <- glm(class ~ nchars + nwords + medianDocMidDist + prob + freq, data = trainingData[trainingData$folds!=i,])
  #summary(m1)
  
  #true values
  true0id <- which(trainingData$class[trainingData$folds==i]==0)
  true1id <- which(trainingData$class[trainingData$folds==i]==1)
  
  #predicted
  predProbs <- predict(m1, trainingData[trainingData$folds==i,])
  pred1id <- which(predProbs>=0.5)
  pred0id <- which(predProbs<=0.5)
  
  #percent correctly predicted
  pcp <- mean(as.logical(predProbs>=0.5) == as.logical(trainingData$class[trainingData$folds==i]==1))
  #true positives
  tp <- length(which(pred1id%in%true1id))
  #false positives
  fp <- length(which(pred1id%in%true0id))
  #false negatives
  fn <- length(which(pred0id%in%true1id))
  #precision
  prec <- tp/(tp+fp)
  #recall
  rec <- tp/(tp+fn)
  #f1 score
  f1 <- 2*((prec*rec)/(prec+rec))
  #add to results list
  crossval_results[[i]] <- c(pcp, prec, rec, f1)
  names(crossval_results[[i]]) <- c("pcp", "prec", "rec", "f1")
}
apply(do.call(rbind, crossval_results), 2, mean)
stargazer(m1)
writeLines(stargazer(m1), "paper/tables/boilerplateClassifier1.tex")

write.csv(trainingData, "data/classifierTrainingDataCoded2.csv", row.names = F)


# random forest
library(randomForest)
trainingDataRF <- trainingData[,c('freq', 'prob', 'medianDocMidDist', 'nchars', 'nwords')]

#mRF <- randomForest(x = trainingDataRF[trainingData$folds!=1,], y = as.factor(trainingData$class[trainingData$folds!=1]))
#predict(mRF, trainingDataRF[trainingData$folds==1,])


crossval_results <- list()
for(i in 1:5){
  mRF <- randomForest(x = trainingDataRF[trainingData$folds!=i,], y = as.factor(trainingData$class[trainingData$folds!=i]))
  #summary(m1)
  
  #true values
  true0id <- which(trainingData$class[trainingData$folds==i]==0)
  true1id <- which(trainingData$class[trainingData$folds==i]==1)
  
  #predicted
  predClasses <- predict(mRF, trainingDataRF[trainingData$folds==i,])
  pred1id <- which(predClasses==1)
  pred0id <- which(predClasses==0)
  
  #percent correctly predicted
  pcp <- mean(as.logical(predClasses==1) == as.logical(trainingData$class[trainingData$folds==i]==1))
  #true positives
  tp <- length(which(pred1id%in%true1id))
  #false positives
  fp <- length(which(pred1id%in%true0id))
  #false negatives
  fn <- length(which(pred0id%in%true1id))
  #precision
  prec <- tp/(tp+fp)
  #recall
  rec <- tp/(tp+fn)
  #f1 score
  f1 <- 2*((prec*rec)/(prec+rec))
  #add to results list
  crossval_results[[i]] <- c(pcp, prec, rec, f1)
  names(crossval_results[[i]]) <- c("pcp", "prec", "rec", "f1")
}
apply(do.call(rbind, crossval_results), 2, mean)

importance(mRF)

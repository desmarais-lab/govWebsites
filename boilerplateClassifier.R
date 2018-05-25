library(stargazer)
library(quanteda)
library(xtable)

trainingData <- read.csv("data/classifierTrainingDataCoded.csv")
trainingData$linesRemove <- as.character(trainingData$linesRemove)
trainingData$nchar <- nchar(trainingData$linesRemove)
#count the number of tokens
tks <- tokens(trainingData$linesRemove)
trainingData$nwords <- unlist(lapply(tks, length))
rm(tks)
names(trainingData) <- c("id", "text", "freq", "class", "city", "prob", "medianDocMidDist", "nchars", "nwords")
#

# Logit Classifier

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
metrics_m1 <- round(apply(do.call(rbind, crossval_results), 2, mean), 3)
sg_m1 <- stargazer(m1, add.lines = list(c("Percent Correctly Predicted", metrics_m1[1]),
                               c("Precision", metrics_m1[2]),
                               c("Recall", metrics_m1[3]),
                               c("F1-Score", metrics_m1[4])))
writeLines(sg_m1, "paper/tables/boilerplateClassifier1.tex")

write.csv(trainingData, "data/classifierTrainingDataCoded2.csv", row.names = F)
trainingData <- read.csv("data/classifierTrainingDataCoded2.csv")

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


# random forest with inverse probability weighting

cityLineInfo <- read.csv("data/cityLines.txt", header = F)
names(cityLineInfo) <- c("city", "nlines", "nlinetypes")
trainingData <- merge(trainingData, cityLineInfo, by = "city")
trainingData$ipw <- 1/((trainingData$nlines/sum(cityLineInfo$nlines))*(trainingData$freq/trainingData$nlines))

library(caret)
library(ranger)

trainingDataRF <- trainingData[,c('freq', 'prob', 'ipw', 'medianDocMidDist', 'nchars', 'nwords', 'class')]
trainingDataRF$class <- as.factor(trainingDataRF$class)

#train_control <- trainControl(method="cv", number=10)

crossval_results <- list()
for(i in 1:5){
  
  mRF <- train(class ~ freq + medianDocMidDist + nchars + nwords, 
               data = trainingDataRF[trainingData$folds!=i,], 
               method = "ranger",
               weights = ipw,
               #trControl=train_control,
               importance = 'impurity')
  
  #true values
  true0id <- which(trainingData$class[trainingData$folds==i]==0)
  true1id <- which(trainingData$class[trainingData$folds==i]==1)
  
  #predicted
  predClasses <- predict(mRF, trainingDataRF[trainingData$folds==i,])
  pred1id <- which(predClasses==1)
  pred0id <- which(predClasses==0)
  
  #percent correctly predicted
  true_weight <- sum(trainingDataRF$ipw_scaled[trainingData$folds==i][as.logical(predClasses==1) == as.logical(trainingData$class[trainingData$folds==i]==1)])
  false_weight <- sum(trainingDataRF$ipw_scaled[trainingData$folds==i][as.logical(predClasses==1) != as.logical(trainingData$class[trainingData$folds==i]==1)])
  pcp <- true_weight/(true_weight+false_weight)
  #true positives
  tp <- sum(trainingDataRF$ipw_scaled[trainingData$folds==i][which(pred1id%in%true1id)])
  #false positives
  fp <- sum(trainingDataRF$ipw_scaled[trainingData$folds==i][which(pred1id%in%true0id)])
  #false negatives
  fn <- sum(trainingDataRF$ipw_scaled[trainingData$folds==i][which(pred0id%in%true1id)])
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

metrics_mRF <- data.frame(round(apply(do.call(rbind, crossval_results), 2, mean), 3))
names(metrics_mRF) <- "Value"
rownames(metrics_mRF) <- c("Percent Correctly Predicted", "Precision", "Recall", "F1-Score")
xt <- print.xtable(xtable(metrics_mRF,
                          label = "randomForest",
                          caption = "Performance metrics for random forest boilerplate classifier, with inverse probability weights."),
                   include.rownames = T)
writeLines(xt, 
           con = "paper/tables/boilerplateClassifierRFMetrics.tex")

#Comparison
# metrics_mRFTab <- metrics_mRF
# metrics_mRFTab <- cbind(metrics_mRFTab,metrics_mRF)
# names(metrics_mRFTab) <- c("IPW", "Prob", "No Weight")
# xt <- print.xtable(xtable(metrics_mRFTab,
#                           caption = "Performance metrics for random forest boilerplate classifier, with either Inverse Probability Weights (IPW), probabilty (of being in the training sample) (prob), or no weighting."),
#                    include.rownames = T)
# writeLines(xt,
#            con = "paper/tables/boilerplateClassifierRFMetrics.tex")

# export variable importance as a latex table
varimp <- varImp(mRF)$importance
varimp <- data.frame(Feature = rownames(varimp), Importance = round(varimp$Overall, 1))
varimp <- varimp[order(varimp$Importance, decreasing  = T),]
xt <- print.xtable(xtable(varimp,
                          label = "randomForestVarimp",
                          caption = "Variable importance for random forest boilerplate classifier, with IPW weighting."),
                   include.rownames = F)
writeLines(xt, 
           con = "paper/tables/boilerplateClassifierRFImportance.tex")

save(mRF, file = "rfiles/boilerplateClassifierWeights.rdata")

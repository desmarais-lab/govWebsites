#Purpose of the script:
#Train a random forest with ipw weighting on 500 hand-coded samples from 5 cities
#5-fold cross-validation with a 400/100 split
#Create a final model from the full sample of 500 and save the weights for later use

library("stargazer")
library("quanteda")
library("xtable")
library("caret")
library("ranger")
library("data.table")

set.seed(1)

#Prepare the training data
trainingData <- fread("data/classifierTrainingDataCoded.csv")
#convert lines to character
trainingData$linesRemove <- as.character(trainingData$linesRemove)
#count the number of characters per line
trainingData$nchar <- nchar(trainingData$linesRemove)
#count the number of tokens
tks <- tokens(trainingData$linesRemove)
trainingData$nwords <- unlist(lapply(tks, length))
rm(tks)
#rename the important variables
names(trainingData) <- c("id", "text", "freq", "class", "city", "prob", "medianDocMidDist", "nchars", "nwords")
#add a variable indicating the fold of a sample
trainingData$folds <- sample(rep(1:5, 100), 500, replace = F)
write.csv(trainingData, "data/classifierTrainingDataCoded2.csv", row.names = F)

#Add inverse probability weights to the dataset
cityLineInfo <- read.csv("data/cityLines.txt", header = F)
names(cityLineInfo) <- c("city", "nlines", "nlinetypes")
trainingData <- merge(trainingData, cityLineInfo, by = "city")
#add inverse probability weights
trainingData$ipw <- 1/((trainingData$nlines/sum(cityLineInfo$nlines))*(trainingData$freq/trainingData$nlines))

#keep only the important variables
trainingDataRF <- trainingData[,c('freq', 'prob', 'ipw', 'medianDocMidDist', 'nchars', 'nwords', 'class')]
#for a binary outcome variable, the caret package requires it to be a factor
trainingDataRF$class <- as.factor(trainingDataRF$class)
levels(trainingDataRF$class) <- c("substantive", "boilerplate")


## Random forest with inverse probability weighting:

#5-fold cross validation
crossval_results <- list()
for(i in 1:5){
  
  mRF <- train(class ~ freq + medianDocMidDist + nchars + nwords, 
               data = trainingDataRF[trainingData$folds!=i,], 
               method = "ranger",
               weights = ipw,
               trControl = trainControl(classProbs=TRUE),
               importance = 'impurity')
  
  #true values
  true0id <- which(trainingData$class[trainingData$folds==i]==0)
  true1id <- which(trainingData$class[trainingData$folds==i]==1)
  
  #predicted
  predClasses <- predict(mRF, trainingDataRF[trainingData$folds==i,])
  pred1id <- which(predClasses=="boilerplate")
  pred0id <- which(predClasses=="substantive")
  
  #percent correctly predicted
  true_weight <- sum(trainingDataRF$ipw[trainingData$folds==i][as.logical(predClasses=="boilerplate") == as.logical(trainingData$class[trainingData$folds==i]==1)])
  false_weight <- sum(trainingDataRF$ipw[trainingData$folds==i][as.logical(predClasses=="boilerplate") != as.logical(trainingData$class[trainingData$folds==i]==1)])
  pcp <- true_weight/(true_weight+false_weight)
  #true positives
  tp <- sum(trainingDataRF$ipw[trainingData$folds==i][which(pred1id%in%true1id)])
  #false positives
  fp <- sum(trainingDataRF$ipw[trainingData$folds==i][which(pred1id%in%true0id)])
  #false negatives
  fn <- sum(trainingDataRF$ipw[trainingData$folds==i][which(pred0id%in%true1id)])
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

#Print performance metrics in a table
metrics_mRF <- data.frame(round(apply(do.call(rbind, crossval_results), 2, mean), 3))
names(metrics_mRF) <- "Value"
rownames(metrics_mRF) <- c("Percent Correctly Predicted", "Precision", "Recall", "F1-Score")
xt <- print.xtable(xtable(metrics_mRF,
                          label = "randomForest",
                          caption = "Performance metrics for random forest boilerplate classifier, with inverse probability weights."),
                   include.rownames = T)
writeLines(xt, 
           con = "paper/tables/boilerplateClassifierRFMetrics.tex")

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



#Train again, this time for the full training data
#Then save the weights for future use

mRF <- train(class ~ freq + medianDocMidDist + nchars + nwords, 
             data = trainingDataRF,
             method = "ranger",
             weights = ipw,
             trControl = trainControl(classProbs=TRUE),
             importance = 'impurity')

save(mRF, file = "rfiles/boilerplateClassifierWeights.rdata")



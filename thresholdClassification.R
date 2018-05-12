testDF <- data.frame(linesRemove)

test1_4 <- as.character(sample(test$linesRemove[test$Freq>=1 & test$Freq<=4], 100, F))
test5_9 <- as.character(sample(test$linesRemove[test$Freq>=5 & test$Freq<=9], 100, F))
test10_19 <- as.character(sample(test$linesRemove[test$Freq>=10 & test$Freq<=19], 100, F))
test20_39 <- as.character(sample(test$linesRemove[test$Freq>=20 & test$Freq<=39], 100, F))
test40 <- as.character(sample(test$linesRemove[test$Freq>=40], 100, F))

test <- c(test1_4, test5_9, test10_19, test20_39, test40)
test <- tibble::tibble(text = test, number = rep(c("1-4", "5-9", "10-19", "20-39", "40plus"), each = 100))

test.ind <- sample(c(1:500), 500, F)

test.text <- test$text[test.ind]
test.number <- test$number[test.ind]
test.text <- as.data.frame(test.text)

test.text$class <- NA

test.text.class <- c(F,T,T,F,T,F,F,F,T,T,F,F,T,F,F,F,T,F,F,F,F,F,F,F,F,F,T,F,T,F,F,F,T,T,F,F,F,T,F,F,F,F,F,T,T,T,T,F,F,T,T,F,T,F,T,F,F,T,T,F,F,T,T,F,T,F,T,F,T,T,T,T,T,T,T,T,F,T,F,F,T,F,T,F,F,F,F,T,F,T,T,T,T,F,T,T,F,T,T,T)

library(ggplot2)

results <- tibble::tibble(number = test.number[1:100], substantive = test.text.class)

ggplot(results, aes(x = substantive, color = number)) + geom_bar()
ggsave("../Dropbox/govWebsitesMovingVan/numberDuplicate_substantive.pdf")

results2 <- tibble::tibble(textlength = nchar(as.character(test.text$test.text[1:100])), substantive = test.text.class)

ggplot(results2, aes(x = substantive, y = textlength)) + geom_point()
ggsave("../Dropbox/govWebsitesMovingVan/nchar_substantive.pdf")

results3 <- cbind(results, results2)
results3 <- results3[,1:3]
results3$text <- as.character(test.text$test.text[1:100])
results3$realDuplicate <- NA
for(i in 1:100){
results3$realDuplicate[i] <- testDF$Freq[which(results3$text[i]==testDF$linesRemove)]
}

m1 <- glm(substantive ~ textlength + realDuplicate + textlength*realDuplicate, data = results3[1:70,])
summary(m1)

m1 <- glm(substantive ~ textlength + number + textlength*number, data = results3[1:70,])
summary(m1)

mean((predict(m1, results3[71:100,])>0.5)==results3$substantive[71:100])

save.image(file = "../Dropbox/govWebsitesMovingVan/thresholdClassification.rdata")

# add keyness and word number

library(stringr)
library(quanteda)
library(stargazer)

tks <- tokens(results3$text)
results3$ntokens <- unlist(lapply(tks, length))

load("./../Dropbox/govWebsitesMovingVan/Alaska_Anchorage.rdata")

keyness$pos <- 0
keyness$pos[keyness$chi2>0] <- 1
keyness$chi22 <- abs(keyness$chi2)
keyness$chi22 <- log(keyness$chi22)
keyness$chi22[keyness$pos == 0] <- -keyness$chi22[keyness$pos == 0]
keyness$chi2 <- keyness$chi22

lineKeyness <- list()
for(i in 1:nrow(results3)){
  tokenKeyness <- list()
  for(j in 1:length(tks[[i]])){
    tokenKeyness[[j]] <- keyness$chi2[which(tks[[i]][j]==rownames(keyness))]
  }
  lineKeyness[[i]] <- unlist(tokenKeyness)
}

results3$lineKeyness <- unlist(lapply(lineKeyness, mean))

#new model

results3$folds <- sample(rep(1:5,20), 100, replace = F)
crossval_results <- list()
for(i in 1:5){
  m1 <- glm(substantive ~ textlength + realDuplicate + ntokens + lineKeyness, data = results3[results3$folds!=i,])
  #summary(m1)
  #percent correctly predicted
  pcp <- mean((predict(m1, results3[results3$folds!=i,])>=0.5)==results3$substantive[results3$folds!=i])
  #true positives
  tp <- length(which(which(predict(m1, results3[results3$folds!=i,])>=0.5)%in%which(results3$substantive[results3$folds!=i])))
  #false positives
  fp <- length(which(which(predict(m1, results3[results3$folds!=i,])>=0.5)%in%which(results3$substantive[results3$folds!=i])==F))
  #false negatives
  fn <- length(which(which(predict(m1, results3[results3$folds!=i,])<0.5)%in%which(results3$substantive[results3$folds!=i])))
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

#reduced model
crossval_results <- list()
for(i in 1:5){
  m2 <- glm(substantive ~ textlength, data = results3[results3$folds!=i,])
  #percent correctly predicted
  pcp <- mean((predict(m2, results3[results3$folds!=i,])>=0.5)==results3$substantive[results3$folds!=i])
  #true positives
  tp <- length(which(which(predict(m2, results3[results3$folds!=i,])>=0.5)%in%which(results3$substantive[results3$folds!=i])))
  #false positives
  fp <- length(which(which(predict(m2, results3[results3$folds!=i,])>=0.5)%in%which(results3$substantive[results3$folds!=i])==F))
  #false negatives
  fn <- length(which(which(predict(m2, results3[results3$folds!=i,])<0.5)%in%which(results3$substantive[results3$folds!=i])))
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
stargazer(m2)
writeLines(stargazer(m2), "paper/tables/boilerplateClassifier2.tex")

crossval_results <- list()
for(i in 1:5){
  m3 <- glm(substantive ~ textlength + realDuplicate + ntokens + lineKeyness + textlength*realDuplicate + textlength*ntokens + textlength*lineKeyness + realDuplicate*ntokens + realDuplicate*lineKeyness + ntokens*lineKeyness, data = results3[results3$folds!=i,])
  #percent correctly predicted
  pcp <- mean((predict(m3, results3[results3$folds!=i,])>=0.5)==results3$substantive[results3$folds!=i])
  #true positives
  tp <- length(which(which(predict(m3, results3[results3$folds!=i,])>=0.5)%in%which(results3$substantive[results3$folds!=i])))
  #false positives
  fp <- length(which(which(predict(m3, results3[results3$folds!=i,])>=0.5)%in%which(results3$substantive[results3$folds!=i])==F))
  #false negatives
  fn <- length(which(which(predict(m3, results3[results3$folds!=i,])<0.5)%in%which(results3$substantive[results3$folds!=i])))
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
writeLines(stargazer(m3), "paper/tables/boilerplateClassifier3.tex")


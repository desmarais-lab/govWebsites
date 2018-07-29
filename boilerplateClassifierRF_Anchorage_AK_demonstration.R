#Purpose of the script: 
#Illustrate the effectiveness of the classifier by:
#1. Comparing the lines left over after it is applied to every line, from before
#2. Creating a table of the lines with the highest and lowest probabilities of being boilerplate
#Both are done on the corpus of Anchorage, AK.

library('stringr')
library('caret')
library('ranger')
library('quanteda')
library('xtable')

set.seed(1)

# ----

f <- list.files("rfiles/city_chunks_unprocessed", full.names = T)
f_file <- list.files("rfiles/city_chunks_unprocessed")

#load the data for Anchroage
city = 1
  
load(f[city])
a <- a[!a$text=="",]
a <- a[!a$text==" ",]

load(file = paste("rfiles/city_chunks_part1_processed/", f_file[city], sep = ""))

#Preprocessing until the point where the classifier comes in
tt3 <- unlist(tt2)

#create the new X'
testData <- data.frame(text = tt3, stringsAsFactors = F)
testDataTextTable <- data.frame(table(testData$text), stringsAsFactors = F)
testDataTextTable$Var1 <- as.character(testDataTextTable$Var1)
testData$lineID <- 1:nrow(testData)
testData <- merge(testData, testDataTextTable, by.x = "text", by.y = "Var1", all.x = T)
rm(testDataTextTable, tt3)
testData <- testData[order(testData$lineID),]

#create the variables necessary to create the variables used in the classifier
docLengths <- as.numeric(unlist(lapply(tt2, length)))
testData$docID <- rep(1:length(tt2), docLengths)
testData$docLengths <- rep(docLengths, docLengths)
testData$linePosition <- unlist(sapply(docLengths, seq, from = 1))

#calculate distance to document midpoint
docMidpoint <- testData$docLengths/2
docLineMidpointDist <- abs(docMidpoint-testData$linePosition)
docLineMidpointDistRelative <- docLineMidpointDist/testData$docLengths
testData$medianDocMidDist <- docLineMidpointDistRelative
rm(docMidpoint,docLineMidpointDist,docLineMidpointDistRelative)

#calculate nunber of characters
testData$nchars <- nchar(testData$text)
#count the number of tokens
tks <- tokens(testData$text)
testData$nwords <- unlist(lapply(tks, length))
rm(tks)

#rename variables so they match up with those in the random forest weights
names(testData)[names(testData)=="Freq"] <- "freq"

#load the weights from the random forest
load('rfiles/boilerplateClassifierWeights.rdata')

if(nrow(testData)<=10000){
  testData$predictedClass <- predict(mRF, testData)
}else{
  chunks <- seq(0,nrow(testData), 10000)
  chunks <- c(chunks, nrow(testData))
  
  #start_time <- Sys.time()
  testResults <- list()
  for(i in 1:(length(chunks)-1)){
    testData2 <- testData[(chunks[i]+1):chunks[i+1],]
    testResults[[i]] <- predict(mRF, testData2)
  }
  #end_time <- Sys.time()
  #end_time - start_time
  testResults <- unlist(testResults)
  testData$predictedClass <- testResults
}

classifiedSubstantive <- testData[testData$predictedClass=="substantive",]
classifiedBoilerplate <- testData[testData$predictedClass=="boilerplate",]

# # Illustrate the impact of the boilerplate removal
library(ggplot2)

testDataBefore <- subset(testData, select = c(freq, medianDocMidDist, nchars, nwords))
testDataAfter <- subset(classifiedSubstantive, select = c(freq, medianDocMidDist, nchars, nwords))
testDataBefore$`Boilerplate Removal` <- "Before"
testDataAfter$`Boilerplate Removal` <- "After"
testDataBeforeAfter <- rbind(testDataBefore, testDataAfter)

ggplot(testDataBeforeAfter, aes(x = freq, color = `Boilerplate Removal`)) + geom_density() + xlim(0, 1000)
ggsave("paper/figures/boilerplateBeforeAfterFreq.pdf")
ggplot(testDataBeforeAfter, aes(x = medianDocMidDist, color = `Boilerplate Removal`)) + geom_density()
ggsave("paper/figures/boilerplateBeforeAfterMedianDocMidDist.pdf")
ggplot(testDataBeforeAfter, aes(x = nchars, color = `Boilerplate Removal`)) + geom_density()
ggsave("paper/figures/boilerplateBeforeAfterNchars.pdf")
ggplot(testDataBeforeAfter, aes(x = nwords, color = `Boilerplate Removal`)) + geom_density()
ggsave("paper/figures/boilerplateBeforeAfterNwords.pdf")

rm(testDataAfter, testDataBefore, testDataBeforeAfter)

# Illustrate the probabilities of which lines get classified as either boilerplate or substantive
predictedProbabilities <- predict(mRF, testData, type = "prob")
predictedProbabilities$Text <- testData$text
names(predictedProbabilities)[1:2] <- c("Substantive", "Boilerplate")
predictedProbabilities <- predictedProbabilities[order(predictedProbabilities$Boilerplate),]
#Put the 10 examples with the lowest and highest probabilities in a table
tabBoilerplateIllustration <- predictedProbabilities[1:10,]
tabBoilerplateIllustration <- rbind(tabBoilerplateIllustration, predictedProbabilities[(nrow(predictedProbabilities)-9):nrow(predictedProbabilities),])
#switch around the variables so the text comes first
tabBoilerplateIllustration <- data.frame(Line = substring(tabBoilerplateIllustration$Text, 1, 50),
                                         Substantive = tabBoilerplateIllustration$Substantive,
                                         Boilerplate = tabBoilerplateIllustration$Boilerplate)

xtTabBoilerplateIllustration <- print(xtable(tabBoilerplateIllustration,
                                             digits = 2,
                                             caption = "Lines (or the first 50 characters of a line) in the corpus of Anchorage, AK, with the 10 highest and lowed probabilities of being classified as boilerplate."), 
                                      label = "tabBoilerplateIllustration",
                                      size = "small",
                                      include.rownames = FALSE)

writeLines(xtTabBoilerplateIllustration, con = 'paper/tables/tabBoilerplateIllustration.tex')

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

save.image("rfiles/AnchorageDemoImg.rdata")

ggplot(testDataBeforeAfter, aes(x = log(freq), color = `Boilerplate Removal`)) + geom_density(adjust=5) + labs(x = "Log Line Frequency in the City", y = "Density")
ggsave("paper/figures/boilerplateBeforeAfterFreq.pdf")
ggplot(testDataBeforeAfter, aes(x = medianDocMidDist, color = `Boilerplate Removal`)) + geom_density()  + labs(x = "Median Distance to the Document Midpoint", y = "Density")
ggsave("paper/figures/boilerplateBeforeAfterMedianDocMidDist.pdf")
ggplot(testDataBeforeAfter, aes(x = log(nchars), color = `Boilerplate Removal`)) + geom_density(adjust=5)  + labs(x = "Line Length (Log Number of Characters)", y = "Density")
ggsave("paper/figures/boilerplateBeforeAfterNchars.pdf")
ggplot(testDataBeforeAfter, aes(x = log(nwords), color = `Boilerplate Removal`)) + geom_density(adjust=5)  + labs(x = "Line Length (Log Number of Words)", y = "Density")
ggsave("paper/figures/boilerplateBeforeAfterNwords.pdf")

rm(testDataAfter, testDataBefore, testDataBeforeAfter)

# Illustrate the probabilities of which lines get classified as either boilerplate or substantive
predictedProbabilities <- predict(mRF, testData, type = "prob")
predictedProbabilities$Text <- testData$text
names(predictedProbabilities)[1:2] <- c("Substantive", "Boilerplate")
predictedProbabilities <- predictedProbabilities[order(predictedProbabilities$Boilerplate, decreasing = T),]
#predicted probabilities of the lines that occur the most
testData4 <- subset(testData, select = c(text, freq))
testData4 <- testData4[order(testData4$freq, decreasing = T),]
testData4 <- testData4[!duplicated(testData4$text),]
testData4 <- testData4[1:20,]

tabBoilerplateDemo <- merge(predictedProbabilities, testData4, by.x = "Text", by.y = "text")
tabBoilerplateDemo <- tabBoilerplateDemo[!duplicated(tabBoilerplateDemo$Text),]
tabBoilerplateDemo <- tabBoilerplateDemo[order(tabBoilerplateDemo$Boilerplate, decreasing = T),]
tabBoilerplateDemo <- subset(tabBoilerplateDemo, select = -Substantive)
names(tabBoilerplateDemo) <- c("Line", "Boilerplate Probability", "Line Frequency")

xtTabBoilerplateIllustration <- print(xtable(tabBoilerplateDemo,
                                             digits = 2,
                                             caption = "The 20 most frequent lines in the corpus of Anchorage, AK, sorted according to the probability with which the classifier identifies them as boilerplate. This table illustrates that the boilerplate classifier correctly flags and removes interpretable but unimportant content which would otherwise have a disproportionate impact on the topic model.",
                                             label = "tabBoilerplateIllustration"),
                                      size = "small",
                                      include.rownames = FALSE)

writeLines(xtTabBoilerplateIllustration, con = 'paper/tables/tabBoilerplateIllustration.tex')

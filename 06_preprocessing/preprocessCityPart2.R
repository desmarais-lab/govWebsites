set.seed(1)

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

#remove the lines predicted to be boilerplate by the classifier
testDataClassified <- testData[testData$predictedClass=="substantive",]

#paste the strings back together
tt22 <- aggregate(testDataClassified$text, by = list(testDataClassified$docID), FUN = paste, collapse = " ")

# do the rest of the preprocessing

#convert back to a quanteda tokens object
tt3 <- tokens(tt22$x)

uniquetokens <- unique(unlist(tt3))

#create a vector of words to be removed: short words, non-English words, stopwords
#get words that are too short
tooShort <- uniquetokens[nchar(uniquetokens)<3]

#get words that are not in an english dictionary
library('hunspell')
# library('parallel')
# ncores <- detectCores() - 1
# cl <- makeForkCluster(ncores)
# spellingErrors <- parSapply(cl, uniquetokens, FUN = hunspell_check)
# stopCluster(cl)
#non-parallelized version:
spellingErrors <- sapply(uniquetokens, hunspell_check)

spellingErrors <- names(spellingErrors)[spellingErrors==F]
#Note that this also tends to get rid of the proper nouns because they are not capitalized anymore
#this suits us just fine

#get stopwords
removeWords <- unique(c(spellingErrors, tooShort, stopwords()))

#remove short words, non-english words and stopwords
tt3 = tokens_select(tt3, removeWords, "remove")
rm(spellingErrors, tooShort, removeWords)

#everything that removes entire documents should be at the end

#calculate token statistics
ntokens <- unlist(lapply(tt3, length))
nuniquetokens <- unlist(lapply(tt3, function(x){length(unique(x))}))
tokenratio <- nuniquetokens/ntokens
#remove documents in which too many words are the same
#remove documents with too few unique tokens
removeDocs <- which(as.numeric(tokenratio)<0.15)
#remove documents with too few tokens
removeDocs <- c(removeDocs, which(as.numeric(ntokens)<50))

#there are still some documents that are exact duplicates of one another
tt4 <- unlist(lapply(tt3, paste, collapse = " "))
duplicatedDocs <- which(duplicated(tt4)==T)
removeDocs <- unique(c(duplicatedDocs, removeDocs))

a <- a[tt22$Group.1,]
a$text <- tt4
a <- a[-removeDocs,]







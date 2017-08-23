# This script selects the top n documents for each topic
# and copies these documents (both the raw, original version;
# as well as the version actually used in mallet) into their
# own directories in the paper folder

source('malletAnalysisPartisanTopics.R')
library('stringr')

#number of example documents
numdocs <- 10

#topic numbers of the topics
selectedTopics <- as.integer(unique(df.words$topic.num))

#get doc-topic matrix
doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)

#function to get the top n documents for a topic, based on their weights
exampleDocuments <- function(topic, ndocs = numdocs, doctopics = doc.topics){
  topicOrder <- data.frame(value = doc.topics[,topic], document = 1:nrow(doc.topics))
  topicOrder <- topicOrder[order(topicOrder$value, decreasing = T),]
  return(topicOrder$document[1:ndocs])
}

#matrix to store document indices in
A <- matrix(0, nrow = numdocs, ncol = length(selectedTopics))
colnames(A) <- as.character(selectedTopics)

for(i in 1:length(selectedTopics)){
  A[,i] <- exampleDocuments(selectedTopics[i])
  }

#matrix to store original paths to the documents in
B <- A
for(i in 1:nrow(A)){
  for(j in 1:ncol(B)){
    B[i,j] <- d$path[A[i,j]]
  }
}

#names for the folders in which to store the example documents
create_folders <- str_c("topic_", selectedTopics)
create_foldersFull <- str_c("./paper/exampleDocuments/", create_folders)

#Safety check to make sure we're in the right directory
wd <- getwd()
wd <- str_split(wd, "/")
wd <- wd[[1]][length(wd[[1]])]
if(wd=="govWebsites"){

  #create folders for each topic
  for(i in 1:length(create_foldersFull)){
    dir.create(create_foldersFull[i])  
  }
  
  #copy documents into the folders
  for(j in 1:ncol(B)){
    for(i in 1:nrow(B)){
      file.copy(from = B[i,j], to = create_foldersFull[j])
      }
  }
  

}

#writing the cleaned documents to files:

#matrix to store the cleaned documents in
D <- matrix("", dim(A)[1], dim(A)[2])
#matrix to store the document names in
E <- matrix("", dim(A)[1], dim(A)[2])

for(j in 1:ncol(A)){
  for(i in 1:nrow(A)){
    docnum <- A[i,j]
    D[i,j] <- d$doc[docnum]
    E[i,j] <- d$filename[docnum]
  }}

create_folders <- str_c("topic_", selectedTopics)
create_foldersFull <- str_c("./paper/exampleDocumentsCleaned/", create_folders)

#Safety check to make sure we're in the right directory
wd <- getwd()
wd <- str_split(wd, "/")
wd <- wd[[1]][length(wd[[1]])]
if(wd=="govWebsites"){
  
  #create folders for each topic
  for(i in 1:length(create_foldersFull)){
    dir.create(create_foldersFull[i])  
  }
  
  #copy documents into the folders
  for(j in 1:ncol(A)){
    for(i in 1:nrow(A)){
      writeLines(D[i,j], str_c("./paper/exampleDocumentsCleaned/", create_folders[j], "/", E[i,j]))
    }
  }
  
  
}
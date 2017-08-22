library('stringr')

#Example documents
numdocs <- 100

selectedTopics <- as.integer(unique(df.words$topic.num))

doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)

exampleDocuments <- function(topic, ndocs = numdocs, doctopics = doc.topics){
  topicOrder <- data.frame(value = doc.topics[,topic], document = 1:nrow(doc.topics))
  topicOrder <- topicOrder[order(topicOrder$value, decreasing = T),]
  return(topicOrder$document[1:ndocs])
}

A <- matrix(0, nrow = numdocs, ncol = length(selectedTopics))
colnames(A) <- as.character(selectedTopics)

for(i in 1:length(selectedTopics)){
  A[,i] <- exampleDocuments(selectedTopics[i])
  }

B <- A
for(i in 1:nrow(A)){
  for(j in 1:ncol(B)){
    B[i,j] <- d$path[A[i,j]]
  }
}

create_folders <- str_c("topic_", selectedTopics)
create_foldersFull <- str_c("./paper/exampleDocuments/", create_folders)

#Safety check to make sure we're in the right directory
wd <- getwd()
wd <- str_split(wd, "/")
wd <- wd[[1]][length(wd[[1]])]
if(wd!="govWebsites"){

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
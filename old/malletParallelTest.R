options(java.parameters = "-Xmx6000m")
library('mallet')
library('foreach')
library('doParallel')
cl <- makeCluster(2, type="FORK") # number of threads to use
registerDoParallel(cl)

#fake corpus
corpus <- c("this is a test", "minimal working example", "fake document", "is this even long enough",
            "more fake documents to test with", "this takes less than a second when not parallelized",
            "I suspect this might be a problem with rJava", "I tried several different parallel packages",
            "apparently java virtual machines cannot be forked", "therefore this might be impossible")

#load corpus in the form that mallet requires
mallet.instances <- mallet.import(id.array = as.character(1:length(corpus)),
                                  text.array = corpus,
                                  stoplist.file = "./rfiles/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#parallelized loop begins here
#just two iterations
foreach(j=(1:2)) %dopar% {
  
  #not even changing the number of topics between the two iterations
  ntopics <- 5
  #print(ntopics)
  #this doesn't even train the model yet
  topic.model <- MalletLDA(num.topics = ntopics)
  
}

stopCluster(cl)

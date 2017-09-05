options(java.parameters = "-Xmx3000m")
library('mallet')

#load data
load(file = "./rfiles/d.Rdata")

ntests <- 10

ntopics <- 200
opt_it <- 20
opt_burnin <- 50
train_it <- seq(50, 500, length.out = ntests)

entr <- numeric(ntests)

for(i in 1:ntests){

  mallet.instances <- mallet.import(id.array = make.unique(d$folder),
                                    text.array = d$doc,
                                    stoplist.file = "./rfiles/stopwords.txt",
                                    token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  topic.model <- MalletLDA(num.topics = ntopics)
  topic.model$loadDocuments(mallet.instances)
  vocabulary <- topic.model$getVocabulary()
  word.freqs <- mallet.word.freqs(topic.model)
  topic.model$setAlphaOptimization(opt_it, opt_burnin)
  topic.model$train(train_it[i])
  topic.model$maximize(10)
  
  #record mean entropy
  entr[i] <- mean(city_entropy(mallet.doc.topics(topic.model, smoothed = F, normalized = F), d))

}

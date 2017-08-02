options(java.parameters = "-Xmx3000m")

library('mallet')
library('tibble')
library('stringr')
library('tools')
library('tm')
library('dplyr')


corpus <- "current" #"current", "before", or "after"

#Hunspell
load(file = str_c("./rfiles/docs_", corpus, ".Rdata"))

#remove documents where spellchecking failed
d <- d[d$spell_fail==0,]

#remove non-spellchecked text and some other stuff
d$doc <- d$doc2
d <- select(d, -doc2)
d$doc <- gsub("\\s+"," ", d$doc)
d$doc <- str_trim(d$doc, side = "both")
d <- d[d$doc!="",]

#Import website text
mallet.instances <- mallet.import(id.array = make.unique(d$folder),
                                  text.array = d$doc,
                                  stoplist.file = "./rfiles/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#Create a Mallet topic model trainer
ntopics <- 100
topic.model <- MalletLDA(num.topics = ntopics)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary (i.e. list of all the words)
vocabulary <- topic.model$getVocabulary()
vocabulary

## word frequencies
## also contains a column with the total number of documents a word appears in
word.freqs <- mallet.word.freqs(topic.model)
word.freqs[order(word.freqs$term.freq, decreasing = T),]

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

#R seems to be using about 2.3GB while training the model
#takes about 5 minutes

## NEW: run through a few iterations where we pick the best topic for each token, 
##  rather than sampling from the posterior distribution.
topic.model$maximize(10)

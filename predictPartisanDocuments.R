library('quanteda')
library('e1071')

#load data
load(file = "./rfiles/d.Rdata")

#create corpus
crps <- corpus(d$doc)

#set party docvar
#docvars(crps, "Party") <- d$winner
#docvars(crps, "City") <- d$Name

#convert to dfm
dfmIN <- dfm(crps)

#calculate tf-idf
tf_idf <- quanteda::tfidf(dfmIN)
tf_idf_terms <- colSums(tf_idf)
tf_idf_terms_sorted <- sort(tf_idf_terms, decreasing = T)
tf_idf_terms_sorted <- tf_idf_terms_sorted[1:5000] #top 5000 word types by tf-idf
term_indices <- which(names(tf_idf_terms)%in%names(tf_idf_terms_sorted))

#Alternatively, with SpeedReader
#dtm2 <- slam::as.simple_triplet_matrix(dfmIN)
#tf_idf <- SpeedReader::tfidf(dtm2, colnames(dfmIN))
#tf_idf_rankings <- tf_idf$tfidf_rankings
#tf_idf_top_terms <- tf_idf_rankings$term[1:5000]
#term_indices <- which(colnames(dfmIN)%in%tf_idf_top_terms)

#Created document-term matrix; retain only the top 5000 words
dtm <- as.data.frame(dfmIN)
dtm <- dtm[,term_indices]

#Create dependent variable
dtm <- cbind(CLASS = as.numeric(as.factor(d$winner))-1, dtm)

#remove everything but dtm to save memory
rm(list = ls()[ls()!="dtm"])

#training and test sets
sample_indices <- sample(c(1:5), nrow(dtm), replace = TRUE)
training_sets <- list()
testing_sets <- list()
svm_results <- list()

for(i in 1:5){

training <- dtm[sample_indices != i, ]
testing <- dtm[sample_indices == i, ]

training_sets[[i]] <- training
testing_sets[[i]] <- testing

#Use svm on DTM with
svmfit <- svm(CLASS ~ .,
              data = training,
              type = "C-classification",
              kernel = "radial",
              cost = 1)

svm_results[[i]] <- svmfit

}

#Save session
save.image("rfiles/svm_session.RData")
#load("rfiles/svm_session.RData")

#make predictions based on the test sets
predictions <- list()
for(i in 1:5){
  predictions[[i]] <- predict(svm_results[[i]], testing_sets[[i]])
}
#arrange into usable data frames and compare yhats to ys
predictions.vec <- unlist(predictions)
predictions.df <- data.frame(text = names(predictions.vec), pred = predictions.vec)
predictions.df$text <- as.numeric(gsub(pattern = "text", "", predictions.df$text))
predictions.df <- predictions.df[order(predictions.df$text),]
predictions.df$actual <- (as.numeric(as.factor(d$winner))-1) #Democratic is 0, Republican is 1
predictions.df$correct <- predictions.df$pred==predictions.df$actual
sum(predictions.df$correct)/length(predictions.df$correct)
#accuracy: 0.81689 (Not that great considering that always guessing Democratic would get 0.6364483)

#Save session
save.image("rfiles/svm_session.RData")

#too big for git, so keep only important stuff:
save(predictions.df, file = 'svm_predictions.RData')

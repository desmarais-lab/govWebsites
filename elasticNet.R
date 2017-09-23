library('quanteda')
library('Matrix')
library('glmnet')

#setwd('govWebsites')

#load data
load(file = "./rfiles/d.Rdata")

#create corpus
crps <- corpus(d$doc)

#convert to dfm/dtm
dfmIN <- dfm(crps)
dtm <- slam::as.simple_triplet_matrix(dfmIN)

as.sparseMatrix <- function(simple_triplet_matrix_sparse) {
  retval <-  sparseMatrix(i=as.numeric(simple_triplet_matrix_sparse$i),
                          j=as.numeric(simple_triplet_matrix_sparse$j),
                          x=as.numeric(as.character(simple_triplet_matrix_sparse$v)),
                          dims=c(simple_triplet_matrix_sparse$nrow, 
                                 simple_triplet_matrix_sparse$ncol),
                          dimnames = dimnames(simple_triplet_matrix_sparse),
                          giveCsparse = TRUE)
}

dtm <- as.sparseMatrix(dtm)
y <- as.numeric(as.factor(d$winner))-1

#use elastic-net in glmnet

#fit the model
#raises a warning, but increasing maxit does not help
glmnetFit <- glmnet(dtm, y, family = "binomial", maxit = 500000)  

#in-sample prediction
a = predict(glmnetFit, type = "class", dtm)
#a = predict(glmnetFit, type = "response", dtm)

#get majority vote on outcome from 45 factors
a <- apply(a, 2, as.numeric)
which.more <- function(X){
  m <- as.numeric(names(table(X))[which.max(table(X))])
  return(m)
}
a <- apply(a, 1, which.more)

#percent correctly predicted
mean(a==y)

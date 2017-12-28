library(quanteda)
library(xtable)

state <- "Indiana"
stateAbb <- "IN"

state <- "Louisiana"
stateAbb <- "LA"

load(paste0("rfiles/d_", stateAbb, ".rdata"))

#Number of cities per party
ncities <- xtabs(~ City + Party, d)
ncities[ncities!=0] <- 1
ncities <- colSums(ncities)

#Number of documents per party
ndocs <- colSums(xtabs(~ City + Party, d))

#Number of token instances and types per party
crpsD <- corpus(d$doc[d$Party=="Democratic"])
crpsR <- corpus(d$doc[d$Party=="Republican"])
dtmD <- dfm(crpsD)
dtmR <- dfm(crpsR)
toksD <- tokens(crpsD)
toksR <- tokens(crpsR)
ntokenTypesD <- length(types(toksD))
ntokenTypesR <- length(types(toksR))
ntokenInstancesD <- length(unlist(toksD))
ntokenInstancesR <- length(unlist(toksR))

xtab <- matrix(nrow = 4, ncol = 3)

colnames(xtab) <- c("Democratic", "Republican", "Total")
rownames(xtab) <- c("Cities", "Documents", "Token types", "Token instances")

xtab[1,c(1,2)] <- ncities
xtab[2,c(1,2)] <- ndocs
xtab[3,1] <- ntokenTypesD
xtab[3,2] <- ntokenTypesR
xtab[4,1] <- ntokenInstancesD
xtab[4,2] <- ntokenInstancesR
xtab[,3] <- apply(xtab[,c(1,2)], 1, sum)

crps <- corpus(d$doc)
toks <- tokens(crps)
ntokenTypes <- length(types(toks))
xtab[3,3] <- ntokenTypes

writeLines(print.xtable(xtable(xtab, digits = 0, caption = state)),
           paste0("paper/presentation/descriptiveStats", stateAbb, ".tex"))

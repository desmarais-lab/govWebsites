#setwd("govWebsites")
library('xtable')

load('rfiles/d.Rdata')

#Number of cities per party
ncities <- xtabs(~ Name + winner, d)
ncities[ncities!=0] <- 1
ncities <- colSums(ncities)

table1 <- ncities

#Number of citizens per party
#indiana <- subset(indiana, select = c('District', 'POPESTIMATE2015'))
#d2 <- merge(d, indiana, by.x = 'Name', by.y = 'District', all.x = T)
#unique(d2$Name[is.na(d2$POPESTIMATE2015)==T])

#Number of documents per party
ndocs <- colSums(xtabs(~ Name + winner, d))

table1 <- rbind(table1, ndocs)

#Number of tokens per party
ntokensD <- sum(d$ntokens[d$winner=="Democratic"])
ntokensR <- sum(d$ntokens[d$winner=="Republican"])

table1 <- rbind(table1, c(ntokensD, ntokensR))

source('malletAnalysisPartisanTopics.R')

#Number of token assignments


#Number of topics


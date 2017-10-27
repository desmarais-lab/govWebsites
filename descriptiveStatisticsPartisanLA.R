#setwd("govWebsites")
library('xtable')

load('rfiles/dLA.rdata')

#Number of cities per party
ncities <- xtabs(~ City + winner, d)
ncities[ncities!=0] <- 1
ncities <- colSums(ncities)

table1 <- ncities

#Number of citizens per party
#indiana <- subset(indiana, select = c('District', 'POPESTIMATE2015'))
#d2 <- merge(d, indiana, by.x = 'Name', by.y = 'District', all.x = T)
#unique(d2$Name[is.na(d2$POPESTIMATE2015)==T])

#Number of documents per party
ndocs <- colSums(xtabs(~ City + winner, d))

table1 <- rbind(table1, ndocs)

#Number of tokens per party
ntokensD <- sum(d$ntokens[d$winner=="Democratic"])
ntokensR <- sum(d$ntokens[d$winner=="Republican"])

table1 <- rbind(table1, c(ntokensD, ntokensR))

source('malletAnalysisPartisanTopicsLA.R')

#Number of token assignments
ntokenaD <- sum(democratic)
ntokenaR <- sum(republican)
table1 <- rbind(table1, c(ntokenaD, ntokenaR))

#Number of topics
ntopics <- table(props$party)
table1 <- rbind(table1, ntopics)

rownames(table1) <- c("Cities", "Documents", "Tokens", "Token assignments", "Topics")

xt <- xtable(table1,
             digits = 0,
             caption = "Descriptive statistics for Louisiana. ``Tokens'' describes the number
             of words in each party's documents, ``token assignments'' the tokens assigned
             to each party in the topic model depending on the ratio of Democratic to Republican 
             tokens in it (both weighted by the total number of tokens per party).",
             label = "tabDescriptiveLA")

xt <- print.xtable(xt)

writeLines(xt, 
           con = 'paper/tables/descriptiveStatisticsPartisanLA.tex')

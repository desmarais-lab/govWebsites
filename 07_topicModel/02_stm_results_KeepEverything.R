library(stm)

set.seed(1)

load("out/stmSession_sim_60_KeepEverything.rdata")

#make a dataframe to store the results in
df <- data.frame(coef = rep(0, numtopics), sig = rep(FALSE, numtopics),
                 lower = rep(0, numtopics), upper = rep(0, numtopics),
                 topic = 1:numtopics)

conf_level <- 1 #in percent
conf_level <- conf_level/100 #as a fraction
conf_level <- conf_level/2 #two-sided

conf_lower <- 0 + conf_level
conf_upper <- 1 - conf_level

for(j in 1:numtopics){
  
  coefs <- list()
  
  for(i in 1:sims){
    coefs[[i]] <- prep$parameters[[j]][[i]]$est[2]
  }
  
  coefs <- do.call(c, coefs)
  coefs <- quantile(coefs, c(conf_lower, conf_upper))
  
  df$lower[j] <- coefs[1]
  df$upper[j] <- coefs[2]
  df$sig[j] <- all(coefs>0) | all(coefs<0)
  df$coef[j] <- mean(coefs)
  
}

#number of significant topics
sum(df$sig)

topwords <- stm::labelTopics(stmFit, topics = NULL, n = 7, frexweight = 0.5)
topwords <- topwords$frex

library(stringr)
library(xtable)

coeffBatches <- seq(-max(abs(df$coef)), max(abs(df$coef)), length.out = 17)
blues <- rev(c("blue!10", "blue!20", "blue!30", "blue!40", "blue!50", "blue!60", "blue!70", "blue!80"))
reds <- c("red!10", "red!20", "red!30", "red!40", "red!50", "red!60", "red!70", "red!80")

f <- function(x) cut(x, coeffBatches, 
                     labels= c(blues, reds),
                     include.lowest = T, right = TRUE)

f(-max(abs(df$coef)))

topwords2 <- as.data.frame(topwords[,1:7])

#tab = data.frame(category = c("A","B","C"), groupA = c(.2,.3,.5), groupB= c(.6,.7,.9))
cellcolors <- lapply(df$coef, function(x) paste0("\\cellcolor{", f(x), "}"))
cellcolors[which(df$sig==F)] <- "\\cellcolor{white}"
topwords3 <- apply(topwords[,1:7], 2, function(x)paste(cellcolors, x, sep = ""))
topwords3 <- as.data.frame(topwords3)

#add topic column
topwords3 <- cbind(1:nrow(topwords3), topwords3)

#add topic number of topics column
topicWordMatrix <- do.call(rbind, stmFit$beta$logbeta)
tokenTopicAssignments <- apply(topicWordMatrix, 2, which.max)
topwords3$ntokens <- as.vector(table(tokenTopicAssignments))
topwords3$ntokens <- paste0("\\mybar{", topwords3$ntokens, "}")

names(topwords3) <- c("Topic", paste("Top Word", 1:7), "Tokens assigned")

#order by coefficient
topwords3 <- topwords3[order(df$coef, decreasing = T),]

#remove the last topic
topwords3 <- topwords3[,-8]

xtTopwords <- print(xtable(topwords3, caption = "Top words from a structural topic model with 60 topics and FREX scoring. Colors depict partisanship based on coefficient size. White cells are non-significant topics.",
                           label = "tabSTMtopwords60"), 
                    sanitize.text.function = identity,
                    size = "scriptsize",
                    include.rownames = FALSE)

xtTopwords <- str_replace(xtTopwords, "rlllllll", "rllllllll")
xtTopwords <- str_replace(xtTopwords, "& Tokens assigned", "& \\\\multicolumn{2}{c}{Tokens assigned}")
xtTopwords <- str_replace(xtTopwords, "\nTopic &", "\n \\\\# &")


writeLines(xtTopwords, con = '../paper/tables/stmTopWords60_KeepEverything.tex')


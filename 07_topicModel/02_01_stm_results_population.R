library(stm)

set.seed(1)

load("out/stmSession_sim_60.rdata")

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
    coefs[[i]] <- prep$parameters[[j]][[i]]$est[8]
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
blues <- rev(c("magenta!10", "magenta!20", "magenta!30", "magenta!40", "magenta!50", "magenta!60", "magenta!70", "magenta!80"))
reds <- c("cyan!10", "cyan!20", "cyan!30", "cyan!40", "cyan!50", "cyan!60", "cyan!70", "cyan!80")

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

xtTopwords <- print(xtable(topwords3, caption = "Top words from a structural topic model with 60 topics and FREX scoring. Colors depict city population based on coefficient size (larger cities are cyan, smaller cities are magenta). White cells are non-significant topics.",
                           label = "tabSTMtopwords60_population"), 
                    sanitize.text.function = identity,
                    size = "scriptsize",
                    include.rownames = FALSE)

xtTopwords <- str_replace(xtTopwords, "rlllllll", "rllllllll")
xtTopwords <- str_replace(xtTopwords, "& Tokens assigned", "& \\\\multicolumn{2}{c}{Tokens assigned}")
xtTopwords <- str_replace(xtTopwords, "\nTopic &", "\n \\\\# &")


writeLines(xtTopwords, con = '../paper/tables/stmTopWords60_population.tex')

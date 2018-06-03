#run stmQuanteda first

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

xtTopwords <- print(xtable(topwords3, caption = "Top words from a structural topic model with 60 topics and FREX scoring. Based on data preprocessed with the classifier. Colors depict partisanship based on coefficient size. White cells are non-significant topics."), 
      sanitize.text.function = identity,
      label = "tabSTMtopwords",
      size = "scriptsize",
      include.rownames = FALSE)

writeLines(xtTopwords, con = 'paper/tables/stmTopWords2.tex')

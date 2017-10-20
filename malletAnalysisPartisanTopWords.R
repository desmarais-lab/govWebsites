#Run malletTraining.R and the first 53 lines of malletAnalysisPartisanTopics.R first

library('xtable')

#Democratic top words
dem.topic.words <- topic.words[dem.topics,]
agg.dem.topic.words <- colSums(dem.topic.words)
names(agg.dem.topic.words) <- vocabulary
dem.top.words <- sort(agg.dem.topic.words, decreasing = T)[1:50]

#Republican top words
rep.topic.words <- topic.words[rep.topics,]
agg.rep.topic.words <- colSums(rep.topic.words)
names(agg.rep.topic.words) <- vocabulary
rep.top.words <- sort(agg.rep.topic.words, decreasing = T)[1:50]

#Create table
word.table <- tibble(dem.word = names(dem.top.words),
                     dem.tokens = dem.top.words,
                     rep.word = names(rep.top.words),
                     rep.tokens = rep.top.words)

#Write Latex table to file
xt <- xtable(word.table,
             digits = 0,
             caption = "Top 50 Democratic and Republican words (Indiana), according to LDA. 
             Topic ownership is determined by the ratio of Democratic to Republican tokens in 
             it (both weighted by the total number of tokens per party). The instances of each 
             token type are then summed across all topics owned by the party.",
             label = "tabLDAIN")

names(xt) <- c("Word (D)", "Instances (D)", "Word (R)", "Instances (R)")

xt <- print.xtable(xt, 
                   include.rownames = F,
                   size = "\\fontsize{9pt}{10pt}\\selectfont")

writeLines(xt, 
           con = 'paper/tables/partisanTopWords.tex')

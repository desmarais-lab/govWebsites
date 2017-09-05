library('xtable')

#Democratic top words
dem.topic.words <- topic.words[dem.topics,]
agg.dem.topic.words <- colSums(dem.topic.words)
names(agg.dem.topic.words) <- vocabulary
dem.top.words <- sort(agg.dem.topic.words, decreasing = T)[1:30]

#Republican top words
rep.topic.words <- topic.words[rep.topics,]
agg.rep.topic.words <- colSums(rep.topic.words)
names(agg.rep.topic.words) <- vocabulary
rep.top.words <- sort(agg.rep.topic.words, decreasing = T)[1:30]

#Create table
word.table <- tibble(dem.word = names(dem.top.words),
                     dem.tokens = dem.top.words,
                     rep.word = names(rep.top.words),
                     rep.tokens = rep.top.words)

#Write Latex table to file
writeLines(print.xtable(xtable(word.table, digits = 0),
                        include.rownames = F),
           "paper/tables/partisanTopWords.tex")

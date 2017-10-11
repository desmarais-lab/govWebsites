library('quanteda')
library('SpeedReader')

#setwd('govWebsites')

#load data
load(file = "./rfiles/dLA.rdata")

#create corpus
crps <- corpus(d$doc)

#convert to dfm; then data frame
dtm <- dfm(crps)
dtm2 <- slam::as.simple_triplet_matrix(dtm)

#create contingency table

d$party <- d$winner

#only works if force_tense = T
cgt <- contingency_table(d, dtm2, variables_to_use = c(13,13), force_dense = T)

#fix the feature selection function
source('functions/feature_selection_fixed.R')

#throws a complaint about tf-idf which isnt even selected
#fs <- feature_selection(cgt, rows_to_compare = c(1,4))

#feature selection
fs <- feature_selection(cgt, 
                        rows_to_compare = c(4,1), #Democrat first so it gets colored blue
                        method = "informed Dirichlet")

#make the funnel plot
#pdf('paper/figures/fightinWords.pdf', width = 8, height = 8) #shitty quality
ppi <- 300
png("paper/figures/fightinWordsLA.png", width=8*ppi, height=8*ppi, res=ppi)
fightin_words_plot(fs, 
                   title = "", 
                   positive_category = 'Democrat', 
                   negative_category = 'Republican',
                   display_terms_next_to_points = T,
                   size_terms_by_frequency = F,
                   right_margin = 6)
dev.off()

#make a table with the top 50 words
library('xtable')
zscoretable <- tibble::tibble(DemocraticWord = fs$Democratic_Democratic$term[1:50],
                              DemocraticScore = fs$Democratic_Democratic$z_scores[1:50],
                              RepublicanWord = fs$Republican_Republican$term[1:50],
                              RepublicanScore = fs$Republican_Republican$z_scores[1:50])

writeLines(print.xtable(xtable(zscoretable), include.rownames = F), con = 'paper/tables/fightinwordsLA.tex')

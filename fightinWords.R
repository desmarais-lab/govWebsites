library('quanteda')
library('SpeedReader')

#setwd('govWebsites')

# if no state data is loaded, default to Indiana
if(exists("d")==F){
  load(file = "./rfiles/d.Rdata")
}

if(exists("state")==F){
  state <- "Indiana"
}

if(exists("stateAbb")==F){
  stateAbb <- "IN"
}

#create corpus
crps <- corpus(d$doc)

#convert to dfm; then data frame
dtm <- dfm(crps)
dtm <- slam::as.simple_triplet_matrix(dtm)

#create contingency table
cgt <- contingency_table(d, dtm, variables_to_use = "Party")

#feature selection
#Democrat first so it gets colored blue
party_order <- c(which(unique(d$Party)=="Democratic"),
                 which(unique(d$Party)=="Republican"))

fs <- feature_selection(cgt, 
                        rows_to_compare = party_order,
                        method = "informed Dirichlet")

#make the funnel plot
#pdf('paper/figures/fightinWords.pdf', width = 8, height = 8) #shitty quality
ppi <- 300
png(paste0("paper/figures/fightinWords", stateAbb, ".png"), 
    width=8*ppi, height=8*ppi, res=ppi)
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
zscoretable <- tibble::tibble(DemocraticWord = fs$Democratic$term[1:50],
                              DemocraticScore = fs$Democratic$z_scores[1:50],
                              RepublicanWord = fs$Republican$term[1:50],
                              RepublicanScore = fs$Republican$z_scores[1:50])

xt <- xtable(zscoretable,
             caption = paste0("Top 50 Democratic and Republican words (",
                              state, "), according to the informed Dirichlet model of Monroe et al. (2008)."),
             label = paste0("tabFightin", stateAbb))

names(xt) <- c("Word (D)", "z-Score (D)", "Word (R)", "z-Score (R)")

xt <- print.xtable(xt, 
                   include.rownames = F,
                   size = "\\fontsize{9pt}{10pt}\\selectfont")

writeLines(xt, 
           con = paste0("paper/tables/fightinwords", stateAbb, ".tex"))

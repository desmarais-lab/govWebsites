library(tidyr)
library(xtable)

load("rfiles/stmSession_sim.rdata")
d <- subset(d, select = c(party, State, city))
rm(list = ls()[ls()!="d"])

d2 <- d[!duplicated(d$city),]
d2$State <- as.character(d2$State)

cities_states_parties <- xtabs(~party + State, d2)
cities_states_parties <- as.data.frame(cities_states_parties)
cities_states_parties <- spread(cities_states_parties, party, Freq)

xtTopwords <- print(xtable(cities_states_parties, 
                           caption = "Descriptive statistics on the partisanship of the cities in the corpus."), 
                    sanitize.text.function = identity,
                    label = "tabCitiesStatesParties",
                    #size = "scriptsize",
                    include.rownames = FALSE)

writeLines(xtTopwords, con = 'paper/tables/tabCitiesStatesParties.tex')

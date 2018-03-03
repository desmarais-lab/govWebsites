library(stringr)

#Load Indiana website data
#(file is created in scrapeIndianaURLsNew.R)
load("rfiles/indianaWebsiteURLs.rdata")
indianaWebsiteUrls$State <- "Indiana"

#Load Louisiana website data
#(file is created in scrapeLouisianaURLsNew.R)
load("rfiles/louisianaWebsiteURLs.rdata")
louisianaWebsiteUrls$State <- "Louisiana"

#Load New York website data
#(file is created in scrapeNewYorkURLsNew.R)
load("rfiles/newyorkWebsiteURLs.rdata")
newyorkWebsiteUrls$State <- "New York"

#Load Washington website data
#(file is created in scrapeWashingtonURLsNew.R)
load("rfiles/washingtonWebsiteURLs.rdata")
washingtonWebsiteUrls$State <- "Washington"

#Load 100 largest cities website data
load("rfiles/bigcityWebsiteURLs.rdata")

## Combine everything
library('dplyr')
websiteUrls <- bind_rows(indianaWebsiteUrls,louisianaWebsiteUrls)
websiteUrls <- bind_rows(websiteUrls, newyorkWebsiteUrls)
websiteUrls <- bind_rows(websiteUrls, washingtonWebsiteUrls)

#before merging with the big city data, clean up the strings
websiteUrls$State <- str_trim(websiteUrls$State)
websiteUrls$State_City <- paste(websiteUrls$State, websiteUrls$City, sep = "_")

#merge, then delete the duplicate variables
websiteUrls <- merge(websiteUrls, bigcityWebsiteUrls, by = "State_City", all = T)
websiteUrls$City.x[is.na(websiteUrls$City.y)==F] <- websiteUrls$City.y[is.na(websiteUrls$City.y)==F]
websiteUrls$State.x[is.na(websiteUrls$State.y)==F] <- websiteUrls$State.y[is.na(websiteUrls$State.y)==F]
websiteUrls$wiki_link.x[is.na(websiteUrls$wiki_link.y)==F] <- websiteUrls$wiki_link.y[is.na(websiteUrls$wiki_link.y)==F]
websiteUrls <- subset(websiteUrls, select = -c(City.y, State.y, wiki_link.y))
names(websiteUrls)[names(websiteUrls)=="State.x"] <- "State"
names(websiteUrls)[names(websiteUrls)=="City.x"] <- "City"
names(websiteUrls)[names(websiteUrls)=="wiki_link.x"] <- "wiki_link"

#websiteUrls <- bind_rows(websiteUrls, bigcityWebsiteUrls)
rm(list = c("indianaWebsiteUrls", "louisianaWebsiteUrls", "newyorkWebsiteUrls",
            "washingtonWebsiteUrls", "bigcityWebsiteUrls"))

#merge with the GSA website data
load(file="data/govWebsitesVerifiedCensus.Rdata")
data9 <- subset(data9, select=c("NAME","redirect","STNAME"))
data9$NAME[data9$NAME=="15885"] <- "Indianapolis"
data9$State_City <- paste(data9$STNAME, data9$NAME, sep="_")
data9 <- data.frame(State_City = data9$State_City, gsaCityWebsite = data9$redirect)

websiteUrls <- merge(websiteUrls, data9, by = "State_City", all.x = T)
websiteUrls$gsaCityWebsite <- as.character(websiteUrls$gsaCityWebsite)

#merge with Census data

# << tbd >>

#Set NAs to empty fields
websiteUrls$wikiCityWebsite[websiteUrls$wikiCityWebsite==""] <- NA

#check whether a city has a website from SOME source
websiteUrls$urlExists <- F
websiteUrls$urlExists[is.na(websiteUrls$wikiCityWebsite)==F | is.na(websiteUrls$ballotpCity_website)==F | is.na(websiteUrls$gsaCityWebsite)==F] <- T
#check whether a city has a partisanship indicator from SOME source
websiteUrls$partyExists <- F
websiteUrls$partyExists[is.na(websiteUrls$wikiPartisanship)==F | is.na(websiteUrls$officialParty)==F | is.na(websiteUrls$leapParty)==F | is.na(websiteUrls$financePartisanship)==F | is.na(websiteUrls$financeParty)==F | is.na(websiteUrls$ballotpParty)==F] <- T

save(websiteUrls, file = "rfiles/allURLsFull.rdata")

websiteUrls <- websiteUrls[websiteUrls$urlExists==T & websiteUrls$partyExists == T,]

#combine all the measures
#these should be in the order in which we trust the sources the most
#i.e. most trusted first
websiteUrls$cityWebsite <- websiteUrls$wikiCityWebsite
websiteUrls$cityWebsite[is.na(websiteUrls$cityWebsite)==T] <- websiteUrls$ballotpCity_website[is.na(websiteUrls$cityWebsite)==T]
websiteUrls$cityWebsite[is.na(websiteUrls$cityWebsite)==T] <- websiteUrls$gsaCityWebsite[is.na(websiteUrls$cityWebsite)==T]

websiteUrls$party <- as.character(websiteUrls$officialParty)
websiteUrls$party[is.na(websiteUrls$party)==T] <- as.character(websiteUrls$leapParty[is.na(websiteUrls$party)==T])
websiteUrls$party[is.na(websiteUrls$party)==T] <- as.character(websiteUrls$ballotpParty[is.na(websiteUrls$party)==T])
websiteUrls$party[is.na(websiteUrls$party)==T] <- as.character(websiteUrls$wikiPartisanship[is.na(websiteUrls$party)==T])
websiteUrls$party[is.na(websiteUrls$party)==T] <- as.character(websiteUrls$financePartisanship[is.na(websiteUrls$party)==T])
websiteUrls$party[is.na(websiteUrls$party)==T] <- as.character(websiteUrls$financeParty[is.na(websiteUrls$party)==T])

#rename all party measures to the same scheme
websiteUrls$party[websiteUrls$party%in%c("R", "REP")] <- "Republican"
websiteUrls$party[websiteUrls$party%in%c("D", "DEM")] <- "Democratic"
websiteUrls$party[websiteUrls$party%in%c("I", "Neither", "NEITHER", "Unknown")] <- "Other"

websiteUrls <- websiteUrls[websiteUrls$party!="Other",]

save(websiteUrls, file = "rfiles/allURLs.rdata")

# Summary statistics
websiteUrls$State2 <- websiteUrls$State
websiteUrls$State2[!websiteUrls$State2%in%c("Indiana", "Louisiana", "New York", "Washington")] <- "Other"
xt <- as.data.frame.matrix(xtabs(~ State2 + party, data = websiteUrls))
xt$Total <- xt$Democratic+xt$Republican
xt <- rbind(xt, apply(xt, 2, sum))
rownames(xt)[6] <- "Total"

library(xtable)

xt <- print(xtable(xt,
                   caption = "Descriptive statistics for the URLs for which we have information about city partisanship.", 
                   label = "urlSummary"))

writeLines(xt, con = 'paper/tables/urlSummary.tex')

#cities per state
xt <- as.data.frame(table(websiteUrls$State))
names(xt) <- c("State", "Cities")

xt <- print(xtable(xt,
                   caption = "Number of cities per state for which we have information about partisanship as well as the city's website URL.", 
                   label = "stateUrlSummary"),
            include.rownames = FALSE)

writeLines(xt, con = 'paper/tables/stateUrlSummary.tex')

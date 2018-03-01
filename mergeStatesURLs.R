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
load("./rfiles/campaign_websites.rdata")
df$State <- str_split_fixed(df$City, ",", 2)[,2]
df$City <- str_split_fixed(df$City, ",", 2)[,1]
df$Mayor <- str_replace(df$Mayor, " (\\(.*?\\))", "")
df$party <- str_replace(df$party, "\\(", "")
df$party <- str_replace(df$party, "\\)", "")
names(df) <- c("Rank", "City", "ballotpMayor", "ballotpTermBegin", "ballotpTermEnd", "ballotpGovType", "ballotpParty", "ballotpCityPage", "ballotpMayor2", "ballotpMayorPage", "ballotpCampaign_website", "ballotpCity_website", "State")
bigcityWebsiteUrls <- df
rm(df)

## Combine everything
library('dplyr')

websiteUrls <- bind_rows(indianaWebsiteUrls,louisianaWebsiteUrls)
websiteUrls <- bind_rows(websiteUrls, newyorkWebsiteUrls)
websiteUrls <- bind_rows(websiteUrls, washingtonWebsiteUrls)
websiteUrls <- bind_rows(websiteUrls, bigcityWebsiteUrls)

#Set NAs to empty fields
websiteUrls$wikiCityWebsite[websiteUrls$wikiCityWebsite==""] <- NA

#check whether a city has a website from SOME source
websiteUrls$urlExists <- F
websiteUrls$urlExists[is.na(websiteUrls$wikiCityWebsite)==F] <- T
#check whether a city has a partisanship indicator from SOME source
websiteUrls$partyExists <- F
websiteUrls$partyExists[is.na(websiteUrls$wikiPartisanship)==F | is.na(websiteUrls$officialParty)==F | is.na(websiteUrls$leapParty)==F | is.na(websiteUrls$financePartisanship)==F | is.na(websiteUrls$financeParty)==F | is.na(websiteUrls$ballotpParty)==F] <- T

save(websiteUrls, file = "rfiles/allURLsFull.rdata")

websiteUrls <- websiteUrls[websiteUrls$urlExists==T & websiteUrls$partyExists == T,]
save(websiteUrls, file = "rfiles/allURLs.rdata")


#ALL
# GSA .gov website data
data9 <- subset(data9, select=c("NAME","redirect","StateShort"))
data9 <- subset(data9, StateShort=="IN")
data9$NAME[data9$NAME=="15885"] <- "Indianapolis"
data9$State <- data9$StateShort
data9$StateShort <- NA
data9$State_Name <- paste(data9$State, data9$NAME, sep="_")
names(data9) <- c("Name", "Website", "Designation", "State", "State_Name")



#get the base URL
#df$Website <- str_extract(df$Website, "^.+?[^\\/:](?=[?\\/]|$)")


load("./rfiles/campaign_websites.rdata")
df$State <- str_split_fixed(df$City, ",", 2)[,2]
df$City <- str_split_fixed(df$City, ",", 2)[,1]
df$Mayor <- str_replace(df$Mayor, " (\\(.*?\\))", "")
df$party <- str_replace(df$party, "\\(", "")
df$party <- str_replace(df$party, "\\)", "")
names(df) <- c("Rank", "City", "ballotpMayor", "ballotpTermBegin", "ballotpTermEnd", "ballotpGovType", "ballotpParty", "ballotpCityPage", "ballotpMayor2", "ballotpMayorPage", "ballotpCampaign_website", "ballotpCity_website", "State")
bigcityWebsiteUrls <- df
rm(df)
bigcityWebsiteUrls$State <- str_trim(bigcityWebsiteUrls$State)
bigcityWebsiteUrls$State_City <- paste(bigcityWebsiteUrls$State, bigcityWebsiteUrls$City, sep = "_")

source('functions/scrapeCityUrlsMayors.R')

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[4]] 
df <- mytable %>% html_table(fill=T)
df$City <- str_replace(df$City, "\\[(.*?)\\]", "")
df$City <- str_trim(df$City)

#use the get_link function on all the cities
df$wiki_link <- sapply(df$City, function(x)get_link(mytable, x))

#paste state and city together
df$State_City <- paste(df$`State[5]`, df$City, sep = "_")
df2 <- df[,c(13,11,12)]

#make a few corrections
df2$State_City[df2$State_City=="District of Columbia_Washington"] <- "D.C._Washington"
df2$State_City[df2$State_City=="Minnesota_Saint Paul"] <- "Minnesota_St. Paul"
df2$State_City[df2$State_City=="North Carolina_Winston–Salem"] <- "North Carolina_Winston-Salem"
names(df2)[2] <- "wikiLocation"

#merge
bigcityWebsiteUrls <- merge(bigcityWebsiteUrls, df2, by = "State_City")

#rename new york
bigcityWebsiteUrls$City[bigcityWebsiteUrls$City=="New York"] <- "New York City"
bigcityWebsiteUrls$State_City <- paste(bigcityWebsiteUrls$State, bigcityWebsiteUrls$City, sep = "_")

save(bigcityWebsiteUrls, file =  "rfiles/bigcityWebsiteURLs.rdata")

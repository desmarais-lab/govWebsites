# Purpose of the script:
## 1. Scrape URLs and partisanship of mayor from Wikipedia
## 2. merge in partisanship from LEAP election data
## 3. some data munging to get everything ready to be processed in mergeStatesURLs.R

#Required files:
## functions/scrapeCityUrlsMayors.R
## data/indianaElections2015.rdata

#Created files:
## rfiles/indianaWebsiteURLs.rdata

library('rio')
library('dplyr')
library('tidyr')
library('stringr')
library('tibble')

source('functions/scrapeCityUrlsMayors.R')

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Louisiana"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[1]] 
df <- mytable %>% html_table(fill=T)
df <- df[-1,]
df <- df[-nrow(df),]

#LA has [c] symbols in the table that need to be removed
df$Name <- str_replace(df$Name, "\\[(.*?)\\]", "")
df$Name <- str_trim(df$Name)

#use the get_link function on all the cities
df$wiki_link <- sapply(df$Name, function(x)get_link(mytable, x))

#scrape the mayors and city URLs
df$mayor <- NA
df$CityWebsite <- NA
for(i in 1:nrow(df)){
  
  #page link
  link <- str_c("https://en.wikipedia.org", df$wiki_link[i])
  #read the page html
  page <- read_html(link)
  #get all the tables on the page
  allTables <- page %>% html_nodes("table")
  
  ## Get the mayor
  df$mayor[i] <- findTableElement(allTables, "Mayor")
  
  ## Get the city website URL
  #First try:
  cityWebsiteURL <- extractWebsite(page)
  #Second try:
  if(cityWebsiteURL==""){
    cityWebsiteURL <- try(findTableLink(allTables, "Website"))
  }
  #Last try:
  if(cityWebsiteURL==""){
    cityWebsiteURL <- findTableElement(allTables, "Website")
  }
  
  df$CityWebsite[i] <- cityWebsiteURL
  
}

#get partisanship from wikipedia
df$wikiPartisanship <- NA
df$wikiPartisanship[str_detect(df$mayor, "R\\)")] <- "R"
df$wikiPartisanship[str_detect(df$mayor, "Republican")] <- "R"
df$wikiPartisanship[str_detect(df$mayor, "D\\)")] <- "D"
df$wikiPartisanship[str_detect(df$mayor, "Democrat")] <- "D"
df$wikiPartisanship[str_detect(df$mayor, "I\\)")] <- "I"
df$wikiPartisanship[str_detect(df$mayor, "No party")] <- "I"

#fix some errors
df$mayor <- str_replace_all(df$mayor, "c\"", "")
df$mayor <- str_replace_all(df$mayor, "\"", "")
df$mayor <- str_replace_all(df$mayor, "\\(?D\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\(?R\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\(?I\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\((.*?)\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\[(.*?)\\]", "")
df$mayor <- str_replace_all(df$mayor, "\\[", "")
df$mayor <- str_replace_all(df$mayor, "\\]", "")
df$mayor <- str_replace_all(df$mayor, "\\(", "")
df$mayor <- str_replace_all(df$mayor, "\\)", "")
df$mayor <- str_replace_all(df$mayor, "[0-9]", "")
#delete everything after, as well as including the following
df$mayor <- str_replace_all(df$mayor, "No [p|P]arty(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "Republican(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "Democrat(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "Independent(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "Police(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "elected(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "acting(.*?)$", "")
df$mayor <- str_replace_all(df$mayor, "-present(.*?)$", "")
df$mayor[df$Name=="Greensburg"] <- "Amanda Ficklin-Mixon"
df$mayor[df$Name=="Kinder"] <- ""
df$mayor[df$Name=="Lafayette"] <- ""
df$wikiPartisanship[df$Name=="Lafayette"] <- NA
df$mayor[df$Name=="Minden"] <- "Marvin Thomas 'Tommy' Davis"
df$mayor[df$Name=="Port Allen"] <- "Demetric 'Deedy' Slaughter"
df$wikiPartisanship[df$Name=="Port Allen"] <- NA
df$mayor[df$Name=="Rayville"] <- "Harry Lewis"
df$mayor[df$Name=="Roseland"] <- ""
df$mayor[df$Name=="St. Martinville"] <- "Thomas Nelson"
df$mayor[df$Name=="Sarepta"] <- "Peggy Adkins"
df$mayor[df$Name=="Slaughter"] <- "'Robbie' Jackson"
df$mayor[df$Name=="Sulphur"] <- "Christopher L. Duncan"
df$mayor[df$Name=="Vidalia"] <- "Edwy Gene 'Buz' Craft"
df$mayor[df$Name=="Winnfield"] <- "Kiah Beville"
df$wikiPartisanship[df$Name=="Winnfield"] <- "R"
df$mayor[df$Name=="Youngsville"] <- "Ken Ritter"

#corrections to websites
df$CityWebsite[df$CityWebsite=="http://www.JonesboroLA.org"] <- "http://www.jonesborola.org"
df$CityWebsite[df$CityWebsite=="http://www.cityofmarksville.com/home.html"] <- "http://www.cityofmarksville.com"
df$CityWebsite[df$CityWebsite=="http://www.ci.monroe.la.us/"] <- "https://monroela.us"
df$CityWebsite[df$Name=="Albany"] <- ""

#remove sites pointing to the wayback machine
df$CityWebsite[str_detect(df$CityWebsite, "https://web.archive.org")] <- ""

#merge in the Louisiana election data
leap <- as.tibble(import("data/LEAP_Louisiana_All_Offices_neumann.xlsx"))
#select relevant vars
leap <-  select(leap, c(results_election_id, office_name, year_int, entity_name, 
                        political_party_name, candidate_pct, candidate_name, candidate_total_votes,
                        office_name, fips_place_location))
#find races that are for mayor
leap$mayor <- str_detect(leap$office_name, "Mayor")
leap <- leap[leap$mayor==T,]
#kick out old data
leap <- leap[leap$year_int>=2014,]
#give correct city names to NAs out NAs
NAs <- which(is.na(leap$entity_name)==T)
leap$entity_name[NAs] <- str_replace(leap$fips_place_location[NAs], "Mayor -- City of ", "")
#there are some duplicates
leap <- select(leap, -c(office_name, fips_place_location))
leap <- unique(leap)
#take care of the fact that some candidates appear more than once
#because some cities have two election ids because they span counties
leap2 <- select(leap, candidate_total_votes, candidate_name)
leap3 <- aggregate(leap2$candidate_total_votes, list(leap2$candidate_name), sum)
leap <- select(leap, -c(candidate_pct, candidate_total_votes, mayor, results_election_id))
leap <- unique(leap)
#kick out turkey creek 2014 because the result was annulled because of vote buying o.O
leap <- leap[-which(leap$entity_name=="Turkey Creek" & leap$year_int=="2014"),]
leap <- merge(leap3, leap, by.x = "Group.1", by.y = "candidate_name", all.x = T)
rm(list = c("leap2", "leap3", "NAs"))
#rename
names(leap) <- c("Candidate", "Votes", "Year", "City", "Party")
#apparently the leap project did not code everyone's party even if it is in the name string
leap$Party[str_detect(leap$Candidate, "\\(R\\)") & leap$Party=="Unknown"] <- "Republican"
leap$Party[str_detect(leap$Candidate, "\\(D\\)") & leap$Party=="Unknown"] <- "Democratic"
#for each city, check which party has the most votes
cities <- sort(unique(leap$City), decreasing = F)
leap$winner <- NA
for(i in 1:length(cities)){
  rows <- which(leap$City==cities[i])
  maxrow <- which.max(leap$Votes[rows])
  winner <- leap$Party[rows][maxrow]
  leap$winner[which(leap$City==cities[i])] <- winner
}
#remove irrelevant variables and rows, rename vars
leap <-  select(leap, c(City, winner))
leap <- unique(leap)
names(leap) <- c("City", "leapParty")

#merge with wikipedia data
df <- merge(df, leap, by.x = "Name", by.y = "City", all = T)

#re-name and subset data frame to the variables we need
df <- subset(df, select = c("Name", "wiki_link", "mayor", "CityWebsite", "wikiPartisanship", "leapParty"))
louisianaWebsiteUrls <- df
names(louisianaWebsiteUrls) <- c("City", "wiki_link", "wikiMayor", "wikiCityWebsite", "wikiPartisanship", "leapParty")

#save
save(louisianaWebsiteUrls, file="rfiles/louisianaWebsiteURLs.rdata")

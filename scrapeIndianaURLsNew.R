# Purpose of the script:
## 1. Scrape URLs and partisanship of mayor from Wikipedia
## 2. merge in partisanship from official election data
## 3. some data munging to get everything ready to be processed in mergeStatesURLs.R

#Required files:
## functions/scrapeCityUrlsMayors.R
## data/indianaElections2015.rdata

#Created files:
## rfiles/indianaWebsiteURLs.rdata


source('functions/scrapeCityUrlsMayors.R')

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Indiana"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[14]] 
df <- mytable %>% html_table(fill=T)

#use the get_link function on all the cities
df$wiki_link <- sapply(df$City, function(x)get_link(mytable, x))

#corrections
df$wiki_link[df$City=="Clinton"] <- "/wiki/Clinton,_Indiana"
df$wiki_link[df$City=="Decatur"] <- "/wiki/Decatur,_Indiana"
df$wiki_link[df$City=="Knox"] <- "/wiki/Knox,_Indiana"
df$wiki_link[df$City=="Madison"] <- "/wiki/Madison,_Indiana"
df$wiki_link[df$City=="Marion"] <- "/wiki/Marion,_Indiana"

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
df$wikiPartisanship[str_detect(df$mayor, "D\\)")] <- "D"
df$wikiPartisanship[str_detect(df$mayor, "I\\)")] <- "I"

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
df$mayor <- str_replace_all(df$mayor, "[-|–][p|P]resent", "")

#corrections
df$CityWebsite[str_detect(df$CityWebsite, "subscript out of bounds")] <- ""
df$CityWebsite[df$CityWebsite=="#cite_note-GR3-2"] <- ""
df$CityWebsite[df$City=="Connersville"] <- "http://connersvillecommunity.com/City_of_Connersville"
df$CityWebsite[df$City=="Dunkirk"] <- "www.cityofdunkirkin.com"

#remove sites pointing to the wayback machine
df$CityWebsite[str_detect(df$CityWebsite, "https://web.archive.org")] <- ""

#merge in the Indiana election data
load("data/indianaElections2015.rdata")
mIN <- subset(mIN, Year==2015)
mIN <- data.frame(City = mIN$District, officialParty = mIN$winner)
df <- merge(df, mIN, by = "City", all.x = T)
rm(IN)

#re-name and subset data frame to the variables we need
df <- subset(df, select = c("City", "County", "wiki_link", "mayor", "CityWebsite", "wikiPartisanship", "officialParty"))
names(df) <- c("City", "wikiCounty", "wiki_link", "wikiMayor", "wikiCityWebsite", "wikiPartisanship", "officialParty")
indianaWebsiteUrls <- df

#save
save(indianaWebsiteUrls, file="rfiles/indianaWebsiteURLs.rdata")

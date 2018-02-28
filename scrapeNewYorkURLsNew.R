source('functions/scrapeCityUrlsMayors.R')

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_New_York"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[2]] 
df <- mytable %>% html_table(fill=T)

#use the get_link function on all the cities
df$wiki_link <- sapply(df$City, function(x)get_link(mytable, x))

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

newyorkWebsiteUrls <- df
names(newyorkWebsiteUrls) <- c("City", "wikiCounty", "wikiPopulation", "wikiIncorporationDate", "wiki_link", "wikiMayor", "wikiCityWebsite", "wikiPartisanship")

#remove sites pointing to the wayback machine
newyorkWebsiteUrls$Website[str_detect(newyorkWebsiteUrls$wikiCityWebsite, "https://web.archive.org")] <- ""

#get the base URL
newyorkWebsiteUrls$wikiCityWebsite <- str_extract(newyorkWebsiteUrls$wikiCityWebsite, "^.+?[^\\/:](?=[?\\/]|$)")

#corrections to mayor names:
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$City=="Cohoes"] <- "Shawn Morse"
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$City=="Hudson"] <- "Tiffany Martin Hamilton"
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$City=="Middletown"] <- "Joseph M. DeStefano"
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$City=="Mount Vernon"] <- "Richard Thomas"
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$City=="Rensselaer"] <- "Daniel J. Dwyer "
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$City=="Salamanca"] <- "Michael R. 'Smitty' Smith "
# the mayor of Salamanca is a Democrat according to this: 
# http://www.salamancapress.com/news/smith-running-for-mayor-to-reunite-salamanca/article_38be520e-a124-11e6-968e-5f1fde718ef3.html
# but he was also endorsed by the Republican party
# coding him as a Democrat
newyorkWebsiteUrls$wikiPartisanship[newyorkWebsiteUrls$City=="Salamanca"] <- "D"

#code empty mayors and websites as NA
newyorkWebsiteUrls$wikiMayor[newyorkWebsiteUrls$wikiMayor==""] <- NA
newyorkWebsiteUrls$wikiCityWebsite[newyorkWebsiteUrls$wikiCityWebsite==""] <- NA

#save
save(newyorkWebsiteUrls, file="data/newyorkWebsiteURLs2.rdata")

source('functions/scrapeCityUrlsMayors.R')

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Indiana"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[14]] 
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

#fix some errors
df$mayor <- str_replace_all(df$mayor, "c\"", "")
df$mayor <- str_replace_all(df$mayor, "\"", "")
df$mayor <- str_replace_all(df$mayor, "\\(?D\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\(?R\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\((.*?)\\)", "")
df$mayor <- str_replace_all(df$mayor, "\\[(.*?)\\]", "")
df$mayor <- str_replace_all(df$mayor, "\\[", "")
df$mayor <- str_replace_all(df$mayor, "\\]", "")
df$mayor <- str_replace_all(df$mayor, "\\(", "")
df$mayor <- str_replace_all(df$mayor, "\\)", "")
df$mayor <- str_replace_all(df$mayor, "[0-9]", "")
df$mayor <- str_replace_all(df$mayor, "[-|–][p|P]resent", "")

df$CityWebsite[str_detect(df$CityWebsite, "subscript out of bounds")] <- ""
df$CityWebsite[df$CityWebsite=="#cite_note-GR3-2"] <- ""

save(df, file = "rfiles/IN_city_URLs.rdata")

indianaWebsiteUrls <- subset(df, select = c("City", "CityWebsite"))
names(indianaWebsiteUrls) <- c("Name", "Website")
indianaWebsiteUrls$Designation <- "City"

#remove sites pointing to the wayback machine
indianaWebsiteUrls$Website[str_detect(indianaWebsiteUrls$Website, "https://web.archive.org")] <- ""

#get the base URL
indianaWebsiteUrls$Website <- str_extract(indianaWebsiteUrls$Website, "^.+?[^\\/:](?=[?\\/]|$)")

#save
save(indianaWebsiteUrls, file="data/indianaWebsiteURLs2.rdata")

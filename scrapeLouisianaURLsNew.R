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

save(df, file = "rfiles/LA_city_URLs.rdata")

louisianaWebsiteUrls <- subset(df, select = c("Name", "CityWebsite"))
names(louisianaWebsiteUrls) <- c("Name", "Website")
louisianaWebsiteUrls$Designation <- NA

#remove sites pointing to the wayback machine
louisianaWebsiteUrls$Website[str_detect(louisianaWebsiteUrls$Website, "https://web.archive.org")] <- ""

#get the base URL
louisianaWebsiteUrls$Website <- str_extract(louisianaWebsiteUrls$Website, "^.+?[^\\/:](?=[?\\/]|$)")

#save
save(louisianaWebsiteUrls, file="data/louisianaWebsiteURLs2.rdata")

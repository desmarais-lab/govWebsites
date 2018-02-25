source('functions/scrapeCityUrlsMayors.R')

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Oregon"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[2]] 
df <- mytable %>% html_table(fill=T)

#Oregon has † symbols in the table that need to be removed
df$City <- str_replace_all(df$City, "†", "")
df$City <- str_trim(df$City)

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

df$mayor[df$mayor=="Vacant"] <- ""
df$mayor[df$wiki_link=="/wiki/Antelope,_Oregon"] <- ""

df$CityWebsite[str_detect(df$CityWebsite, "Error in ")] <- ""
df$CityWebsite[str_detect(df$CityWebsite, "https://web.archive.org")] <- ""
df$CityWebsite[str_detect(df$CityWebsite, "#cite_note-City_info-1")] <- ""

save(df, file = "rfiles/OR_city_URLs.rdata")


df <- df[df$CityWebsite!="",]
df <- df[df$mayor!="",]

writeLines(df$mayor, "data/OR_mayors.txt")

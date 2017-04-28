library("rvest")
library("stringr")

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_municipalities_in_Louisiana"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[1]] 
df <- mytable %>% html_table(fill=T)
df$Name <- str_replace(df$Name, "\\[(.*?)\\]", "") #correct an error in the city names
df <- df[-c(1,nrow(df)),]

get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$wiki_link <- sapply(df$Name, function(x)get_link(mytable, x))


#function to extract the links to the city websites
extractWebsite <- function(wikipage){
  link <- str_c("https://en.wikipedia.org", wikipage)
  citylink <- read_html(link) %>% html_nodes(".url .text") %>% html_attr("href")
  if(identical(citylink, character(0))){
    citylink <- NA #NA if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}
df$website <- sapply(df$wiki_link, extractWebsite)

#df <- subset(df, select=c("Name","website"))
louisianaWebsiteUrls <- subset(df, select=c("Name", "website"))
names(louisianaWebsiteUrls) <- c("Name", "Website")
louisianaWebsiteUrls$Designation <- NA

#save
save(louisianaWebsiteUrls, file="data/LouisianaWebsiteURLs.rdata")

#Note: there are no cites with identical names, so the counties column is not needed
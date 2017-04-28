#This script is used to scrape the URLs of Indiana city and town website URLs from Wikipedia

library("rvest")
library("stringr")

#Indiana CITY websites
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Indiana"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[14]]
df <- mytable %>% html_table(fill=T)

#get the link to the city's Wikipedia page
get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$wiki_link <- sapply(df$City, function(x)get_link(mytable, x))

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


indianacities <- subset(df, select=c("City", "website"))
names(indianacities) <- c("Name", "Website")
indianacities$Designation <- "City"


#Indiana TOWN websites
url <- "https://en.wikipedia.org/wiki/List_of_towns_in_Indiana"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[1]]
df <- mytable %>% html_table(fill=T)

#get the link to the town's Wikipedia page
get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$wiki_link <- sapply(df$Town, function(x)get_link(mytable, x))

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

indianatowns <- subset(df, select=c("Town", "website"))
names(indianatowns) <- c("Name", "Website")
indianatowns$Designation <- "Town"

#combine cities and towns
indianaWebsiteUrls <- rbind(indianacities, indianatowns)
#save
save(indianaWebsiteUrls, file="data/indianaWebsiteURLs.rdata")

#Note: there are no cites/towns with identical names, so the counties column is not needed
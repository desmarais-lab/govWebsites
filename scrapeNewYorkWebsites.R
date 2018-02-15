library("rvest")
library("stringr")

#extract the links to the cities' wikipedia pages
load("rfiles/NY_mayors.rdata")

df <- data
#df$website <- paste0("https://en.wikipedia.org", df$wiki)

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
df$website <- sapply(df$wiki, extractWebsite)

#save
save(louisianaWebsiteUrls, file="data/NewYorkWebsiteURLs.rdata")
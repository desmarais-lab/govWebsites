library("rvest")
library("stringr")

load("rfiles/allURLs.rdata")

extractCoords <- function(wikipage){
  link <- str_c("https://en.wikipedia.org", wikipage)
  citylink <- read_html(link) %>% html_nodes("#coordinates span") %>% html_text()
  if(identical(citylink, character(0))){
    citylink <- NA #NA if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}
websiteUrls$coords <- sapply(websiteUrls$wiki_link, extractCoords)

coords <- str_split_fixed(websiteUrls$coords, "/", 3)[,3] %>% str_split_fixed(";", 2)
websiteUrls$latitude <- str_trim(coords[,1])
websiteUrls$longitude <- str_trim(coords[,2])

save(websiteUrls, file = "rfiles/allURLs.rdata")

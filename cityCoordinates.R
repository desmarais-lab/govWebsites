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

#load("rfiles/d.Rdata")
#df <- df[df$City%in%unique(d$Name),]


#function to extract the links to the city websites
extractCoords <- function(wikipage){
  link <- str_c("https://en.wikipedia.org", wikipage)
  citylink <- read_html(link) %>% html_nodes("#coordinates span") %>% html_text()
  if(identical(citylink, character(0))){
    citylink <- NA #NA if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}
df$coords <- sapply(df$wiki_link, extractCoords)


indianacities <- subset(df, select=c("City", "County", "wiki_link", "coords"))

d <- merge(d, indianacities, by.x = "Name", by.y = "City", all = T)

coords <- str_split_fixed(d$coords, "/", 3)[,3] %>% str_split_fixed(";", 2)
d$latitude <- str_trim(coords[,1])
d$longitude <- str_trim(coords[,2])

d <- subset(d, select=-coords)

#save
save(d, file="rfiles/d_coords.Rdata")

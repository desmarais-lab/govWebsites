library("rvest")
library("stringr")

#City websites
url <- "https://ballotpedia.org/List_of_current_mayors_of_the_top_100_cities_in_the_United_States"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[2]]
df <- mytable %>% html_table(fill=T)
names(df) <- df[1,]
df <- df[-1,]
df$party <- str_extract(df$Mayor, "\\(([^\\)]+)\\)")
df$Mayor2 <- str_extract(df$Mayor, "[^(]*")
df$Mayor2 <- str_trim(df$Mayor2)

#get the link to the city's Wikipedia page
get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$wiki_link <- sapply(df$City, function(x)get_link(mytable, x))
df$wiki_link2 <- sapply(df$Mayor2, function(x)get_link(mytable, x))


#function to extract the links to the campaign websites
extractWebsite <- function(wikipage){
  link <- str_c("https://ballotpedia.org", wikipage)
  citylink <- read_html(link) %>% html_nodes(xpath=paste0("//a[text()='", "Campaign website", "']")) %>% html_attr("href")
  if(identical(citylink, character(0))){
    citylink <- NA #NA if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}
df$campaign_website <- sapply(df$wiki_link2, extractWebsite)

#function to extract the links to the campaign websites
extractCityWebsite <- function(wikipage){
  link <- str_c("https://ballotpedia.org", wikipage)
  citylink <- read_html(link) %>% html_nodes(xpath=paste0("//a[text()='", "City website", "']")) %>% html_attr("href")
  if(identical(citylink, character(0))){
    citylink <- NA #NA if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}
df$city_website <- sapply(df$wiki_link, extractCityWebsite)


#remove independent mayors
df <- df[is.na(df$party)==F,]
df <- df[df$party!="(I)",]

save(df, file = "rfiles/campaign_websites.rdata")
load("rfiles/campaign_websites.rdata")

#remove mayors without a campaign website
df <- df[is.na(df$campaign_website)==F,]

df$num <- 1:nrow(df)

#get the row numbers of the republican mayors
rep_mayors <- which(df$party=="(R)")

#randomly pick one partnered democratic
dem_mayors <- numeric(length(rep_mayors))
for(i in 1:length(rep_mayors)){
  dem_mayors[i] <- rep_mayors[i]+sample(c(-1,1), 1)
}

df2 <- df[sort(c(rep_mayors, dem_mayors)),]

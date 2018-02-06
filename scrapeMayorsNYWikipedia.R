library("rvest")
library("stringr")

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_New_York"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[2]] 
df <- mytable %>% html_table(fill=T)

get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$wiki_link <- sapply(df$City, function(x)get_link(mytable, x))


df$mayor <- NA


link <- str_c("https://en.wikipedia.org", df$wiki_link[1])

page <- read_html(link)

for(i in 1:nrow(df)){
ppage <- readLines(paste0("https://en.wikipedia.org", df$wiki_link[i]))
#mayorLine <- which(str_detect(ppage, "<th scope=\"row\">&#160;•&#160;Mayor</th>"))
#also get the next two lines
#mayorLine23 <- ppage[mayorLine:(mayorLine+2)]
#the mayor is the longest line
#mayor <- mayorLine23[which.max(nchar(mayorLine23))]
#df$mayor[i] <- mayor
mayorLine <- which(str_detect(ppage, "Democratic_Party|Republican_Party"))

}

for(i in 1:nrow(df)){
url2 <- paste0("https://en.wikipedia.org", df$wiki_link[i])
mytable2 <- read_html(url2) %>% html_nodes("table") %>% .[[1]] 
df3 <- mytable2 %>% html_table(fill=T)
mayorline <- df3[which(str_detect(df3[,1], "Mayor")),]
mayor <- str_extract(mayorline, "^.*\\([R|D]\\)")
if(length(mayor)>1){
  mayor <- mayor[is.na(mayor)==F]
}
if(length(mayor)==0){
  mayor <- ""
}
df$mayor[i] <- mayor
}

save(df, file = "rfiles/NYmunicipalitiesWiki.rdata")

load("./rfiles/scrapedCampaignFinanceNY.rdata")

df2 <- subset(df, select = c("City", "mayor"))

data2 <- merge(data, df, by.x = "Municipality", by.y = "City")

#save(data2, file = "rfiles/NYmayors.rdata")

data2$strdist <- stringdist::stringdist(data2$`Filer Name`, data2$mayor, method="jaccard")
data2$strdist2 <- stringdist::stringdist(data2$`Filer Name`, data2$mayor, method="cosine")
data2$strdist2 <- stringdist::stringdist(data2$`Filer Name`, data2$mayor, method="soundex")

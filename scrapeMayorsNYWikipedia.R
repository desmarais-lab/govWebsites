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


#change filer names for better matches

del <- c("FOR MAYOR", "MAYOR", "FRIENDS OF", "[0-9]", "COMMITTEE TO ELECT", "FOR")

filer <- data2$`Filer Name`

library(stringr)

data2$filer_name <- data2$`Filer Name`


data2$cand_num <- 1:nrow(data2)


data2$wiki_link['/wiki/Oswego_County,_New_York'] <- "/wiki/Oswego,_New_York"

#ambiguous: 123,124, picked 123
#175, 176, picked 175 because donation

#mayors_numL <- list(10,16,19,21,NA,31,43,48,NA,NA,62,NA,71,83,NA,85,88,94,99,105,NA,110,123,NA,128,134,135,141,147,NA,162,NA,170,175,181,187,192,195,200,209,NA,220,NA,226,235,240,259,269,277,289,290,293,305)
mayors_num <- c(10,16,19,21,NA,31,43,48,NA,NA,62,NA,71,83,NA,85,88,94,99,105,NA,110,123,NA,128,134,135,141,147,NA,162,NA,170,175,181,187,192,195,200,209,NA,220,NA,226,235,240,259,269,277,289,290,293,305)

data <- data.frame(city = unique(data2$Municipality), wiki = unique(data2$wiki_link))
data$REP <- data2$REP[mayors_num]
data$DEM <- data2$DEM[mayors_num]
data$NEITHER <- data2$NEITHER[mayors_num]
data$mayor <- data2$mayor[mayors_num]

table(data$DEM)
table(data$REP)

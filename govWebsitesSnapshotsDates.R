library(rvest)
library(stringr)
require(jsonlite)
require(tidyr)
library(lubridate)
library(tidyverse)


load(file="data/indiana2015.rdata")

indiana$snapshots <- NA

for (j in 1:nrow(indiana)){

URL <- indiana$redirect[j]

#get the range of years for which snapshots are available
earliest <- fromJSON(str_c("http://archive.org/wayback/available?url=", URL, "&timestamp=20050101"))
earliest <- str_sub(earliest$archived_snapshots$closest$timestamp, 1, 4) #year
latest <- fromJSON(str_c("http://archive.org/wayback/available?url=", URL, "&timestamp=20180101"))
latest <- str_sub(latest$archived_snapshots$closest$timestamp, 1, 4) #year

#years to search
years <- as.numeric(earliest):as.numeric(latest)

#character vector for storing snapshot dates in
dates <- character()

for (i in 1:length(years)){

#for some reason the website bugs out if the month is set to january
url <- str_c("https://web.archive.org/web/", years[i], "0701000000*/", URL)
website <- read_html(url)

text <- html_nodes(website, '.captures') %>% html_nodes('h3')
text <- gsub("<h3>", "", text)
text <- gsub("</h3>", "", text)

dates <- append(dates,text)
}

dates <- list(dates)
indiana$snapshots[j] <- dates

}


#
indiana <- subset(indiana, select=c("District","snapshots"))

#save(indiana, file = "indianaSnapshots.rdata")
load("indianaSnapshots.rdata")

Snapshots <- spread(indiana, key = District, value = snapshots) %>%
  apply(2,unlist)
Snapshots <- plyr::ldply(Snapshots, cbind)

names(Snapshots) <- c("City","Snapshot")

#split date string into usable variables
Snapshots <- cbind(Snapshots, apply(data.frame(do.call(rbind, strsplit(as.character(Snapshots$Snapshot), split = " "))),2,as.character))

Snapshots$day <- str_c(Snapshots$X3,Snapshots$X2,Snapshots$X6, sep="-")
Snapshots <- as_tibble(Snapshots)

Snapshots$day <- dmy(Snapshots$day)

Snapshots$days <- ymd("2017-12-31")-Snapshots$day


### Plot

Snapshots$City <- factor(Snapshots$City)
Snapshots$City <- factor(Snapshots$City, levels = rev(levels(Snapshots$City)))

g1 <- ggplot(Snapshots, aes(x=day, y=City)) +
  geom_point() +
  geom_vline(xintercept = c(as.numeric(as.Date("2015-11-03")), as.numeric(as.Date("2011-11-08")), as.numeric(as.Date("2007-11-08")))) +
  labs(x = "", y = "")
g1


ggsave(g1, file="paper/figures/Snapshots.pdf", height = 10, width = 10)


### Count number of snapshots
Snapshots$one <- 1

indianaS <- aggregate(one ~ City, Snapshots, FUN=sum)

setwd("websites/IndianaTest/")
#folder size
#get folder names
d <- list.files()
e <- vector()
#print directory size with Linux command 'du -s'
for(i in 1:length(d)){
  e[i] <- system(paste("du",d[i],"-s"), intern=T)
}
require(reshape2)
f <- colsplit(e,"\\t",c("Size (KB)","Website"))
f$Website <- sub("www.", "", f$Website)
f$Website <- sub("cityofrockport", "rockport", f$Website)
f <- f[order(f$Website),]

storagesize <- cbind(indianaS,f)

storagesize$result <- storagesize$`Size (MB)`*storagesize$one

sum(storagesize$result)/1000000 #in gigabyte

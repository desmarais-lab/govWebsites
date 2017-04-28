

library(dplyr)
library(tidyr)

setwd("D:/Dropbox/4_RA/govWebsites") #Windows
setwd("~/Dropbox/4_RA/govWebsites") #Linux

#2015
mIN15 <- read.csv("data/mayorIN2015.csv", header = F)

mIN15$V1 <- as.character(mIN15$V1)
mIN15$V2 <- as.character(mIN15$V2)
mIN15$V3 <- as.character(mIN15$V3)

#clean up data - put city name, candidate name and votes in the correct column
for(i in 1:nrow(mIN15)){
  if(is.na(mIN15$V3[i])==T){
    mIN15$V3[i] <- mIN15$V2[i]
    mIN15$V2[i] <- mIN15$V1[i]
    mIN15$V1[i] <- mIN15$V1[i-1]
    }
}

mIN15$V3 <- as.numeric(mIN15$V3)

#extract party
library(stringr)
mIN15$V4 <- str_extract(mIN15$V2,"\\((.+?)\\)$")
mIN15$V5 <- str_extract(mIN15$V4," \\((.+?)\\)$")
mIN15$V5 <- str_trim(mIN15$V5)
mIN15$V4[is.na(mIN15$V5)==F] <- mIN15$V5[is.na(mIN15$V5)==F]
#mIN15$V4 <- gsub("log\\(", "", mIN15$V4)
mIN15$V4 <- gsub("(", "", mIN15$V4, fixed="TRUE")
mIN15$V4 <- gsub(")", "", mIN15$V4, fixed="TRUE")

mIN15$V5 <- 2015

#2011
mIN11 <- read.csv("data/mayorIN2011compatibility.csv", header = T)
mIN11$District <- as.character(mIN11$District)

for(i in 1:nrow(mIN11)){
  if(mIN11$District[i]==""){
    mIN11$District[i] <- mIN11$District[i-1]
  }
}

#extract party
library(stringr)
mIN11$Party <- str_extract(mIN11$Candidate,"\\((.+?)\\)$")
mIN11$Party2 <- str_extract(mIN11$Party,"\\s\\((.+?)\\)$")
mIN11$Party2 <- str_trim(mIN11$Party2)
mIN11$Party[is.na(mIN11$Party2)==F] <- mIN11$Party2[is.na(mIN11$Party2)==F]
#mIN11$Party <- gsub("log\\(", "", mIN11$Party)
mIN11$Party <- gsub("(", "", mIN11$Party, fixed="TRUE")
mIN11$Party <- gsub(")", "", mIN11$Party, fixed="TRUE")

names(mIN11)[5] <- "Year"
mIN11$Year <- 2011

#rename 2015
names(mIN15) <- names(mIN11)

mIN15 <- subset(mIN15, select = -Candidate)
mIN15 <- subset(mIN15, Party %in% c("Democratic","Republican"))

mIN15$District <- factor(mIN15$District)
mIN15 <- mIN15 %>% spread(Party, Votes)
mIN15$Democratic[is.na(mIN15$Democratic)==T] <- -1
mIN15$Republican[is.na(mIN15$Republican)==T] <- -1
mIN15$winner <- names(mIN15)[3:4][max.col(mIN15[,3:4])]

#do the same for 2011
mIN11 <- subset(mIN11, select = -Candidate)
mIN11 <- subset(mIN11, Party %in% c("Democratic","Republican"))
mIN11$District <- factor(mIN11$District)
mIN11 <- mIN11 %>% spread(Party, Votes)
mIN11$Democratic[is.na(mIN11$Democratic)==T] <- -1
mIN11$Republican[is.na(mIN11$Republican)==T] <- -1
mIN11$winner <- names(mIN11)[3:4][max.col(mIN11[,3:4])]


mIN <- rbind(mIN11,mIN15)
mIN <- mIN[order(mIN$District,mIN$Year),]
mIN <- mIN[-c(237:239),] #remove last 3 because they dont appear in 2011

mIN$control_change <- 0

for(i in 2:nrow(mIN)){
  if(mIN$winner[i]!=mIN$winner[i-1])
  mIN$control_change[i] <- 1
}
mIN$control_change[mIN$Year==2011] <- NA

#party control changed
table(mIN$control_change) #35 out of 118 times
table(mIN$control_change)[2]/sum(table(mIN$control_change)) #29.66%

#set -1s back to NA
mIN$Democratic[mIN$Democratic==-1] <- NA
mIN$Republican[mIN$Republican==-1] <- NA

#mIN <- mIN[mIN$Year==2015,]



#load websites data
load("data/govWebsitesVerifiedCensus.Rdata")
#only Indiana
data9 <- subset(data9,State=="IN")
#check overlap
match(data9$City,mIN$District)
#how many matches?
length(match(data9$City,mIN$District)[is.na(match(data9$City,mIN$District))==F]) #17
#names of matches
mIN$District[match(data9$City,mIN$District)[is.na(match(data9$City,mIN$District))==F]]
#merge
combined <- merge(mIN,data9,by.x = "District", by.y = "City", all.x = F, all.y = F)
#only 2015
indiana <- subset(combined, Year==2015)
#save
save(indiana, file="data/indiana2015.rdata")
load(file="data/indiana2015.rdata")

#correction to indianapolis
indiana$redirect[indiana$District=="Indianapolis"] <- "http://www.indy.gov"

#
indiana.table <- subset(indiana, select=c("District","Democratic","Republican",
                                          "winner","control_change","POPESTIMATE2015",
                                          "redirect"))
names(indiana.table) <- c("City","DemVotes","RepVotes",
                          "Winner","Change","Pop15",
                          "url")

require(xtable)
print(xtable(indiana.table, caption="", digits = 0), include.rownames = F)


### pull websites from wayback machine
library(jsonlite)

API_base <- 'http://archive.org/wayback/available?url='

test <- indiana$redirect

#set up folders
setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/")
system("mkdir oct15")
system("mkdir jan16")
setwd("/home/markus/Dropbox/4_RA/govWebsites/websites/jan16/")
#loop through websites, results automatically get saved into 'websites' folder inside wd
for (i in 1:length(test)){
  
  website <- test[i] #loop through websites
  
  #the following three lines aren't actually needed when using the Ruby package
  API_URL <- paste(API_base,website,sep = "")
  wayback <- fromJSON(API_URL)
  waybackURL <- wayback$archived_snapshots$closest$url
  
  #pasting input for Ruby package, then executing it
  #--concurrency 20 causes 20 items to be downloaded at the same time
  #the default is 1, this takes WAY too long (i.e. one hour for a website...)
  #--from 201510 downloads a snapshot from October 2015, or, if not available, later
  
  #The mayoral elections in IN happened on November 3
  WBMD_base <- "wayback_machine_downloader --concurrency 40 --from 201601"
  #WBMD_base <- "wayback_machine_downloader --concurrency 40 --from 201510"
  WBMD_site <- paste(WBMD_base,website)
  system(WBMD_site, intern = T) #just ignore the printout if running outside of loop
}

#setwd("D:/Dropbox/4_RA/govWebsites/websites/pdfsfolder/")

list.files(path='websites/frankfort-in.gov/', pattern='*.pdf', recursive=T)

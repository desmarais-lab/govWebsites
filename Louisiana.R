library('rio')
library('dplyr')
library('tidyr')
library('stringr')
library('tibble')

leap <- as.tibble(import("data/LEAP_Louisiana_All_Offices_neumann.xlsx"))

#select relevant vars
leap <-  select(leap, c(results_election_id, office_name, year_int, entity_name, 
                        political_party_name, candidate_pct, candidate_name, candidate_total_votes,
                        office_name, fips_place_location))

#find races that are for mayor
leap$mayor <- str_detect(leap$office_name, "Mayor")
leap <- leap[leap$mayor==T,]

#kick out old data
leap <- leap[leap$year_int>=2014,]

#give correct city names to NAs out NAs
NAs <- which(is.na(leap$entity_name)==T)
leap$entity_name[NAs] <- str_replace(leap$fips_place_location[NAs], "Mayor -- City of ", "")

#there are some duplicates
leap <- select(leap, -c(office_name, fips_place_location))
leap <- unique(leap)

#take care of the fact that some candidates appear more than once
#because some cities have two election ids because they span counties
leap2 <- select(leap, candidate_total_votes, candidate_name)
leap3 <- aggregate(leap2$candidate_total_votes, list(leap2$candidate_name), sum)
leap <- select(leap, -c(candidate_pct, candidate_total_votes, mayor, results_election_id))
leap <- unique(leap)

#kick out turkey creek 2014 because the result was annulled because of vote buying o.O
leap <- leap[-which(leap$entity_name=="Turkey Creek" & leap$year_int=="2014"),]
leap <- merge(leap3, leap, by.x = "Group.1", by.y = "candidate_name", all.x = T)
rm(list = ls()[ls()!="leap"])

#rename
names(leap) <- c("Candidate", "Votes", "Year", "City", "Party")

#for each city, check which party has the most votes
cities <- sort(unique(leap$City), decreasing = F)
leap$winner <- NA
for(i in 1:length(cities)){
  rows <- which(leap$City==cities[i])
  maxrow <- which.max(leap$Votes[rows])
  winner <- leap$Party[rows][maxrow]
  leap$winner[which(leap$City==cities[i])] <- winner
}

#safer this way, because some cities have two election ids because they span counties
#for each city, check which party has the most votes
#el_id <- sort(unique(leap$results_election_id), decreasing = F)
#leap$winner <- NA
#for(i in 1:length(el_id)){
#  
#  rows <- which(leap$results_election_id==el_id[i])
#  maxrow <- which.max(leap$candidate_pct[rows])
#  winner <- leap$political_party_name[rows][maxrow]
#  leap$winner[which(leap$results_election_id==el_id[i])] <- winner
#  
#}

#remove irrelevant variables and rows, rename vars
leap <-  select(leap, c(Year, City, winner))
leap <- unique(leap)

#remove unknown
leap <- leap[leap$winner!="Unknown",]


## Load website URLs from Wikipedia
load(file="data/louisianaWebsiteURLs.rdata")

## Load website URLs from Wikipedia
load(file="data/govWebsitesVerifiedCensus.Rdata")
data9 <- subset(data9, select=c("NAME","redirect","StateShort"))
data9 <- subset(data9, StateShort%in%c("LA"))
data9$NAME[data9$NAME=="15885"] <- "Indianapolis"
data9$State <- data9$StateShort
data9$StateShort <- NA
data9$State_Name <- paste(data9$State, data9$NAME, sep="_")
names(data9) <- c("Name", "Website", "Designation", "State", "State_Name")

#Merge the two sets of URLs

louisiana <- merge(louisianaWebsiteUrls, subset(data9, select=c("Website", "Name")), by="Name", all = T)
#keeping the wikipedia URLs, because for the one case where it matters, the one
#from the .gov websites list redirects to that
louisiana$Website.x[is.na(louisiana$Website.x)==T] <- louisiana$Website.y[is.na(louisiana$Website.x)==T]
louisiana <- subset(louisiana, select=-Website.y)
names(louisiana)[names(louisiana)=="Website.x"] <- "Website"

#merge with website data
louisiana <- select(louisiana, -Designation)
louisiana <- merge(leap, louisiana, by.x ="City", by.y ="Name", all.x = T, all.y = F)

#remove cities without a website
louisiana <- louisiana[is.na(louisiana$Website)==F,]

#fix errors in the URLs
louisiana$Website[louisiana$Website=="http://www.JonesboroLA.org"] <- "http://www.jonesborola.org"
louisiana$Website[louisiana$Website=="http://www.cityofmarksville.com/home.html"] <- "http://www.cityofmarksville.com"
louisiana$Website[louisiana$Website=="http://www.ci.monroe.la.us/"] <- "https://monroela.us/"

#However, Marksville and Monroe can't be scraped with wget anyway

#19 Democratic cities, 11 Republican ones
table(louisiana$winner)

#save
save(louisiana, file = "data/louisiana.rdata")

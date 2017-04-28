library("rio")

load(file="data/louisianaWebsiteURLs.rdata")
load(file="data/IndianaWebsiteURLs.rdata")
load(file="data/govWebsitesVerifiedCensus.rdata")

#Preparing the data for merge

#.gov website data
data9 <- subset(data9, select=c("NAME","redirect","StateShort"))
data9 <- subset(data9, StateShort%in%c("IN","LA"))
data9$NAME[data9$NAME=="15885"] <- "Indianapolis"
data9$State <- data9$StateShort
data9$StateShort <- NA
data9$State_Name <- paste(data9$State, data9$NAME, sep="_")
names(data9) <- c("Name", "Website", "Designation", "State", "State_Name")

#indiana & louisiana URLs from Wikipedia
indianaWebsiteUrls$State <- "IN"
louisianaWebsiteUrls$State <- "LA"
URLs <- rbind(indianaWebsiteUrls, louisianaWebsiteUrls)
URLs$State_Name <- paste(URLs$State, URLs$Name, sep="_")

#merge
df <- merge(URLs, subset(data9, select=c("Website", "State_Name")), by="State_Name", all = T)
#the .gov URL is generally more reliable, so using that, if available
df$Website.x[is.na(df$Website.y)==F] <- df$Website.y[is.na(df$Website.y)==F]
df <- subset(df, select=-Website.y)
names(df)[names(df)=="Website.x"] <- "Website"

length(which(is.na(df$Website)==F))

#load LEAP data
leap <- import("data/LEAP_Louisiana_All_Offices_neumann.xlsx")

leap$mayor <- grepl("Mayor", leap$office_name)
leap <- subset(leap, mayor==T)
leap <- subset(leap, year_int>2010)

sort(unique(leap$entity_name)) #cities

length(unique(leap$results_election_id))

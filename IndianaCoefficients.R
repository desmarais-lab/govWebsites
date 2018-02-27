library("rio")
library("dplyr")
library("stringr")

load(file="data/indianaWebsiteURLs2.rdata")
load(file="data/govWebsitesVerifiedCensus.Rdata")

#Preparing the data for merge

#.gov website data
data9 <- subset(data9, select=c("NAME","redirect","StateShort"))
data9 <- subset(data9, StateShort=="IN")
data9$NAME[data9$NAME=="15885"] <- "Indianapolis"
data9$State <- data9$StateShort
data9$StateShort <- NA
data9$State_Name <- paste(data9$State, data9$NAME, sep="_")
names(data9) <- c("Name", "Website", "Designation", "State", "State_Name")

#indiana URLs from Wikipedia
indianaWebsiteUrls$State <- "IN"
URLs <- indianaWebsiteUrls
URLs$State_Name <- paste(URLs$State, URLs$Name, sep="_")

#merge
URLs <- merge(URLs, subset(data9, select=c("Website", "State_Name")), by="State_Name", all = T)
#the .gov URL should be more reliable, so using that, if available
URLs$Website.x[is.na(URLs$Website.y)==F] <- URLs$Website.y[is.na(URLs$Website.y)==F]
URLs <- subset(URLs, select=-Website.y)
names(URLs)[names(URLs)=="Website.x"] <- "Website"

#remove irrelevant data
rm(data9,indianaWebsiteUrls) #remove objects that are no longer needed

# Load Indiana election data
load("data/indianaElections2015.rdata")

mIN <- filter(mIN, Year==2015)

URLs <- merge(URLs, mIN, by.x = "Name", by.y = "District", all.x = T)

rm(mIN) #remove objects that are no longer needed

URLs <- filter(URLs, Designation=="City") %>%
  filter(is.na(Website)==F) %>%
  filter(is.na(control_change)==F) %>%
  filter(!Name%in%c("Decatur","Knox","Marion")) #remove few sites that are actually counties

#remove /county/ from Tipton
#unfortunately, wget and the ruby WBM downloader will still get both parts of the website
#if I could stop this behavior, I should instead replace "/county/" with "/city/"
#URLs$Website[URLs$Name=="Tipton"] <- str_replace(URLs$Website[URLs$Name=="Tipton"], "/county/", "")
#-- note: in the latest version of the scraped URLs, this doesn't seem necessary any more

#get the base URL
URLs$Website <- str_extract(URLs$Website, "^.+?[^\\/:](?=[?\\/]|$)")

#get foldername with http//: or https//: as well as other forward slashes removed
URLs$foldername <- str_extract(URLs$Website, "//(.*)") %>%
  str_replace_all("/", "")

URLs_IN <- URLs

#save the results
save(URLs_IN, file = "data/URLs_IN.rdata")

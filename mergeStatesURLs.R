
#Load Indiana website data
#(file is created in scrapeIndianaURLsNew.R)
load("rfiles/indianaWebsiteURLs.rdata")

#Load Louisiana website data
#(file is created in scrapeLouisianaURLsNew.R)
load("rfiles/louisianaWebsiteURLs.rdata")

#Load New York website data

#Load Washington website data

#Load 100 largest cities website data
load("./rfiles/campaign_websites.rdata")

#ALL
# GSA .gov website data
data9 <- subset(data9, select=c("NAME","redirect","StateShort"))
data9 <- subset(data9, StateShort=="IN")
data9$NAME[data9$NAME=="15885"] <- "Indianapolis"
data9$State <- data9$StateShort
data9$StateShort <- NA
data9$State_Name <- paste(data9$State, data9$NAME, sep="_")
names(data9) <- c("Name", "Website", "Designation", "State", "State_Name")



#get the base URL
#df$Website <- str_extract(df$Website, "^.+?[^\\/:](?=[?\\/]|$)")


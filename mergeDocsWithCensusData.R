library("data.table")
library("stringr")

#read in the census data
census <- fread("data/sub-est2015_all.csv")
#census <- subset(census, STNAME == state)

#alter city names to make them fit
census$NAME <- str_replace(census$NAME, "town", "")
census$NAME <- str_replace(census$NAME, "village", "")
census$NAME <- str_replace(census$NAME, "city", "")
census$NAME <- str_trim(census$NAME)
census$State_City <- paste(census$STNAME, census$NAME, sep = "_")

#merge
census <- subset(census, select = c("State_City","CENSUS2010POP"))
census <- census[!duplicated(paste0(census$State_City,census$CENSUS2010POP))]
d <- base::merge(websiteUrls, census, by = "State_City", all.x = T, all.y = F)

save(d, file = paste0("rfiles/d_", stateAbb, ".rdata"))
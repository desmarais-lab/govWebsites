library("data.table")
library("stringr")

#read in the census data
census <- fread("data/sub-est2015_all.csv")
census <- subset(census, STNAME == state)

#alter city names to make them fit
census$NAME <- str_replace(census$NAME, "town", "")
census$NAME <- str_replace(census$NAME, "village", "")
census$NAME <- str_replace(census$NAME, "city", "")
census$NAME <- str_trim(census$NAME)

#merge
census <- subset(census, select = c("NAME","CENSUS2010POP", paste0("POPESTIMATE", unique(d$Year))))
census <- census[!duplicated(paste0(census$NAME,census$CENSUS2010POP))]
d <- base::merge(d, census, by.x = "City", by.y = "NAME", all.x = T, all.y = F)

save(d, file = paste0("rfiles/d_", stateAbb, ".rdata"))
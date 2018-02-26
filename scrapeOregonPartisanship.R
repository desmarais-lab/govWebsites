f <- list.files('/home/markus/govWebsites/websites/OR_campaign_finance', full.names = T)

for(i in 2:length(f)){
  file.rename(f[i], paste0(f[i], ".xls"))
}

#library(data.table)
f <- list.files('/home/markus/govWebsites/websites/OR_campaign_finance', full.names = T)
#fread(f[1])

load("rfiles/OR_city_URLs.rdata")
df <- df[df$CityWebsite!="",]
df <- df[df$mayor!="",]
df$mayor <- str_trim(df$mayor)
df$mayor_IDs <- read.table("data/OR_ids.txt")
df$mayor_IDs[df$mayor_IDs==999999999] <- NA
df$party <- NA

library(gdata)
library(stringr)

for(i in 1:length(f)){

df2 <- read.xls(f[i])

if(nrow(df2)>0){

DEM <- str_detect(df2$Contributor.Payee, fixed('democrat', ignore_case=TRUE))
REP <- str_detect(df2$Contributor.Payee, fixed('republican', ignore_case=TRUE))

DEM <- length(which(DEM))
REP <- length(which(REP))

#the loop is for files, not candidates frame
#putting this info in the candidates frame doesnt work, change this

party <- NA

party[DEM>0 & REP == 0] <- "DEM"
party[DEM == 0 & REP > 0] <- "REP"
party[DEM == 0 & REP == 0] <- "NEITHER"
party[DEM > 0 & REP > 0] <- "BOTH"

cand_id <- df2$Filer.Id[1]

#party
df$party[which(df$mayor_IDs==cand_id)] <- party

}
}

save(df, file = "rfiles/OR_city_URLs_partisanship.rdata")

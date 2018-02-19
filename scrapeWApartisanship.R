library(data.table)
library(stringr)

candidates <- fread("data/Campaign_Finance_Reporting_History.csv")
candidates <- candidates[candidates$url!=""]
candidates <- candidates[!duplicated(candidates$filer_id)]


f <- list.files("websites/WA_campaign_finance", full.names = T)

candidate <- fread(f[2])

candidate$DEM <- str_detect(candidate$Name, fixed('democrat', ignore_case=TRUE))
candidate$REP <- str_detect(candidate$Name, fixed('republican', ignore_case=TRUE))

which(candidate$DEM)
which(candidate$REP)

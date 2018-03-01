library(data.table)
library(stringr)

#https://data.wa.gov/Politics/Campaign-Finance-Reporting-History/7qr9-q2c9/data
candidates <- fread("data/Campaign_Finance_Reporting_History.csv")
candidates <- candidates[candidates$url!="",]
candidates <- candidates[!duplicated(candidates$filer_id),]
candidates <- candidates[candidates$election_year>2013,]

load("rfiles/WA_city_URLs.rdata")
df <- df[df$mayor!="",]

#process the files after they got downloaded by the python script

f <- list.files("websites/WA_campaign_finance", full.names = T)

candidates$party_inferred <- NA
#candidates$candidateCity <- NA

for(i in 1:length(f)){

candidate_file <- f[i]
candidate_name <- str_replace(candidate_file, "websites/WA_campaign_finance/", "")
candidate_name <- str_replace(candidate_name, ", [0-9]+.csv", "")

candidate <- fread(candidate_file)
candidate$DEM <- str_detect(candidate$Name, fixed('democrat', ignore_case=TRUE))
candidate$REP <- str_detect(candidate$Name, fixed('republican', ignore_case=TRUE))

DEM <- length(which(candidate$DEM))
REP <- length(which(candidate$REP))

#the loop is for files, not candidates frame
#putting this info in the candidates frame doesnt work, change this

party[DEM>0 & REP == 0] <- "DEM"
party[DEM == 0 & REP > 0] <- "REP"
party[DEM == 0 & REP == 0] <- "NEITHER"
party[DEM > 0 & REP > 0] <- "BOTH"

candidates$party_inferred[candidates$filer_name==candidate_name] <- party

}

candidates$city <- candidates$jurisdiction
candidates$city <- str_replace(candidates$city, "CITY OF ", "")
#de-capitalize every character but the first in each word of the city name
candidates$city <- str_to_title(candidates$city)
candidates$city <- str_replace(candidates$city, "\\*", "")
candidates$new_name <- str_to_title(paste(candidates$first_name, candidates$last_name, sep = " "))

#get rid of the ones where no file was available
candidates2 <- candidates
candidates <- candidates[is.na(candidates$party_inferred)==F,]

df$party <- NA
df$matched_name <- NA
df$num_candidates <- NA
rownames(df) <- 1:nrow(df)

for (i in 1:nrow(df)){
  #try to match them
  possible_matches <- which(df$Name[i] == candidates$city)
  candidates_possible <- candidates[possible_matches,]
  df$num_candidates[i] <- length(na.omit(candidates_possible$new_name))
  if(df$num_candidates[i]>0){
    #first try to match directly
    whichCandidate <- match(df$mayor[i], candidates_possible$new_name)
    if(is.na(whichCandidate)==F){
      df$party[i] <- candidates_possible$party_inferred[whichCandidate]
      df$matched_name[i] <- candidates_possible$new_name[whichCandidate]
    }else{
      #otherwise, try a proximity match
      mayors_strdist <- stringdist::stringdist(df$mayor[i], candidates_possible$new_name, method="jaccard")
      df$party[i] <- candidates_possible$party_inferred[which.min(mayors_strdist)]
      df$matched_name[i] <- candidates_possible$new_name[which.min(mayors_strdist)]
    }
  }
}

#Note: the proximity matching seems to have worked well here - no mistakes

#
washingtonWebsiteUrls <- subset(df, select = c("Name", "wiki_link", "mayor", "CityWebsite", "party"))
names(washingtonWebsiteUrls) <- c("City", "wiki_link", "wikiMayor", "wikiCityWebsite", "financeParty")

save(washingtonWebsiteUrls, file = "rfiles/washingtonWebsiteURLs.rdata")

#This script reads in and filters the Census data
#Then it gets merged with the GSA websites data
#About 80 manual corrections are necessary

setwd("/home/markus/Dropbox/4_RA/govWebsites")

#read in census data
#Source:
#http://www2.census.gov/programs-surveys/popest/datasets/2010-2015/cities/totals/sub-est2015_all.csv
censusdata <- read.csv("data/sub-est2015_all.csv")
censusdata <- censusdata[order(censusdata$STNAME,censusdata$NAME),]

#remove states
censusdata <- subset(censusdata, SUMLEV!=40)
#remove counties
censusdata <- subset(censusdata, SUMLEV!=50)
#remove county place part
#censusdata <- subset(censusdata, SUMLEV!=157)

#find and remove parts of cities/towns/etc.
#censusdata <- censusdata[-which(str_detect(censusdata$NAME," \\(pt\\.\\)")),]

#remove anything without a county
censusdata <- subset(censusdata, PRIMGEO_FLAG!=0)

#find and remove townships
censusdata <- censusdata[-which(str_detect(censusdata$NAME," township")),]

#find and remove boroughs
censusdata <- censusdata[-which(str_detect(censusdata$NAME," borough")),]

#find and remove Balance of
censusdata <- censusdata[-which(str_detect(censusdata$NAME,"Balance of")),]

#once states and counties are removed, every place can be uniquely identified with either
#PLACE (Place FIPS code)
#COUSUB (Minor Civil Division FIPS code)
#CONCIT (Consolidated city FIPS code)

censusdata$ID <- paste(censusdata$PLACE,censusdata$COUSUB,censusdata$CONCIT,sep="-")

censusdata <- censusdata[order(censusdata$STATE,censusdata$NAME),]


#in the census data, names always end with "town" or "city", removing
censusdata$NAME <- sub(" \\(pt\\.\\)", "", censusdata$NAME)
censusdata$NAME <- sub(" town$", "", censusdata$NAME)
censusdata$NAME <- sub(" city$", "", censusdata$NAME)
censusdata$NAME <- sub(" village$", "", censusdata$NAME)


#create state-city-population variable, and remove rows that aren't unique according to it
censusdata$statecitypop <- paste(censusdata$STATE,censusdata$NAME,censusdata$POPESTIMATE2015,sep="-")
censusdata <- censusdata[!duplicated(censusdata$statecitypop),]

#create variable with state abbreviations
censusdata$StateShort <- state.abb[match(censusdata$STNAME, state.name)]

#create StateCity variable for merge
censusdata$StateCity <- paste(censusdata$StateShort, censusdata$NAME, sep="")

###############################################################

#load verified and fixed GSA data
load("data/govWebsitesVerified.Rdata")

#merge census data and verified .gov addresses
data2 <- merge(data, censusdata, by="StateCity", all.x = T, all.y = F)

#remove any city that didn't get merged and put them in their own dataset
data3 <- data2[is.na(data2$CENSUS2010POP)==T,] #missings
data2 <- data2[is.na(data2$CENSUS2010POP)==F,]

#failure to merge
#create Census dataset with boroughs/townships
boroughs <- read.csv("data/sub-est2015_all.csv")

#find townships
townships <- boroughs[which(str_detect(boroughs$NAME," township")),]
#find boroughs
boroughs <- boroughs[which(str_detect(boroughs$NAME," borough") | str_detect(boroughs$NAME," Borough")),]

#create a new census dataset with either townships and boroughs
#then reduce it to two variables (indicator for townships / boroughs)
#and ID for merge
#then merge this into the full dataset, so I have an indicator for which
#websites belong to townships/boroughs
#then remove those
townships$NAME <- sub(" township$", "", townships$NAME)
townships$StateShort <- state.abb[match(townships$STNAME, state.name)]
townships$StateCity <- paste(townships$StateShort, townships$NAME, sep="")
townships$TOWNSHIP <- 1
townships <- subset(townships, select=c("TOWNSHIP","StateCity"))
townships <- townships[!duplicated(townships$StateCity),]
data3 <- merge(data3,townships,by="StateCity",all.x = T)

boroughs$NAME <- sub(" borough$", "", boroughs$NAME)
boroughs$NAME <- sub(" Borough$", "", boroughs$NAME)
boroughs$StateShort <- state.abb[match(boroughs$STNAME, state.name)]
boroughs$StateCity <- paste(boroughs$StateShort, boroughs$NAME, sep="")
boroughs$BOROUGH <- 1
boroughs <- subset(boroughs, select=c("BOROUGH","StateCity"))
boroughs <- boroughs[!duplicated(boroughs$StateCity),]
data3 <- merge(data3,boroughs,by="StateCity",all.x = T)

#remove missings that are townships or boroughs
data3 <- subset(data3, is.na(TOWNSHIP)==T)
data3 <- subset(data3, is.na(BOROUGH)==T)

rm(townships,boroughs)

#fix the rest manually
data3$City[data3$StateCity=="ARDewitt"] <- "DeWitt"
data3$City[data3$StateCity=="AZHumboldt"] <- "Dewey-Humboldt"
data3$City[data3$StateCity=="AZLakeside"] <- "Pinetop-Lakeside"
data3$City[data3$StateCity=="CAMt. Shasta"] <- "Mount Shasta"
data3$City[data3$StateCity=="CTPomfret Center"] <- "Pomfret"
data3$City[data3$StateCity=="FLLauderdale-By-The-Sea"] <- "Lauderdale-by-the-Sea"
data3$City[data3$StateCity=="FLLauderdale By The Sea"] <- "Lauderdale-by-the-Sea"
data3$City[data3$StateCity=="FLOpalocka"] <- "Opa-locka"
data3$City[data3$StateCity=="FLSaint Leo"] <- "St. Leo"
data3$State[data3$StateCity=="FMClermont"] <- "FL"
#data3$City[data3$StateCity=="GAAugusta"] <- "Augusta-Richmond County consolidated government" #DIFFICULT TO MERGE LATER, DO BY HAND
data3$City[data3$StateCity=="GALagrange"] <- "LaGrange"
data3$City[data3$StateCity=="GAMans"] <- "Mansfield"
data3$City[data3$StateCity=="GAMccaysville"] <- "McCaysville"
#GARTucker does not exist in other dataset because it "was officially incorporated as DeKalb County's newest city in early 2016"
#####Honolulu -- do this by hand
data3$City[data3$StateCity=="IALeclaire"] <- "Le Claire"
data3$City[data3$StateCity=="IDBoise"] <- "Boise City"
####Brookfield merge manually with 8576 0 0 0 A Brookfield
data3$City[data3$StateCity=="ILLasalle"] <- "LaSalle"
####Indianapolis merge manually with 36003 0 0 0 F Indianapolis city (balance)
data3$City[data3$StateCity=="KYCoal Run"] <- "Coal Run Village"
data3$City[data3$StateCity=="KYLexington"] <- "Lexington-Fayette urban county" ###MERGE BY HAND
data3$City[data3$Domain.Name=="DRUIDHILLSKY.GOV"] <- "Druid Hills"
####Louisville/Jefferson County metro government
data3$City[data3$StateCity=="MAAssonet"] <- "Freetown"
data3$City[data3$StateCity=="MABraintree"] <- "Braintree Town"
data3$City[data3$StateCity=="MAFranklin"] <- "Franklin Town"
data3$City[data3$StateCity=="MAGreenfield"] <- "Greenfield Town"
data3$City[data3$StateCity=="MATurners Falls"] <- "Montague"
data3$City[data3$StateCity=="MAWatertown"] <- "Watertown Town"
#Friendship Heights cant be matched. Originally it was called Chevy Chase, which does exist;
#but FH is a subdivision of CC, and apparently only consists of 16 bldgs. Parts of CC
#exist in the census data, but nothing small enough to match that
data3$City[data3$StateCity=="MDHavre De Grace"] <- "Havre de Grace"
data3$City[data3$StateCity=="MDRiverdale"] <- "Riverdale Park"
data3$City[data3$StateCity=="MEEast Waterboro"] <- "Waterboro"
#Grosse Pointe Shores can't be matched. there are several Grosse Pointes (Park, Farms, etc.) in the other dataset,
#but they refer to different parts of the city. I checked on Google Maps, there is nothing that matches Shores
data3$City[data3$StateCity=="MNSaint Paul"] <- "St. Paul"
data3$City[data3$StateCity=="MNSaint Peter"] <- "St. Peter"
data3$City[data3$StateCity=="MNWest Saint Paul"] <- "West St. Paul"
data3$City[data3$StateCity=="MOBellerive Acres"] <- "Bellerive"
data3$City[data3$StateCity=="MOLagrange"] <- "La Grange"
data3$City[data3$StateCity=="MOSaint Charles"] <- "St. Charles"
data3$City[data3$StateCity=="MSMccomb"] <- "McComb"
data3$City[data3$StateCity=="MSPoplarvile"] <- "Poplarville"
data3$City[data3$StateCity=="NMCorales"] <- "Corrales"
data3$City[data3$StateCity=="NMLos Ranchos De Albuquerque"] <- "Los Ranchos de Albuquerque"
#NVGardnerville not in the dataset https://en.wikipedia.org/wiki/Gardnerville,_Nevada
#NYBedford Hills not in the dataset, since it is a hamlet within Bedford https://en.wikipedia.org/wiki/Bedford_Hills,_New_York
data3$City[data3$StateCity=="NYCroton-On-Hudson"] <- "Croton-on-Hudson"
data3$City[data3$StateCity=="NYHemsptead"] <- "Hempstead"
data3$City[data3$StateCity=="NYKent Lakes"] <- "Kent"
data3$City[data3$StateCity=="NYLagrangeville"] <- "La Grange"
data3$City[data3$StateCity=="OHMccomb"] <- "McComb"
data3$City[data3$StateCity=="ORGoldbeach"] <- "Gold Beach"
data3$City[data3$StateCity=="ORMcminnville"] <- "McMinnville"
data3$City[data3$StateCity=="PADubois"] <- "DuBois"
data3$City[data3$StateCity=="PAMckeesport"] <- "McKeesport"
data3$City[data3$StateCity=="TNLafollette"] <- "La Follette"
data3$City[data3$StateCity=="TNMckenzie"] <- "McKenzie"
data3$City[data3$StateCity=="TNMt Juliet"] <- "Mount Juliet"
#data3$City[data3$StateCity=="TNNashville"] <- "Nashville-Davidson metropolitan government" match by hand
data3$City[data3$StateCity=="TXCrossroads"] <- "Cross Roads"
data3$City[data3$StateCity=="TXDesoto"] <- "DeSoto"
data3$City[data3$StateCity=="UTWest Valley"] <- "West Valley City"
data3$City[data3$StateCity=="VAPoquioson"] <- "Poquoson"
data3$City[data3$StateCity=="VTEast Calais"] <- "Calais"
data3$City[data3$StateCity=="VTManchester Center"] <- "Manchester"
data3$City[data3$StateCity=="VTUnderhill, Ctr"] <- "Underhill"
data3$City[data3$StateCity=="WABeaux Arts"] <- "Beaux Arts Village"
data3$City[data3$StateCity=="WICity Of St. Croix Falls"] <- "St. Croix Falls"
data3$City[data3$StateCity=="WIPrairie Du Chien"] <- "Prairie du Chien"
#Washington Island FIPS code: FIPS code	55-83600

data3$StateCity <- paste(data3$State,data3$City,sep="")
data3 <- data3[,-c(13:35)]

data4 <- merge(data3,censusdata,by="StateCity", all.x = T)


#The rest is more tricky, so needs to be directly matched by hand:

#read in censusdata again, this time not removing anything + create vars
censusdata2 <- read.csv("data/sub-est2015_all.csv")
censusdata2$ID <- paste(censusdata2$PLACE,censusdata2$COUSUB,censusdata2$CONCIT,sep="-")
censusdata2$statecitypop <- paste(censusdata2$STATE,censusdata2$NAME,censusdata2$POPESTIMATE2015,sep="-")
censusdata2$StateShort <- state.abb[match(censusdata2$STNAME, state.name)]

#now, match by hand
data4[data4$StateCity=="GAAugusta",c(13:33)] <- censusdata2[censusdata2$PLACE==0 & censusdata2$CONCIT==4200,1:21]
data4[data4$StateCity=="HIHonolulu",c(13:33)] <- censusdata2[censusdata2$STATE==15 & censusdata2$PLACE==99990,1:21]
data4[data4$StateCity=="INIndianapolis",c(13:33)] <- censusdata2[censusdata2$PLACE==0 & censusdata2$CONCIT==36000,1:21]
data4[data4$StateCity=="KYLouisville",c(13:33)] <- censusdata2[censusdata2$PLACE==0 & censusdata2$CONCIT==48003,1:21]
data4[data4$StateCity=="TNNashville",c(13:33)] <- censusdata2[censusdata2$PLACE==0 & censusdata2$CONCIT==52004,1:21]
data4[data4$StateCity=="WIWashington Island",c(13:33)] <- censusdata2[censusdata2$COUNTY==29 & censusdata2$COUSUB==83600,1:21]

##Cities that simply cannot be matched:
#GA-Tucker (created in 2016, so not in the Census data yet)
#MD-Friendship Heights (subdivision of Chevy Chase)
#MI-Grosse Pointe Shores (subdivision of Grosse Pointe)
#NV-Gardnerville (not sure why this is not in the dataset, it is an unincorporated place in NV)
#NY-Bedford Hills (hamlet within Bedford)

#merge the fixed results back into dataset
data4 <- subset(data4, is.na(POPESTIMATE2015)==F)
data5 <- rbind(data2,data4)

#remove duplicates
#duplicate if state,city,and population size are the same
data6 <- data5[!duplicated(data5$statecitypop),]

#aggregate, so that parts of cities get merged into one
data7 <- data6[,c(1,23:30)]
data7[,2:9] <- apply(data7[,2:9],2,as.character)
data7[,2:9] <- apply(data7[,2:9],2,as.numeric)
data8 <- aggregate(data7[,2:9], by=list(data6$StateCity), FUN = sum)

#merge back with websites
data6 <- data6[,-c(23:30)]
data9 <- merge(data8,data6, by.x="Group.1",by.y="StateCity",all.x = F,all.y = T)
data9 <- data9[!duplicated(data9$Group.1),]

#save results
save(data9, file="data/govWebsitesVerifiedCensus.Rdata")

#check population coverage
sum(data9$POPESTIMATE2015) #90616865
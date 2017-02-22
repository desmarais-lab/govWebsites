setwd("D:/Dropbox/4_RA/govWebsites") #Windows
setwd("/home/markus/Dropbox/4_RA/govWebsites") #Linux

require(stringr)
require(stringi)

#read in the data created by the python scripts
results1 <- read.table("results/results1.txt")
results2 <- read.table("results/results2.txt")
results3 <- read.table("results/results3.txt")
results4 <- read.table("results/results4.txt")
results5 <- read.table("results/results5.txt")
results6 <- read.table("results/results6.txt")

#crete one data frame
result <- rbind(results1,results2,results3,results4,results5,results6)
rm(results1,results2,results3,results4,results5,results6)

#read in original RSA file
data <- read.csv("data/current-full.csv")

#variable indicating where the .gov addresses redirect to
data$redirect <- as.character(result$V1)
rm(result)

#did the website not load due to timeout
data$timeout <- 0
data$timeout[data$redirect=="TimeoutException"] <- 1

#did the website not load at all?
data$dead <- 0
data$dead[data$redirect=="WebDriverException"] <- 1

#set redirect to NA if either of those happened
data$redirect[data$redirect%in%c("TimeoutException","WebDriverException")] <- NA

#did the website load correctly?
data$loaded <- 0
data$loaded[is.na(data$redirect)==F] <- 1

#clean up city var
data$City <- str_trim(data$City, side = "right")

#first letter(s) of city name to uppercase, rest to lowercase
data$CityOriginal <- data$City #preserve old var
data$City <-  stri_trans_totitle(data$City)

#create StateCity variable for merge
data$StateCity <- paste(data$State,data$City,sep="")

#Remove everything but cities
data <- subset(data, Domain.Type=="City")

#remove any city whose website didn't load
data <- subset(data, loaded==1)

#cities with duplicated names AND websites
data$StateCityRedirect <- paste(data$StateCity, data$redirect, sep="")
data <- data[!duplicated(data$StateCityRedirect),]

which(duplicated(data$redirect))


##FIXES

#data <- data[,c(1,6,2,3,4,5,7:11)]

###################data$redirect[data$StateCity=="ALMillbrook"] <- "http://tallassee-al.gov/" #error
data <- data[!data$redirect=="http://www.acton-ma.gov/",] #duplicate website
data <- data[!data$redirect=="http://www.annettatx.gov/",] #wrong website
data <- data[!data$redirect=="http://www.antiochtownshipil.gov/",] #township
data <- data[!data$redirect=="http://archbaldboroughpa.gov/",] #borough
data$City[data$StateCity=="TXRhome"] <- "Aurora" #wrong city
data$City[data$StateCity=="WIHayward"] <- "Bass Lake" #wrong city
data <- data[!data$redirect=="http://www.baycounty-mi.gov/",] #county
data <- data[!data$redirect=="http://www.beavertwp-oh.gov/",] #township
data$City[data$redirect=="http://bellemeade-ky.gov/"] <- "Bellemeade" #wrong city
data$City[data$redirect=="http://www.belleriveacresmo.gov/"] <- "Bellerive Acres" #wrong city
data <- data[!data$redirect=="http://www.bentonchartertownship-mi.gov/",] #township + also not really a website
data <- data[!data$redirect=="http://www.bernco.gov/",] #county
data <- data[!data$redirect=="http://www.blendontownship-mi.gov/",] #township
data$City[data$redirect=="http://www.bourbon-in.gov/"] <- "Bourbon" #wrong city
data <- data[!data$redirect=="http://www.bowersde.gov/",] #website doesnt work
#Bow Mar CO https://www.colorado.gov/townofbowmar --- exercise caution when scraping this later
data <- data[!data$redirect=="http://www.brimfieldohio.gov/",] #township
data$City[data$redirect=="http://www.brookhavenga.gov/"] <- "Brookhaven" #wrong city
data$City[data$redirect=="http://www.brookhavenny.gov/"] <- "Brookhaven" #wrong city
data$City[data$redirect=="https://www.bunkerhilltx.gov/"] <- "Bunker Hill Village" #wrong city
data$City[data$redirect=="http://www.burrillville.org/"] <- "Burrillville" #wrong city
data <- data[!data$redirect=="http://www.calumettwp-in.gov/",] #township
data$City[data$redirect=="http://www.centervilletx.gov/"] <- "Centerville" #wrong city
data <- data[!data$redirect=="http://www.chestnuthilltwp-pa.gov/",] #township
data <- data[!data$redirect=="http://www.chowancounty-nc.gov/",] #county
data$City[data$redirect=="http://www.cityofnormandy.gov/"] <- "Normandy" #wrong city
data$City[data$redirect=="http://cityofwestonlakes-tx.gov/"] <- "Weston Lakes" #wrong city
data$City[data$redirect=="http://www.clintonnj.gov/"] <- "Clinton" #wrong city
data$City[data$redirect=="http://www.coalrunky.gov/"] <- "Coal Run" #wrong city
data <- data[!data$redirect=="http://www.collincountytx.gov/Pages/default.aspx",] #county
data <- data[!data$redirect=="http://www.columbiatwp.us/",] #township
data <- data[!data$redirect=="http://www.comstockmi.gov/",] #township
data$City[data$redirect=="http://www.cranberryisles-me.gov/"] <- "Cranberry Isles" #wrong city
data$City[data$redirect=="http://www.creditriver-mn.gov/"] <- "Credit River" #wrong city
data <- data[!data$redirect=="http://www.daughertytownship-pa.gov/",] #township
data$City[data$redirect=="http://dekorra-wi.gov/"] <- "Dekorra" #wrong city
data <- data[!data$redirect=="http://www.delawaretownshippa.gov/",] #township
data <- data[!data$redirect=="http://www.deltami.gov/",] #township
data <- data[!data$redirect=="http://www.dutchessny.gov/",] #county
data <- data[!data$redirect=="http://eastcoventry-pa.gov/",] #township
data <- data[!data$redirect=="http://www.easthamptonny.gov/",] #wrong city + also not really a website
data$City[data$redirect=="http://www.eastwindsor-ct.gov/Public_Documents/index"] <- "East Windsor" #wrong city
data <- data[!data$redirect=="http://www.elktownshipnj.gov/",] #township
data <- data[!str_detect(data$redirect,"township"),]
data <- data[!data$redirect=="http://www.elmwoodplace-oh.gov/",] #wrong city + also not really a website
data <- data[!data$redirect=="http://www.emeraldbay-tx.gov/",] #utility district
data <- data[!data$redirect=="http://www.evesham-nj.gov/",] #township
data$City[data$redirect=="http://www.fairviewnc.gov/"] <- "Fairview" #wrong city
data <- data[!data$redirect=="http://www.fortlupton-co.gov/",] #duplicate website
#data$City[data$redirect=="http://www.town-menasha.com/"] <- "Fox Crossing" #wrong city; BUT old name still in census data
data$City[data$redirect=="http://www.friendshipheightsmd.gov/"] <- "Friendship Heights" #wrong city
data <- data[!data$redirect=="http://www.georgetown-mi.gov/",] #township
data$City[data$redirect=="http://www.greeceny.gov/"] <- "Greece" #wrong city
data <- data[!data$redirect=="http://www.greenhoustontx.gov/",] #Houston Office of Sustainability
data$City[data$redirect=="http://www.hamiltonma.gov/Pages/index"] <- "Hamilton" #wrong city
data <- data[!data$redirect=="http://www.harmonytwp-nj.gov/",] #township
data <- data[!data$redirect=="http://www.harpercountyks.gov/",] #county
data <- data[!data$redirect=="http://www.harrisontwp-pa.gov/",] #township + doesnt work
data <- data[!data$redirect=="http://www.headoftheharborny.gov/",] #doesnt work
data$City[data$redirect=="http://www.hedwigtx.gov/"] <- "Hedwig Village" #wrong city
data$City[data$redirect=="http://www.horiconny.gov/"] <- "Horicon" #wrong city
data <- data[!data$redirect=="http://www.hrpdcva.gov/",] #Hampton roads district something
data$City[data$redirect=="http://www.indianpoint-mo.gov/"] <- "Indian Point" #wrong city
data$City[data$redirect=="http://www.kannapolisnc.gov/"] <- "Kannapolis" #wrong city
data$City[data$redirect=="http://www.killingly.org/"] <- "Killingly" #wrong city
data$City[data$redirect=="http://www.kinderhook-ny.gov/public_documents/index"] <- "Kinderhook" #wrong city
data$City[data$redirect=="http://lakeparknc.gov/"] <- "Lake Park" #wrong city
data$City[data$redirect=="http://www.lakevillagear.gov/"] <- "Lake Village" #wrong city
data <- data[!data$redirect=="https://apps.rackspace.com/",] #website not working
data <- data[!data$redirect=="https://www.colorado.gov/logan",] #county
data$redirect[data$redirect=="http://freeads556.com/"] <- "http://www.lovelandoh.com/" #wrong website
data <- data[!data$redirect=="http://www.lowerallowayscreek-nj.gov/",] #township
data <- data[!data$redirect=="http://www.lowerpaxton-pa.gov/",] #township
data$City[data$redirect=="http://www.manisteemi.gov/"] <- "Manistee" #wrong city
data <- data[!data$redirect=="http://www.mifflin-oh.gov/",] #township
data <- data[!data$redirect=="http://www.monroetwp-oh.gov/",] #township
data <- data[!data$redirect=="http://www.mundytwp-mi.gov/",] #township
data <- data[!data$redirect=="http://www.nazarethboroughpa.gov/",] #borough
data$City[data$redirect=="http://www.newmarlboroughma.gov/pages/index"] <- "New Marlborough" #wrong city
data$City[data$redirect=="http://nissequogueny.gov/pageon.php?b=home"] <- "Nissequogue" #wrong city
data <- data[!data$redirect=="https://www.nolaerb.gov/",] #New Orleans already has a website
data <- data[!data$redirect=="http://www.nolaipm.gov/main/index.php?page=home",] #New Orleans already has a website
data <- data[!data$redirect=="http://www.nolaoig.gov/",] #New Orleans already has a website
data$City[data$redirect=="http://www.northhempsteadny.gov/"] <- "North Hempstead" #wrong city
data$City[data$redirect=="http://www.northsiouxcity-sd.gov/"] <- "North Sioux City" #wrong city
data <- data[!data$redirect=="http://www.nsidfl.gov/",] #North Springs Improvement District
data$City[data$redirect=="http://www1.nyc.gov/"] <- "New York" #wrong city
data$City[data$redirect=="http://www.oakridgetn.gov/"] <- "Oak Ridge" #wrong city
data <- data[!data$redirect=="http://otaywater.gov/",] #water utility company
data$City[data$redirect=="http://paxtonfl.gov/"] <- "Paxton" #wrong city
data$City[data$redirect=="http://www.perry-wi.gov/"] <- "Perry" #wrong city
data <- data[!data$redirect=="http://www.pittsfield-mi.gov/",] #township
data$City[data$redirect=="http://plandomeheights-ny.gov/"] <- "Plandome Heights" #wrong city
data <- data[!data$redirect=="http://www.poconopa.gov/",] #township
data$City[data$redirect=="http://www.pompey-ny.gov/"] <- "Pompey" #wrong city
data <- data[!data$Domain.Name=="POOLER-GA.GOV",] #website not working
data <- data[!data$redirect=="http://www.pottertwp-pa.gov/",] #township
#https://www.colorado.gov/townofrangely --- be careful here later
data <- data[!data$redirect=="http://www.readyhoustontx.gov/",] #not the website of Houston, already got that one
data <- data[!data$redirect=="http://www.readysouthtexas.gov/",] #disaster preparedness website like above
data <- data[!data$redirect=="http://www.rheacountytn.gov/",] #county
data$City[data$redirect=="http://www.richfieldwi.gov/"] <- "Richfield" #wrong city
data <- data[!data$redirect=="http://www.robinsonpa.gov/",] #township
data <- data[!data$redirect=="http://www.sheltercove-ca.gov/",] #"resort improvement district"
data <- data[!data$redirect=="http://www.staffordnj.gov/",] #township
data <- data[!data$redirect=="http://www.stlucieco.gov/",] #county
data$City[data$redirect=="http://stlucievillagefl.gov/"] <- "St. Lucie Village" #wrong city
data$City[data$redirect=="http://www.stmatthewsky.gov/"] <- "St. Matthews" #wrong city
data$City[data$redirect=="http://tallassee-al.gov/"] <- "Tallassee" #wrong city
data <- data[!data$redirect=="http://www.townofbethlehem-ny.gov/",] #website not working
data$City[data$redirect=="http://www.townofhomecroftin.gov/"] <- "Homecroft" #wrong city
data$City[data$redirect=="http://www.townofhounsfield-ny.gov/"] <- "Hounsfield" #wrong city
data$City[data$redirect=="http://www.townofnortheastny.gov/"] <- "North East" #wrong city
data$City[data$redirect=="http://www.townofpalermony.gov/"] <- "Palermo" #wrong city
data$City[data$redirect=="http://www.townofshields-wi.gov/"] <- "Shields" #wrong city
data <- data[!data$redirect=="http://www.tricountyconservancy-in.gov/",] #not a city
data <- data[!data$redirect=="http://ecbiz193.inmotionhosting.com/unavailable.html",] #website broken
data$City[data$redirect=="http://tuxedopark-ny.gov/"] <- "Tuxedo Park" #wrong city
data$City[data$redirect=="http://www.tyngsboroughma.gov/"] <- "Tyngsborough" #wrong city
data <- data[!data$redirect=="http://www.uniontwp-hcnj.gov/",] #township
data$City[data$redirect=="http://www.unitynh.gov/"] <- "Unity" #wrong city
data <- data[!data$redirect=="http://www.upperuwchlan-pa.gov/",] #township
data <- data[!data$redirect=="http://www.vernontwp-pa.gov/online/index.php",] #township
data$City[data$redirect=="http://www.villageofkensingtonny.gov/"] <- "Kensington" #wrong city
data$City[data$redirect=="http://www.waldentn.gov/"] <- "Walden" #wrong city
data <- data[!data$redirect=="http://www.washington-warrenairport-nc.gov/",] #website not working + probably not a city
data <- data[!data$redirect=="http://www.washingtonboro-nj.gov/",] #borough
data$City[data$redirect=="http://www.westfargond.gov/"] <- "West Fargo" #wrong city
data <- data[!data$redirect=="http://www.whitecounty-il.gov/",] #county
data$City[data$redirect=="http://www.windsorwi.gov/"] <- "Windsor" #wrong city
data$City[data$redirect=="http://yanceyvillenc.gov/"] <- "Yanceyville" #wrong city
data <- data[!data$redirect=="http://www.waterfordmi.gov/",] #township
data <- data[!data$redirect=="http://www.northfieldmi.gov/",] #township
data <- data[!data$redirect=="http://www.twpoceannj.gov/",] #township
data <- data[!data$redirect=="http://www.southabingtonpa.gov/",] #township
data <- data[!data$redirect=="http://www.plumstead.gov/",] #township
data <- data[!data$redirect=="http://www.jacksontwp-pa.gov/",] #township
data <- data[!data$redirect=="http://www.mayaguezpr.gov/",] #Puerto Rico is not in the dataset
data <- data[!data$redirect=="http://www.chesterfield.gov/",] #county
data <- data[!data$redirect=="http://www.powhatanva.gov/",] #county




data$StateCity <- paste(data$State,data$City,sep="")
###############################################################

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

#remove states and counties
#townshipts and boroughs are still in the data
#but this shouldn't be an issue since they have that title in the name
#censusdata <- censusdata[!(censusdata$PLACE==0 & censusdata$COUSUB==0),]

#exclude duplicates
#censusdata <- subset(censusdata, SUMLEV==157)

###############################################################

#merge census data and verified .gov addresses
data2 <- merge(data, censusdata, by="StateCity", all.x = T, all.y = F)

#remove duplicates (no relevant information is lost)
#data2 <- data2[!duplicated(data2$StateCity),]

#remove any city that didn't get merged and put them in their own dataset
data3 <- data2[is.na(data2$CENSUS2010POP)==T,] #missings
data2 <- data2[is.na(data2$CENSUS2010POP)==F,]


#failure to merge
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
sum(data9$POPESTIMATE2015)
#90616865
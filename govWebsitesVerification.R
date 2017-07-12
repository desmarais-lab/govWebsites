#This script reads in the textfiles I created with Selenium 
#(describing where websites redirect to)
#This is then merged with the GSA data
#Furthermore, about 130 manual corrections to the dataset are made, removing
#townships, counties, boroughs that are marked as citites
#as well as correcting city names that don't correspond to the website

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

#create variable to be used for merge
data$StateCity <- paste(data$State,data$City,sep="")

#save results
save(data, file="data/govWebsitesVerified.Rdata")


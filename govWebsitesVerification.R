#setwd("D:/Dropbox/4_RA/govWebsites")
setwd("/home/markus/Dropbox/4_RA/govWebsites")
#setwd(getSrcDirectory(function(x) {x})) #set wd to .R file directory


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

#xtabs(~loaded+Domain.Type, data=data)
#loaded City County Federal Agency Native Sovereign Nation State/Local Govt
#0  292    100            301                      39              221
#1 2133    497           1030                     130              903


#number of municipalities etc. in the US as of 2007:
#source: http://www.nlc.org/number-of-municipal-governments-population-distribution

#39,044 general purpose local governments
#19,492 municipal governments
#16,519 township governments
#3.033 county governments


#read in census data
#Source:
#http://www2.census.gov/programs-surveys/popest/datasets/2010-2015/cities/totals/sub-est2015_all.csv
censusdata <- read.csv("data/sub-est2015_all.csv")
censusdata <- censusdata[order(censusdata$STNAME,censusdata$NAME),]

#in the census data, names always end with "town" or "city", removing
censusdata$NAME <- sub(" town$", "", censusdata$NAME)
censusdata$NAME <- sub(" city$", "", censusdata$NAME)

#create variable with state abbreviations
censusdata$StateShort <- state.abb[match(censusdata$STNAME, state.name)]

#create StateCity variable for merge
censusdata$StateCity <- paste(censusdata$StateShort, censusdata$NAME, sep="")

#create StateCity variable for merge
data$StateCity <- paste(data$State,data$City,sep="")

#Remove everything but cities
data <- subset(data, Domain.Type=="City")

#merge census data and verified .gov addresses
data <- merge(data, censusdata, by="StateCity", all.x=T, all.y = F)

#remove duplicates (no relevant information is lost)
data <- data[!duplicated(data$StateCity),]

#remove any city whose website didn' load
data <- subset(data, loaded==1)

#remove any city that didn't get merged
data <- data[is.na(data$CENSUS2010POP)==F,]

#save results
save(data, file="data/govWebsitesVerified.Rdata")
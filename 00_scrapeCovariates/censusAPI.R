#use the census API to get the covariates needed for the stm
library(censusapi)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY=readLines("censusAPI_Key.txt"))
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

varsbds <- listCensusMetadata(name="acs/acs5", vintage = 2015, type = "v")

#Variables:

#B01001_001E -- total population
#B01001_002E -- male
#B01001A_001E -- white
#B01001B_001E -- African American
#B03001_001E -- Hispanic or Latino

#B07001. Geographical Mobility in the Past Year
#B07001_017E -- Same house 1 year ago
#B07001_033E -- Moved within same county
#B07001_065E -- Moved from different state

#B08128. MEANS OF TRANSPORTATION TO WORK
#B08128_031E -- Public transportation (excluding taxicab)

#B10054. Lang and Ability to Speak Eng
#B10054_002E -- Speak only English

#B17001F. POVERTY STATUS IN THE PAST 12 MONTH
#B17001F_002E -- Income in the past 12 months below poverty level

#B19013. Median Household Income in the Past 12 Months
#B19013_001E -- Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)

#B15003. Educational Attainment for the Population over 25 years
#B15003_002E -- No schooling completed
#B15003_017E -- Regular high school diploma
#B15003_018E -- GED or alternative credential
#B15003_022E -- Bachelor's degree
#B15003_023E -- Master's degree
#B15003_025E -- Doctorate degree

#B25077. Median Value (Dollars) for Owner-Occupied Housing Units
#B25077_001E -- Median value (dollars)

variables <- c("NAME", "B01001_001E", "B01001_002E", "B01001A_001E", "B01001B_001E", "B03001_001E",
               "B07001_017E", "B07001_033E", "B07001_033E", "B07001_065E",
               "B08128_031E", "B10054_002E", "B17001F_002E", "B19013_001E",
               "B15003_002E", "B15003_017E", "B15003_018E", "B15003_022E",
               "B15003_023E", "B15003_025E", "B25077_001E")

acs5 <- getCensus(name = "acs/acs5",
                 vintage = 2015,
                 vars = variables,
                 region = "place:*")

save(acs5, file = "out/acs5.rdata")

#----
# Merge with city metadata
load("rfiles/allURLs.rdata")

acs5$NAME <- sub(" \\(pt\\.\\)", "", acs5$NAME)
acs5$NAME <- sub(" town", "", acs5$NAME)
acs5$NAME <- sub(" city", "", acs5$NAME)
acs5$NAME <- sub(" village", "", acs5$NAME)
acs5$NAME <- sub(" municipality", "", acs5$NAME)
acs5$NAME <- sub(" CDP", "", acs5$NAME)
acs5$NAME <- sub(", ", "_", acs5$NAME)
library(stringr)
dNAME <- do.call(rbind, str_split(acs5$NAME, "_"))
acs5$NAME <- paste(dNAME[,2], dNAME[,1], sep = "_")

websiteUrls2 <- merge(websiteUrls, acs5, by.x = "State_City", by.y = "NAME")
#which are still missing?
websiteUrls$State_City[!websiteUrls$State_City%in%websiteUrls2$State_City]

#fix
acs5$NAME <- sub("District of Columbia_Washington", "D.C._Washington", acs5$NAME)
acs5$NAME <- sub("Hawaii_Urban Honolulu", "Hawaii_Honolulu", acs5$NAME)
acs5$NAME <- sub("Idaho_Boise City", "Idaho_Boise", acs5$NAME)
acs5$NAME <- sub("Indiana_Indianapolis \\(balance\\)", "Indiana_Indianapolis", acs5$NAME)
acs5$NAME <- sub("Kentucky_Lexington-Fayette urban county", "Kentucky_Lexington", acs5$NAME)
acs5$NAME <- sub("Kentucky_Louisville\\/Jefferson County metro government \\(balance\\)", "Kentucky_Louisville", acs5$NAME)
acs5$NAME <- sub("New York_New York", "New York_New York City", acs5$NAME)
acs5$NAME <- sub("Tennessee_Nashville-Davidson metropolitan government \\(balance\\)", "Tennessee_Nashville", acs5$NAME)

#continue once everything matches:
websiteUrls <- merge(websiteUrls, acs5, by.x = "State_City", by.y = "NAME")

#one duplicate new york Tonawanda; remove place 74183
websiteUrls <- websiteUrls[websiteUrls$place!=74183,]
websiteMeta <- websiteUrls

save(websiteMeta, file = "out/websiteMetadata_Census.rdata")

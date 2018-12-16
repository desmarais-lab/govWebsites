# This document creates a list of all the files that were downloaded with wget
# and then merges it with the city metadata
# the resultant citydocs.rdata file should be used as input for convertToText.R
# the resultant city-specific rdata files will then further be processed in preprocessingPart2.R

#setwd("govWebsites")

library('stringr')
library('urltools') #used toget the domain from each url
library('tools')

# ---- #

#get metadata
load("rfiles/allURLs.rdata")

#get URLs verified through Python Selenium
websiteUrls$urls_verified <- readLines("websites/urls_verified.txt")
#get the domain from each url
websiteUrls$urls_verified <- url_parse(websiteUrls$urls_verified)$domain
#make some corrections
websiteUrls$urls_verified[websiteUrls$urls_verified=="www.atticaonline.com"] <- "attica-in.gov"
websiteUrls$urls_verified[websiteUrls$urls_verified=="www.unioncity-in.gov"] <- "unioncity-in.com"

# ---- #

path <- "/home/mneumann/hd2/govWebsites"

f <- list.files(path, recursive = T) #create a list of all files in all subdirectories
f <- f[!stringr::str_detect(f, "[^\\x00-\\x7F]")]

#file types
ext <- file_ext(f) #get file extension
folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]

#store objects in a data frame
d <- data.frame(path = str_c(path, f, sep = "/"), 
                folder = str_c(path, folder, sep = "/"),
                filename,
                ext,
                stringsAsFactors = F)
d <- subset(d, filename != "")

#new files

#only txt, pdf, html, doc, or docx
d <- d[d$ext %in% c('txt', 'pdf', 'html', 'doc', 'docx'),]

#remove some files that crash readtext
#everything in this folder causes some problems
d <- d[-which(str_detect(d$path, "/home/mneumann/hd2/govWebsites/bloomington.in.gov/trades/parcel/(.*?).pdf")),]

d$path <- str_replace_all(d$path, "\\[", "\\\\[")
d$path <- str_replace_all(d$path, "\\]", "\\\\]")

#save
d$iter <- 1:nrow(d)
save(d, file = "rfiles/docnames.rdata")

# ---- #

#load the document paths
#load("rfiles/doc_chunks/docnames.rdata")

#function to extract city from directory
extractCity <- function(path){
  
  city <- str_replace(path, "/home/mneumann/hd2/govWebsites/", "")
  city <- str_extract(city, "^(.*?)\\/")
  city <- str_replace(city, "\\/", "")
  return(city)
  
}
d$city <- extractCity(d$path)
#character vector of city base urls
cities <- unique(d$city)

#kick out the documents whose city has less than 5 documents
citytable <- data.frame(table(d$city))
d <- d[!d$city%in%citytable$Var1[citytable$Freq<5],]
rm(citytable)
#re-do character vector of city base urls
cities <- unique(d$city)

#which chunk file is a given document in?
# ab <- list()
# chunk_size <- 10000
# for (i in seq(1, nrow(d), chunk_size)) {
#   seq_size <- chunk_size
#   if ((i + seq_size) > nrow(d)) seq_size <- nrow(d) - i + 1
#   ab[[i]] <- paste0("rfiles/doc_chunks/parsedtexts_", i, "_", (i+seq_size-1), ".rdata")
# }
# ab <- do.call(c, ab)
# ab[length(ab)] <- "rfiles/doc_chunks/parsedtexts_1520001_1528747.rdata"
# abc <- rep(ab, each = 10000)
# d$parsedtextfile <- abc[1:nrow(d)]
# rm(ab, abc, i, seq_size, chunk_size)

# ---- #

## merge the metadata with the document paths
#test which cities still don't work
websiteUrls2 <- subset(websiteUrls, select = c(urls_verified, State))
cities2 <- data.frame(urls_verified = cities)
test <- merge(websiteUrls2, cities2, by.y = "urls_verified", all = T)
print(test$urls_verified[is.na(test$State)==T])

#manually fix these:
d$city[d$city=="charmeck.org"] <- "charlottenc.gov"
websiteUrls$urls_verified[websiteUrls$State_City=="New York_Peekskill"] <- "www.cityofpeekskill.com"
websiteUrls$urls_verified[websiteUrls$urls_verified=="springsgov.com"] <- "www.springsgov.com"
#re-do character vector of city base urls
cities <- unique(d$city)
rm(websiteUrls2, cities2, test)

#do the actual merge
d <- merge(d, websiteUrls, by.x = "city", by.y = "urls_verified", all = T)

#cities that still need to be downloaded/fixed
todo <- d[is.na(d$path)==T,]

#remove the above from the big data frame
d <- d[is.na(d$path)==F,]

save(d, file = "rfiles/citydocs.rdata")
save(todo, file = "rfiles/citydocs_todo.rdata")

rm(todo, websiteUrls)

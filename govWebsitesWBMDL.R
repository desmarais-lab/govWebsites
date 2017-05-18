library("stringr")

load(file = "data/URLs_IN.rdata")

source("wbmdownloader.R")

#for downloading multiple snapshots
#for(i in 1:nrow(URLs)){
#  for(j in 1:length(URLs$snapshots[[i]])){
#    wbmdownloader(website = URLs$Website[i],
#                  from = URLs$snapshots[[i]][j],
#                  to = URLs$snapshots[[i]][j],
#                  path = str_c("./WBM/", URLs$Name[i], "/", URLs$snapshots[[i]][j]))
#  }
#}

#before
for(i in 16:nrow(URLs)){
    wbmdownloader(website = URLs$Website[i],
                  from = 20141103,
                  to = 20151102,
                  path = str_c("./websites/before/", URLs$foldername[i]))
}

#after
for(i in 1:nrow(URLs)){
    wbmdownloader(website = URLs$Website[i],
                  from = 20160101,
                  to = 20161231,
                  path = str_c("./websites/after/", URLs$foldername[i]))
}
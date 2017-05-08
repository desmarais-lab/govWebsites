library("stringr")

load(file = "data/URLs2.rdata")

source("wbmdownloader.R")

for(i in 1:nrow(URLs)){
  for(j in 1:length(URLs$snapshots[[i]])){
    wbmdownloader(website = URLs$Website[i],
                  from = URLs$snapshots[[i]][j],
                  to = URLs$snapshots[[i]][j],
                  path = str_c("./WBM/", URLs$Name[i], "/", URLs$snapshots[[i]][j]))
  }
}
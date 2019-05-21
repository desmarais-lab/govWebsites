library(stringr)

load("out/results_boilerpipe_KeepEverything.rdata")
load("out/results_readtext.rdata")

#combine
results_parsed <- rbind(results_html, results_nonhtml)
rm(results_html, results_nonhtml)

#remove empties
results_parsed <- results_parsed[results_parsed$text!="",]

#sort by id
results_parsed <- results_parsed[order(as.numeric(str_remove(results_parsed$id, "file"))),]

#save
save(results_parsed, file = "out/results_parsed_all_KeepEverything.rdata")

# load("Alaska_Anchorage.rdata")
# f <- list.files("../city_chunks_unprocessed/")
# files <- list()
# for(i in 1:length(f)){
#   load(f[i])
#   a$city <- str_remove(f[i], ".rdata")
#   files[[i]] <- a
# }
# 
# files[[188]] = NULL
# dd <- rbindlist(files)
# save(dd, file = "out/all_non_html.rdata")
# 
# load("out/citydocs.rdata")
# d <- d[d$ext!="html",]
# test = match(dd$path, d$path)

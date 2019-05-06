library(stringr)

load("out/results_boilerpipe.rdata")
load("out/results_readtext.rdata")

results_parsed <- rbind(results_html, results_nonhtml)
rm(results_html, results_nonhtml)
results_parsed <- results_parsed[order(as.numeric(str_remove(results_parsed$id, "file"))),]

save(results_parsed, file = "out/results_parsed_all.rdata")

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

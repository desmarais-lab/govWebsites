library(stringr)

#load the extracted texts
load("./out/results_parsed_all.rdata")
#rename because "parsed" is a bit misleading in the context of this script
extracted <- results_parsed
rm(results_parsed)
#load the metadata, find out which id belongs to which city
load("./out/citydocs.rdata")
d <- subset(d, select = c(id, State_City, path))
extracted$city <- d$State_City[match(extracted$id, d$id)]
extracted$path <- d$path[match(extracted$id, d$id)]
extracted$path <- str_remove(extracted$path, "/home/mneumann/hd2/govWebsites/")
all_cities <- unique(extracted$city)

#----
# Parse each city

for(i in 1:length(all_cities)){
  #subset the data table to this city only
  extracted_city <- extracted[extracted$city == all_cities[i],]
  #path to save the rdata file in
  citypath <- paste0("out/results_by_city/", all_cities[i], ".rdata")
  #save
  save(extracted_city, file = citypath)
}

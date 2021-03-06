library('stringr')
set.seed(1)

# ----

f <- list.files("../04_parseText/out/city_chunks_unprocessed", full.names = T)
f_file <- list.files("../04_parseText/out/city_chunks_unprocessed")

selected_cities <- c("../04_parseText/out/city_chunks_unprocessed/Indiana_Indianapolis.rdata",
                     "../04_parseText/out/city_chunks_unprocessed/Louisiana_Shreveport.rdata",
                     "../04_parseText/out/city_chunks_unprocessed/New York_New York City.rdata",
                     "../04_parseText/out/city_chunks_unprocessed/California_Los Angeles.rdata",
                     "../04_parseText/out/city_chunks_unprocessed/Washington_Seattle.rdata")

selected_cities <- which(f%in%selected_cities)
f <- f[selected_cities]
f_file <- f_file[selected_cities]
testDF <- list()
citySamples <- list()
#loop over cities
for(city in 1:length(f)){
  
  load(f[city])
  a <- a[!a$text=="",]
  a <- a[!a$text==" ",]
  
  #Seattle has some very large GIS maps which quanteda doesn't like at all
  if(city==5){
    gisWebplots <- which(str_detect(a$path, "\\/home\\/mneumann\\/hd2\\/govWebsites\\/www.seattle.gov\\/dpd\\/Research\\/gis\\/webplots\\/*"))
    a <- a[-gisWebplots,]
  }
  
  source(textConnection(readLines("../06_preprocessing/preprocessCityPart1.R")[1:47]))
  #source("preprocessingQuanteda.R")
  linesTable <- data.frame(linesRemove)
  linesTable$linesRemove <- as.character(linesTable$linesRemove)
  
  #remove empty lines
  linesTable <- linesTable[linesTable$linesRemove!="",]
  
  #record the total number of lines per city
  lineInfo <- paste(str_replace(f_file[city], ".rdata", ""), sum(linesTable$Freq), length(linesTable$Freq), sep = ",")
  write.table(lineInfo, file = "data/cityLines.txt", append = T, col.names = F, row.names = F, quote = F)

  #create the variable to be coded
  linesTable$class <- 0
  #include the city name
  linesTable$city <- str_replace(f_file[city], ".rdata", "")
  
  #sample from the lines table, with probability weights proportional to how often a line occurs
  linesTable$p <- linesTable$Freq/sum(linesTable$Freq)
  linesSample <- linesTable[sample(c(1:nrow(linesTable)), 100, F, linesTable$p),]
  
  #for each of the sampled lines, find its relative position within the documents it occurs in
  linesSample$medianDocMidDist <- NA
  #iterate over samples
  for(k in 1:nrow(linesSample)){
    
    docLineDistances <- list()
    docLineDocMatches <- list()
    
    #iterate over documents
    for(j in 1:length(tt2)){
      docLineMatches <- which(tt2[[j]]==linesSample$linesRemove[k])
      if(length(docLineMatches)>0){
        docLen <- length(tt2[[j]])
        docMidpoint <- docLen/2
        docLineMidpointDist <- abs(docMidpoint-docLineMatches)
        docLineMidpointDistRelative <- docLineMidpointDist/docLen
        
        docLineDistances[[j]] <- docLineMidpointDistRelative
        
      }
    }
    
    linesSample$medianDocMidDist[k] <- median(unlist(docLineDistances))
    
  }
  
  citySamples[[city]] <- linesSample
  
}

citySamplesConc <- do.call(rbind, citySamples)

save.image("rfiles/classifierTrainingDataNew.rdata")

write.csv(citySamplesConc, "data/classifierTrainingDataUncoded.csv")

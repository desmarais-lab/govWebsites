#create corpus
crps <- corpus(d$doc)
#set party docvar
docvars(crps, "Party") <- d$winner
docvars(crps, "City") <- d$Name
#convert to dfm
dfmIN <- dfm(crps)
#term freq
tf <- colSums(dfmIN)
#city names
citynames <- unique(d$Name)

#loop over cities

for(i in 1:length(citynames)){

  dfmCity <- dfm_subset(dfmIN, City == citynames[i])
  tfCity <- colSums(dfmCity)
  #tibble with words and frequencies
  tfCityTibble <- tibble(tfCity = tfCity, wordCity = names(tfCity))
  tfTibble <- tibble(tfCorpus = tf, wordCorpus = names(tf))
  #merge
  tfComparison <- merge(tfCityTibble, tfTibble, by.x = "wordCity", by.y = "wordCorpus")
  #% occurs within city
  tfComparison$tfCityPCT <- tfComparison$tfCity/sum(tfComparison$tfCity)
  #% occurs across corpus
  tfComparison$tfCorpusPCT <- tfComparison$tfCorpus/sum(tfComparison$tfCorpus)
  #difference
  tfComparison$tfPCTDiff <- tfComparison$tfCityPCT-tfComparison$tfCorpusPCT
  #order
  tfComparison <- tfComparison[order(tfComparison$tfPCTDiff, decreasing = T),]
  #define threshold
  threshold <- 0.001
  #delete everything above
  delete <- tfComparison$wordCity[tfComparison$tfPCTDiff>threshold]
  
  d$doc[d$Name == citynames[i]] <- removeWords(d$doc[d$Name == citynames[i]], delete)
  
}

#remove extraneous whitespaces
d$doc <- gsub("\\s+"," ", d$doc)
d$doc <- str_trim(d$doc, side = "both")

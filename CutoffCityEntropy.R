
city.entropies <- list()

for(i in 1:20){
  
  #load("rfiles/docDuplicatesHash.Rdata")
  #load("rfiles/dIN2.rdata")
  
  cutoff <- i
  
  #source("functions/preprocessingTesting.R")
  
  load(paste0("./../Dropbox/govWebsitesMovingVan/d_test_K", cutoff, ".rdata"))
  
  source("functions/malletTraining.R")
  
  doc.topics <- mallet.doc.topics(topic.model, smoothed = F, normalized = F)
  
  source("functions/entropy.R")
  
  city.entr <- data.frame(x = city_entropy(doc.topics, d))
  
  city.entropies[[i]] <- city.entr

}

#create a density plot with one line for each cutoff value
kCityEntropyVector <- unlist(city.entropies)
#repeat the cutoff values, as many times as there are lines left at that cutoff
kCutoffsVector <- rep(1:20, 200)
#put in dataframe
entropy.df <- data.frame(Entropy = kCityEntropyVector,
                         Cutoff = kCutoffsVector)

save.image("rfiles/CutoffCityEntropy.rdata")

m1 <- ggplot(entropy.df, aes(x = kCityEntropyVector, 
                                   colour=kCutoffsVector, 
                                   group=kCutoffsVector))
m1 <- m1 + geom_density(fill = NA, size = .2, alpha = 0.1)
m1 <- m1 + labs(x = "Entropy (between cities, for each topic)", y = "Density", color = "Cutoff")
#ggsave(filename = "paper/figures/CutoffCityEntropy.pdf", plot = m1, width = 8, height = 6)

#multiply entropy values by number of tokens assigned to topic

topic.tokens <- apply(doc.topics, 2, sum)

city.entropies.weighted <- city.entropies

for(i in 1:length(city.entropies)){
  
  city.entropies.weighted[[i]] <- city.entropies[[i]]*topic.tokens
  
}

#create a density plot with one line for each cutoff value
kCityEntropyVector <- unlist(city.entropies.weighted)
#repeat the cutoff values, as many times as there are lines left at that cutoff
kCutoffsVector <- rep(1:20, 200)
#put in dataframe
entropy.df <- data.frame(Entropy = kCityEntropyVector,
                         Cutoff = kCutoffsVector)

m1 <- ggplot(entropy.df, aes(x = kCityEntropyVector, 
                             colour = kCutoffsVector, 
                             group = kCutoffsVector))
m1 <- m1 + geom_density(fill = NA, size = .2, alpha = 0.1)
m1 <- m1 + labs(x = "Weighted entropy (between cities, for each topic)", y = "Density", color = "Cutoff")
#ggsave(filename = "paper/figures/CutoffCityEntropyWeighted.pdf", plot = m1, width = 8, height = 6)

library(ggplot2)
library(dplyr)
library(tidyr)

## Retrieve a matrix of topic weights for every document
## I.e. for each document, the probability of being assigned to a topic
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)

dim(doc.topics) #12397 documents; 100 topics


apply(doc.topics, 2, mean)

## Subset the matrix into two matrices, one containing
# documents from Democratic cities, the other from Republican cities
republican <- doc.topics[which(d$winner == "Republican"),]
democratic <- doc.topics[which(d$winner == "Democratic"),]



## Calculate the median (or mean) topic weight across documents
rep.topic.prop <- apply(republican, 2, median)
dem.topic.prop <- apply(democratic, 2, median)
rep.topic.prop <- apply(republican, 2, mean)
dem.topic.prop <- apply(democratic, 2, mean)

## Plot distribution of both
weights.tibble <- tibble(rep = rep.topic.prop, dem = dem.topic.prop) %>%
  gather(party, weight)

ggplot(weights.tibble, aes(weight, color = party)) + geom_density()

#For each topic, calculate the absolute difference
#between the weight of this topic in Democratic and Republican cities/documents
topicdiffs <- abs(dem.topic.prop-rep.topic.prop) %>% tibble(diffs = .)

#Plot
ggplot(topicdiffs, aes(diffs)) + geom_histogram(bins = 100)

hist(topicdiffs)
plot(density(topicdiffs))

which(topicdiffs>.05)

bigdiffs <- which(topicdiffs>0.05)

dem.topic.prop[bigdiffs]
rep.topic.prop[bigdiffs]

for(i in 1:length(bigdiffs)){
  print(mallet.top.words(topic.model, topic.words[bigdiffs[i],]))
}

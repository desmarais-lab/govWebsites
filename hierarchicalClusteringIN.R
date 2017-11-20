library('quanteda')
library('dendextend')
library('magrittr')
library('lsa')

# load data
load("rfiles/d.Rdata")

# convert to quanteda dfm
crps <- corpus(d$doc)
docvars(crps, "Party") <- d$winner
dfmLA <- dfm(crps)

# tf-idf representation
dfmLA <- tfidf(dfmLA)
m <- as.matrix(dfmLA)

# euclidean distance
distMatrix <- dist(m, method="euclidean")

# cosine similarity (requires term-document matrix, so transpose first)
#distMatrix <- cosine(t(m))

# hierarchical clustering
groups <- hclust(distMatrix, method = "ward.D")
groups$labels <- d$winner
groups$labels[groups$labels=="Republican"] <- "R"
groups$labels[groups$labels=="Democratic"] <- "D"
groups2 <- groups
groups2$height <- log(groups2$height)
hcd <- as.dendrogram(groups2)
# color by document party and plot
colr <- rep("blue", nrow(d))
colr[d$winner=="Republican"] <- "red"
ppi <- 300
png("paper/figures/clusteringIN.png", width=8*ppi, height=6*ppi, res=ppi)
par(las = 1)
hcd %>% set("labels_col", colr) %>% # change color
  set("labels_cex", .2) %>% # Change size
  plot()
dev.off()

#everything takes quite a while, so save the results
save.image("rfiles/hclustIN.rdata")

#Shannon Entropy
entropy <- function(freqs){
  return(-sum(freqs * log2(freqs)))
}

# =============================================================================

#explore what the documents cluster into if k = 10
nclust <- 10
groups$labels <- 1:nrow(d)
test <- cutree(groups, k = nclust)
test.names <- names(test)
cluster.categories <- data.frame(cluster = test, category = test.names)
#test2 <- xtabs(~ category + cluster, data = cluster.categories)

#for each cluster
for(i in 1:nclust){

clust.indices <- cluster.categories$category[cluster.categories$cluster==i]
  
ind <- sample(clust.indices, ifelse(length(clust.indices)>=10, 10, length(clust.indices)))

#for each document within the cluster
for(j in 1:length(ind)){

  #get the document
  current.doc <- d$doc[ind[j]]
  #alternatively, read in original document
  current.doc.original <- readLines(d$path[ind[j]])

  separator <- "========================"
  
  # write three separators consisting of city and party
  write(paste("\n", separator, d$Name[ind[j]], separator, d$winner[ind[j]], separator, "\n",
              separator, d$Name[ind[j]], separator, d$winner[ind[j]], separator, "\n",
              separator, d$Name[ind[j]], separator, d$winner[ind[j]], separator, "\n"), 
        paste("./diagnostics/hclustering/cluster", i, ".txt", sep = ""), append = T)
  
  #write document
  write(current.doc, paste("./diagnostics/hclustering/cluster", i, ".txt", sep = ""), append = T)


  # write three separators consisting of city and party
  write(paste("\n", separator, d$Name[ind[j]], separator, d$winner[ind[j]], separator, "\n",
              separator, d$Name[ind[j]], separator, d$winner[ind[j]], separator, "\n",
              separator, d$Name[ind[j]], separator, d$winner[ind[j]], separator, "\n"),
        paste("./diagnostics/hclustering/original_cluster", i, ".txt", sep = ""), append = T)
  #write document
  write(current.doc.original, paste("./diagnostics/hclustering/original_cluster", i, ".txt", sep = ""), append = T)
  
  
}}

# =============================================================================

#Clustering for cities

groups$labels <- d$Name

nclust <- 50

entropy.values <- list()

for(i in 1:nclust){
  #cuttree to decide number clusters
  test <- cutree(groups, k = i)
  #party/city name
  test.names <- names(test)
  #cluster according to party/city
  cluster.categories <- data.frame(cluster = test, category = test.names)
  test2 <- xtabs(~ category + cluster, data = cluster.categories)
  #add a bit of noise to the frequencies that are 0
  test2[test2==0] <- test2[test2==0]+0.0000000000000001
  #represent as proportions instead of frequencies
  props <- as.numeric(test2/sum(test2))
  #calculate entropy and save in list
  entropy.values[[i]] <- entropy(props)
}

library("ggplot2")

df <- data.frame(Entropy = unlist(entropy.values), Clusters = 1:nclust)

ggplot(df, aes(x = Clusters, y = Entropy)) +
  geom_point()

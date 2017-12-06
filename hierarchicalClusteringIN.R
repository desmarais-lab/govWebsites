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
nclust <- 32
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

nclust <- 100

entropy.values <- list()

chisq.pval <- list()

for(i in 1:nclust){
  #cuttree to decide number clusters
  test <- cutree(groups, k = i)
  #party/city name
  test.names <- names(test)
  #cluster according to party/city
  cluster.categories <- data.frame(cluster = test, category = test.names)
  test2 <- xtabs(~ category + cluster, data = cluster.categories)
  
  chisq.pval[[i]] <- chisq.test(test2)$p.value
  
  
  # #add a bit of noise to the frequencies that are 0
  # test2[test2==0] <- test2[test2==0]+0.0000000000000001
  # #represent as proportions instead of frequencies
  # props <- as.numeric(test2/sum(test2))
  # #calculate entropy and save in list
  # entropy.values[[i]] <- entropy(props)
}

chisq.df <- data.frame(clusters = 1:100, pvalues = unlist(chisq.pval))

#ggplot(chisq.df, aes(x = clusters, y = pvalues)) + geom_point() + 
#  ylim(0,10e-200)

# library("ggplot2")
# 
# df <- data.frame(Entropy = unlist(entropy.values), Clusters = 1:nclust)
# 
# ggplot(df, aes(x = Clusters, y = Entropy)) +
#   geom_point()

#------------------------------------------------------------------------------

# Make heatmaps for city documents per cluster, for different numbers of clusters

library(reshape2)
library(ggplot2)

#determines whether city or party
groups$labels <- d$Name

nclust_seq <- c(10, 20, 30, 40, 50, 100)

for(i in 1:length(nclust_seq)){
  
  nclust = nclust_seq[i]
  
  # Cut the tree into clusters
  test <- cutree(groups, k = nclust)
  # Get the city names
  test.names <- names(test)
  # cluster according to party/city
  cluster.categories <- data.frame(cluster = test, category = test.names)
  # Crosstabs
  test2 <- xtabs(~ category + cluster, data = cluster.categories)
  
  chisq.test(test2)
  
  # ----------------------------------------
  
  # Plot the data as a heatmap with borderes colored by party
  # and cell color by the number of city documents in a cluster
  
  # reshape to long format
  test2 <- melt(as.matrix(test2))
  
  # merge in party data
  d2 <- d[!duplicated(d$Name),]
  d2 <- subset(d2, select = c("Name", "winner"))
  d3 <- merge(test2, d2, by.x = "category", by.y = "Name")
  
  # rename variables
  names(d3) <- c("City", "Cluster", "Documents", "Party")
  
  # re-order the city variable so the city names appear in correct alphabetic order
  #d3$City <- factor(d3$City, levels = sort(unique(d3$City), decreasing = T))
  
  # plot
  g1 <- ggplot(d3,
               aes(x = Cluster, y = City, color = Party, fill = log(Documents))) +
    geom_tile(width = 0.95, height = 0.95, size = .5) + 
    guides(fill = F, color = F) + 
    scale_color_manual(values = alpha(c("steelblue1", "tomato1"), .6)) +
    coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
  g1
  
  #save
  plotwidth <- 4
  #32 = number of cities
  #make plot height a function of width and number of clusters
  #but at least 2.5
  plotheight <- max(plotwidth*(nclust/32), 2.5)
  
  ggsave(g1, 
         file = paste0("./paper/figures/heatmap_hclust_", nclust, ".png"), 
         width = plotwidth, height = plotheight)
  
}



#------------------------------------------------------------------------------

#calculate distribution of clusters across cities

groups$labels <- d$Name
nclust <- 32
test <- cutree(groups, k = nclust)
test.names <- names(test)
cluster.categories <- data.frame(cluster = test, category = test.names)
test2 <- xtabs(~ category + cluster, data = cluster.categories)
#marginal.plot(test2)
#test2[1,]/apply(test2, 2, sum)
round(prop.table(test2, margin = 2), 2)

#calculate distribution of clusters across parties

groups$labels <- d$winner
nclust <- 32
test <- cutree(groups, k = nclust)
test.names <- names(test)
cluster.categories <- data.frame(cluster = test, category = test.names)
test2 <- xtabs(~ category + cluster, data = cluster.categories)
#marginal.plot(test2)
#test2[1,]/apply(test2, 2, sum)
round(prop.table(test2, margin = 2), 2)
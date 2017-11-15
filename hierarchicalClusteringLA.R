library('quanteda')
library('dendextend')
library('magrittr')
library('lsa')

# load data
load("rfiles/dLA.rdata")

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
groups <- hclust(distMatrix, method="ward.D")
#sub_grp <- cutree(groups, k = 4)
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
png("paper/figures/clusteringLA.png", width=8*ppi, height=6*ppi, res=ppi)
par(las = 1)
hcd %>% set("labels_col", colr) %>% # change color
  set("labels_cex", .2) %>% # Change size
  plot()
dev.off()

#everything takes quite a while, so save the results
save.image("rfiles/hclustLA.rdata")

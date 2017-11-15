library('pbapply')
library('tidyr')
library('ggplot2')

# Load the documents
load("rfiles/dIN2.rdata")
# Load the duplicates indicators
load("rfiles/docDuplicatesMatch.Rdata")

#try out the function at different thresholds
cleanupTest <- function(i, k = 10){
  keep <- which(docDuplicates[[i]]<=k)
  cleanedDoc <- d$doc[[i]][keep]
  return(cleanedDoc)
}

kCutoffs <- 1:20
kLinesKept <- rep(0, length(kCutoffs))
for(i in 1:length(kCutoffs)){
  test <- pblapply(1:nrow(d), cleanupTest, k = kCutoffs[i])
  kLinesKept[i] <- sum(unlist(lapply(test, length)))
}

kLinesKeptMatch <- kLinesKept

#Do the same, but for the hashmap version

# Load the documents
#load("rfiles/dIN2.rdata")
# Load the duplicates indicators
load("rfiles/docDuplicatesHash.Rdata")

kCutoffs <- 1:20
kLinesKept <- rep(0, length(kCutoffs))
for(i in 1:length(kCutoffs)){
  test <- pblapply(1:nrow(d), cleanupTest, k = kCutoffs[i])
  kLinesKept[i] <- sum(unlist(lapply(test, length)))
}

# Plot the cutoff against the number of lines kept
LinesCutoff <- data.frame(kCutoffs, hashmap = kLinesKept, match = kLinesKeptMatch)
LinesCutoff <- gather(LinesCutoff, Method, LinesKept, hashmap, match)
ggplot(LinesCutoff, aes(x = kCutoffs, y = LinesKept, color = Method)) +
  geom_point() +
  labs(x = "Cutoff", y = "Number of lines kept")
ggsave(file = "paper/figures/linesCutoffIN.pdf", width = 8, height = 6)

library('pbapply')
library('tidyr')

# Load the documents
load("rfiles/dIN2.rdata")
load("rfiles/docDuplicatesHash.Rdata")

kCutoffs <- 1:20
kLinesKept <- rep(0, length(kCutoffs))
kCharsKept <- list()

for(i in 1:length(kCutoffs)){
  test <- pblapply(1:nrow(d), cleanupTest, k = kCutoffs[i])
  kLinesKept[i] <- sum(unlist(lapply(test, length)))
  kCharsKept[[i]] <- as.numeric(sapply(unlist(test), nchar))
}

#----------------------------------------------------------
# Plot the cutoff against the number of characters kept
charsCutoff <- data.frame(chars = unlist(lapply(kCharsKept, length)),
                          cutoff = 1:20)
ggplot(charsCutoff, aes(x = kCutoffs, y = chars)) +
  geom_point() +
  labs(x = "Cutoff", y = "Number of characters kept")
ggsave(file = "paper/figures/charsCutoffIN.pdf", width = 8, height = 6)

#----------------------------------------------------------
#create a density plot with one line for each cutoff value
kCharsKeptVector <- unlist(kCharsKept)
#repeat the cutoff values, as many times as there are lines left at that cutoff
kCutoffsVector <- rep(1:20, unlist(lapply(kCharsKept, length)))
#put in dataframe
charsCutoffLines <- data.frame(linelength = kCharsKeptVector,
                          cutoff = kCutoffsVector)
#plot
m1 <- ggplot(charsCutoffLines, aes(x=kCharsKeptVector, colour=kCutoffsVector, 
                                   group=kCutoffsVector))
m1 <- m1 + geom_density(fill=NA, size=.2, alpha=0.1)
m1 <- m1 + xlim(0, 150)
m1 <- m1 + labs(x = "Line length (characters)", y = "Density", color = "Cutoff")
ggsave(filename = "paper/figures/linesCutoffIN.pdf", plot = m1, width = 8, height = 6)

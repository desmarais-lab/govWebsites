library(quanteda)
library(tidyr)
library(ggplot2)

# presidential inagural addresses corpus
speeches <- corpus(data_corpus_inaugural$documents$texts)
docvars(speeches, "Party") <- rev(c("R","D","D","R","R","D","D","R","R","R","D","R","R", rep(NA, 45)))
docvars(speeches, "Year") <- data_corpus_inaugural$documents$Year

# city documents corpus
load(paste0("rfiles/d_", "IN", ".rdata"))
citydocs <- corpus(d$doc)
docvars(citydocs, "Party") <- d$Party
docvars(citydocs, "Year") <- d$Year

#combine the two corpora
b <- speeches+citydocs


presDfm <- dfm(corpus_subset(b, Year > 1980),
               remove = stopwords("english"), stem = T, remove_punct = TRUE)

rownames(presDfm)[1:10] <- paste(data_corpus_inaugural$documents$President[49:58],
                                 data_corpus_inaugural$documents$Year[49:58], sep = "_")

presSimil <- textstat_simil(presDfm,
                            rownames(presDfm)[1:10], 
                            margin = "documents", method = "cosine")

presSimilRep <- presSimil[which(presDfm@docvars$Party=="Republican"),] %>%
  as.data.frame() %>%
  gather()


presSimilDem <- presSimil[which(presDfm@docvars$Party=="Democratic"),] %>%
  as.data.frame() %>%
  gather()

presDMeanD <- mean(presSimilDem$value[which(presSimilDem$key %in% c("Clinton_1993", "Clinton_1997", "Obama_2009", "Obama_2013"))])
presRMeanD <- mean(presSimilDem$value[which(presSimilDem$key %in% c("Reagan_1981", "Reagan_1985", "Bush_1989", "Bush_2001", "Bush_2005", "Trump_2017"))])

presDMeanR <- mean(presSimilRep$value[which(presSimilRep$key %in% c("Clinton_1993", "Clinton_1997", "Obama_2009", "Obama_2013"))])
presRMeanR <- mean(presSimilRep$value[which(presSimilRep$key %in% c("Reagan_1981", "Reagan_1985", "Bush_1989", "Bush_2001", "Bush_2005", "Trump_2017"))])

pR <- ggplot(presSimilRep, aes(x = value, color = key)) + geom_density() +
  geom_vline(aes(xintercept = presDMeanR), colour = "blue") +
  geom_vline(aes(xintercept = presRMeanR), colour = "red") +
  xlim(c(0, 0.3))
pR
ggsave("paper/figures/inauguralRep.pdf")

pD <- ggplot(presSimilDem, aes(x = value, color = key)) + geom_density() +
  geom_vline(aes(xintercept = presDMeanR), colour = "blue") +
  geom_vline(aes(xintercept = presRMeanR), colour = "red") +
  xlim(c(0, 0.3))
pD
ggsave("paper/figures/inauguralDem.pdf")
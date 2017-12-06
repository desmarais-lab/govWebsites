library('ggplot2')

load("/home/markus/govWebsites/rfiles/docDuplicates.Rdata")
a <- data.frame(unlist(docDuplicates))

g1 <- ggplot(a, aes(a$unlist.docDuplicates.)) + 
  geom_density() +
  labs(x = "Number of duplicate lines")

ggsave(g1, "diagnostics/duplicates_density.pdf")

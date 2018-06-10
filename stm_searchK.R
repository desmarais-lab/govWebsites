library('quanteda')
library('stm')

load(file = "rfiles/websiteMetadata.rdata")
load(file = "rfiles/allDocuments.rdata")

d <- merge(d, websiteMeta, by.x = "city", by.y = "State_City")
rm(websiteMeta)
d$State <- as.factor(d$State)
d <- subset(d, State %in% c("Indiana", "Louisiana", "New York", "California", "Washington", "Texas"))

d <- d[sample(c(1:nrow(d)), 50000),]

# d$doc_id <- paste("doc", 1:nrow(d))
# crps <- corpus(d)
# dtm <- dfm(crps)

temp <- textProcessor(documents = d$text, metadata = d, 
                      removestopwords = F, removenumbers = F, removepunctuation = F,
                      stem = F, wordLengths = c(0,Inf), sparselevel = 1)

# search for the optimal k
set.seed(123)
K <- c(10, 25, 50, 75, 100, 200, 300) 
kresult <- searchK(temp$documents, temp$vocab, K, prevalence = ~party + State + B01001_001E + B19013_001E, data = d)
pdf("paper/figures/stm_searchK_50000.pdf")
plot(kresult)
dev.off()

#save sessison
save.image("rfiles/stm_searchK_50000.rdata")

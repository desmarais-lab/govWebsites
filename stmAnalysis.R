library('stm')

load("rfiles/d.Rdata")

docsIN <- d$doc
meta <- subset(d, select = c('Name', 'winner'))
processed <- textProcessor(docsIN, metadata = meta)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


#data <- read.csv("poliblogs2008.csv")
#processed <- textProcessor(data$documents, metadata = data)
#out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
#docs <- out$documents
#vocab <- out$vocab
#meta <-out$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab,
                        processed$meta, lower.thresh = 15)

stmFit <- stm(documents = out$documents, vocab = out$vocab,
                       K = 60, prevalence =~ Name + winner,
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral")
summary(stmFit)

prep <- estimateEffect(1:20 ~ Name + winner, stmFit,
                       meta = out$meta, uncertainty = "Global")

summary(prep, topics=20)

save.image('rfiles/stmSession2.RData')

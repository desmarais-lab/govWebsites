options(java.parameters="-Xmx4g")   # optional, but more memory for Java helps
library("dfrtopics")

mallet.instances <- mallet.import(id.array = make.unique(d$Name),
                                  text.array = d$doc,
                                  stoplist.file = "./rfiles/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

m <- train_model(mallet.instances, 
                 n_topics=100,
                 n_iters=300,
                 seed=1066)

write_diagnostics(m)
mallet_diag <- read_diagnostics("diagnostics.xml")
mallet_diag$topics$coherence

mallet_diag$topics$tokens


plot(mallet_diag$topics$tokens, mallet_diag$topics$coherence)

plot(mallet_diag$topics$tokens, apply(doc_topics(m), 2, mean))

mat1 <- doc_topics(m) %>% normalize_rows()


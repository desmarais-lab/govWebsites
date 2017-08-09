library('quanteda')
library('tibble')
library('dplyr')

load("rfiles/d.Rdata")
d <- d[d$Name%in%c("Attica", "Auburn", "Brazil", "Gary"),]


#tf-idf for the whole corpus

#create corpus
crps <- corpus(d$doc)
#set party docvar
docvars(crps, "Party") <- d$winner
docvars(crps, "City") <- d$Name
#convert to dfm
dfm_in3 <- dfm(crps, ngrams = 3)
#calculate tf-idf
tfidf_in3 <- tfidf(dfm_in3)

#tfidf_in3[901:905,1:5]

tri_tfidf <- colSums(tfidf_in3)
#names(tri_tfidf)

a <- tibble(tfidf = tri_tfidf, token = names(tri_tfidf))
a <- a[order(a$tfidf, decreasing = T),]
a


# tf-idf for individual cities

b <- sapply(unique(docvars(crps, "City")), 
       function(x){corpus_subset(crps, City == x) %>% 
           dfm(ngrams = 3) %>%
           tfidf() %>%
           colSums})

Attica <- b$Attica
Attica <- tibble(tfidf = Attica, token = names(Attica))

Brazil <- b$Brazil
Brazil <- tibble(tfidf = Brazil, token = names(Brazil))


Att <- merge(Attica, a, by = "token")

Att$tfidfRatio <- Att$tfidf.x/Att$tfidf.y

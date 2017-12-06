## File purpose: compare lemmatized documents to non-lemmatized documents


# load both the lemmatized and original documents
load("./rfiles/d_spacyIN.rdata")
d_spacy <- d
load("./rfiles/d.Rdata")
d_spacy_text <- d_spacy$doc
d_text <- d$doc

# I removed extra whitespaces in Python, doing the 
# same for the version that didn't get lemmatized
d_text <- gsub("\\s+"," ", d_text)
library(stringr)
d_text <- str_trim(d_text)

# print some of the documents to the console to look at them manually
for(i in 1:10){
  print(d_text[i])
  print(d_spacy_text[i])
}

# count the number of characters before and after lemmatization
d_text_len <- nchar(d_text)
d_spacy_text_len <- nchar(d_spacy_text)
text_len_diff <- d_spacy_text_len/d_text_len

# density plot of the number of characters before and after lemmatization
library(ggplot2)

df <- data.frame(difference = text_len_diff)

ggplot(df, aes(x = difference)) + 
  geom_density() + 
  xlab("Proportional length of documents (in number of characters) after lemmatization versus before")

ggsave("paper/figures/lemmatization_comparison_density.pdf")


# lexical diversity

library(quanteda)

crps <- corpus(d_text)
dtm <- dfm(crps)

crps_spacy <- corpus(d_spacy_text)
dtm_spacy <- dfm(crps_spacy)

lexdiv_diff <- textstat_lexdiv(dtm_spacy)/textstat_lexdiv(dtm)

df <- data.frame(lexdiv_diff)

library(tidyr)
df <- gather(df, measure, diversity)


ggplot(df, aes(x = diversity, color = measure)) + 
  geom_density() + 
  xlab("Proportional lexical diversity after lemmatization versus before")

ggsave("paper/figures/lemmatization_comparison_diversity.pdf")

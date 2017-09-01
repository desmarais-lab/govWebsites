library("openNLP")
library("openNLPmodels.en")
library("NLP")
library("tm")

removeNamedEntity <- function(string){
  s <- as.String(string)
  ## Need sentence and word token annotations.
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  ## Entity recognition for persons.
  entity_annotator <- Maxent_Entity_Annotator()
  #entity_annotator
  annotate(s, entity_annotator, a2)
  ## Directly:
  #entity_annotator(s, a2)
  ## And slice ...
  namedEntities <- s[entity_annotator(s, a2)]
  #output <- removeWords(string, namedEntities)
  #return(output)
  return(namedEntities)
}


a <- list("My name is Markus Neumann.","Also, my name is John Francis Doe.","That being said, maybe not.","News", "Test", "you are here", "let's not forget about Duane Roderick.","Mr. Vinken is chairman")

removeNamedEntity(a)

str_c(d$doc[2], collapse = " ")

for(i in 1:10){
test <- lapply(d$doc[i], str_c, collapse = " ")
print(removeNamedEntity(test))
}

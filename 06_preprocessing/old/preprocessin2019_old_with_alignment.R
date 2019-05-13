library(textreuse)
library(quanteda)
library(spacyr)
#spacy_initialize()

load("../04_parseText/out/boilerpipe_img.rdata")

extracts <- unlist(extracts)
#parse all html documents for attica
crps <- corpus(extracts[which(d$city==unique(d$city)[1])])
docvars(crps) <- d[which(d$city==unique(d$city)[1]),]
parsedtxt <- spacy_parse(crps, tag = T, dependency = T)

#entities to remove
entity_rm <- c("CARDINAL_B", "CARDINAL_I", #numbers/counts
               "DATE_B", "DATE_I",
               "EVENT_B", "EVENT_I", #B: Twilight, September, Ordinance, I: Pull, Over, 30, Year, Awards
               "FAC_B", "FAC_I", #B: facility, i.e. McDonalds, Derrick, Sycamore, Taylor, Market, I: Street, Park, Avenue
               "GPE_B", "GPE_I", #Geopolitical Entity -- Attica, Lafayette, Laundromat, Rockville
               "LANGUAGE_B", #B: Department ???
               #"LAW_B", "LAW_I", #Keep -- Resolution, Chapter, Article, I: #, 9
               "LOC_B", "LOC_I", #Crystal, Badlands, West, Perry, I: Attachment, Street
               "MONEY_B", "MONEY_I", #B: 1,487,455, 35,000/yr, I: Million -- this seems very reliable and also interesting, even if not for our purposes
               #"NORP_B", #Nationalities or religious or political groups; keep -- Americans, Mexican, British, Jewish
               "ORDINAL_B", #first, second, 3rd
               "ORG_B", "ORG_I", #unsure -- Council, McDonald, ADA, DLZ, I: Indiana, Council, Department for Appropriations
               "PERCENT_B", "PERCENT_I", #15 % - mph a.m.-12
               "PERSON_B", "PERSON_I", #Grimmett, Ouabache, Askren, Wayne
               "PRODUCT_I", #Cole, Danko, Engine, I: St
               "QUANTITY_B", "QUANTITY_I", #54,000, 75/25, ninety
               "TIME_B", "TIME_I")#, #B: 5:00, I: P.M., hours
               #"WORK_OF_ART_B", "WORK_OF_ART_I") #B: Street, Attica, Confined, Riverboat, I: Ravine, Retirement Fund, Police

#in attica, this removes about 40k out of 240k token instances
parsedtxt <- parsedtxt[!parsedtxt$entity%in%c(entity_rm),]

#might maybe keep PROPN, but still contains a lot of mr. mrs., etc., but on the other hand also chief, mayor, etc.

#remove all non-verbs and nouns
parsedtxt <- parsedtxt[parsedtxt$pos%in%c("VERB", "NOUN"),]

#convert to tokens object
tks <- as.tokens(parsedtxt, use_lemma = T)

#remove stopwords
tks = tokens_select(tks, stopwords(), "remove")

#convert to character vectors (in list)
docs <- unlist(lapply(tks, paste, collapse = " "))

#
pipelineAlignment <- function(text1, text2){
  
  alignmentResult <- NULL
  tryCatch({alignmentResult <- align_local(text1, text2)}, 
           error = function(err){
             alignmentResult <- NULL})
  
  if(is.null(alignmentResult) == F){
    if(alignmentResult[[3]]<3){
      alignmentResult <- NULL
    }else{
      names(alignmentResult) <- c("mood_text", "tv_text", "alignment_score")
    }
  }
  
  return(alignmentResult)
  
}

for(i in 1:length(docs)){
  print(pipelineAlignment(docs[1], docs[i]))
}

for(i in 1:length(docs)){
  try({print(align_local(docs[1], docs[i]))})
}

pipelineAlignment <- function(texts){
  for(i in 1:length(texts)){
    alignmentResult <- try({align_local(docs[1], docs[30])})
    alignmentResult <- data.frame(score = alignmentResult$score,
                              a_edits = alignmentResult$a_edits,
                              b_edits = alignmentResult$b_edits,
                              doc_a = 
                              stringsAsFactors = F)
    }

}

test = TextReuseCorpus(text = docs)
test2 = align_local(test[[1]], test[[2]])
alignmentResult <- NULL

results <- list()
counter <- 1
for(a in 1:length(test)){
  for(b in 1:length(test)){
    alignmentResult <- try({align_local(docs[a], docs[b])})
    if(length(alignmentResult)>1){
      if(alignmentResult$score>=20){
      alignmentResult <- data.frame(score = alignmentResult$score,
                                    a_edits = alignmentResult$a_edits,
                                    b_edits = alignmentResult$b_edits,
                                    doc_a = a,
                                    doc_b = b,
                                    stringsAsFactors = F)
      results[[counter]] <- alignmentResult
      counter <- counter + 1
      }
    }
    alignmentResult <- NULL
  }
}

results2 <- data.table::rbindlist(results)

save.image("99_preprocessing.rdata")

results2$identical_docs <- unlist(purrr::map2(results2$a_edits, results2$b_edits, identical))
results2 <- results2[results2$doc_a != results2$doc_b,]
results3 <- results2[results2$identical_docs==F,]

results3$doc_a_text <- docs[results3$doc_a]
results3$doc_b_text <- docs[results3$doc_b]

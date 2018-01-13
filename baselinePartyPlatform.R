library(readtext)
library(stringr)
library(tm)
library(pbapply)
library('hunspell')


# Republicans
rep_ground_truth <- readtext("data/republican_platform_2016.pdf")$text
rep_ground_truth <- str_replace_all(rep_ground_truth, "[^A-Za-z ]", "")
rep_ground_truth <- tolower(rep_ground_truth)
individual_letters <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
rep_ground_truth <- removeWords(rep_ground_truth, individual_letters)
hunRemove <- function(charstring){
  charstring_split <- strsplit(charstring, " ")[[1]]
  errors <- unlist(hunspell(charstring_split))
  if(length(errors)>0){
    output <- tokens_remove(tokens(charstring), errors)
    output <- str_c(output[[1]], collapse = " ")
  }
  else{
    output <- charstring
  }
  #in case everything is removed, make an empty string
  if(identical(output, character(0))){
    output <- ""
  }
  return(output)
}
rep_ground_truth <- hunRemove(rep_ground_truth)
rep_ground_truth <- gsub("\\s+"," ", rep_ground_truth)

# Democrats
dem_ground_truth <- readtext("data/democratic_platform_2016.pdf")$text
dem_ground_truth <- str_replace_all(dem_ground_truth, "[^A-Za-z ]", "")
dem_ground_truth <- tolower(dem_ground_truth)
individual_letters <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
dem_ground_truth <- removeWords(dem_ground_truth, individual_letters)
hunRemove <- function(charstring){
  charstring_split <- strsplit(charstring, " ")[[1]]
  errors <- unlist(hunspell(charstring_split))
  if(length(errors)>0){
    output <- tokens_remove(tokens(charstring), errors)
    output <- str_c(output[[1]], collapse = " ")
  }
  else{
    output <- charstring
  }
  #in case everything is removed, make an empty string
  if(identical(output, character(0))){
    output <- ""
  }
  return(output)
}
dem_ground_truth <- hunRemove(dem_ground_truth)
dem_ground_truth <- gsub("\\s+"," ", dem_ground_truth)


# ---------------------------------------------------------

manifestos <- corpus(c(dem_ground_truth,rep_ground_truth))
docvars(manifestos, "Party") <- c("D","R")

# city documents corpus
load(paste0("rfiles/d_", "IN", ".rdata"))
citydocs <- corpus(d$doc)
docvars(citydocs, "Party") <- d$Party


#combine the two corpora
b <- manifestos+citydocs

presDfm <- dfm(b,
               remove = stopwords("english"), stem = T, remove_punct = TRUE)
rownames(presDfm)[1:10]

presSimil <- textstat_simil(presDfm,
                            rownames(presDfm)[1:2], 
                            margin = "documents", method = "cosine")

presSimilRep <- presSimil[which(presDfm@docvars$Party=="Republican"),] %>%
  as.data.frame() %>%
  gather()


presSimilDem <- presSimil[which(presDfm@docvars$Party=="Democratic"),] %>%
  as.data.frame() %>%
  gather()

presDMeanD <- mean(presSimilDem$value[which(presSimilDem$key %in% c("text1"))])
presRMeanD <- mean(presSimilDem$value[which(presSimilDem$key %in% c("text2"))])

presDMeanR <- mean(presSimilRep$value[which(presSimilRep$key %in% c("text1"))])
presRMeanR <- mean(presSimilRep$value[which(presSimilRep$key %in% c("text2"))])

pR <- ggplot(presSimilRep, aes(x = value, color = key)) + geom_density() +
  geom_vline(aes(xintercept = presDMeanR), colour = "blue") +
  geom_vline(aes(xintercept = presRMeanR), colour = "red") +
  xlim(c(0, 0.3))
pR
ggsave("paper/figures/platformRep.pdf")

pD <- ggplot(presSimilDem, aes(x = value, color = key)) + geom_density() +
  geom_vline(aes(xintercept = presDMeanR), colour = "blue") +
  geom_vline(aes(xintercept = presRMeanR), colour = "red") +
  xlim(c(0, 0.3))
pD
ggsave("paper/figures/platformDem.pdf")

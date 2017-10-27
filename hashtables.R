#Create the test dataset
load("hashtables.RData")
#d <- d[d$Name %in% c("Attica","Auburn","Batesville", "Bedford", "Lake Station"),]
#rm(list = ls()[ls()!="d"])
#save(d, file = "hashtest.rdata")
#load("hashtest.rdata")
#d <- d[sample(1:785, 100),]

library('hashmap')
library('profvis')
library('pbapply')

#remove cities with only 1 document (since that breaks the whole hashtable thing)
d <- d[!d$Name %in% names(table(d$Name)[table(d$Name)<2]),]

#create a table of the number of documents for each city
citytable <- table(d$Name)
citynames <- d$Name

#iterate over cities, creating a list for each
#each of these lists contains one numerical vector for each document
#each vector contains one element for each line in the document
#each of these elements denominates the number of times that line occurs within the city
proc_city <- function(j){

#what's the name of the current city
cityname <- names(citytable)[j]
#what is the index of the city in the list of cities
citytableindex <- as.numeric(which(names(citytable)==cityname))
#determine which row the city starts on
ifelse(citytableindex>1, citystart <- sum(citytable[1:(citytableindex-1)])+1, citystart <- 1)
#how many documents does the current city have?
citylength <- as.numeric(citytable[citytableindex])
#which indices belong to this city?
cityindices <- citystart:(citystart+citylength-1)

#---
#up until here, we basically just get the indices for the city within the data frame
#the next part is the hashmap

#concatenate documents of the current city
lines <- unlist(d$doc[cityindices])
names(lines) <- 1:length(lines)

#initialize a hashmap
#the key/value generated here doesn't matter, I never use it
HH <- hashmap('INIT', -1)

#for each in the city's documents...
for(i in 1:length(lines)){
#look for the key and see if it already has a value (i.e. the count)
value <- HH[[lines[i]]]
if(is.na(value)) {
  value = 1L #if not, make it 1
} else {
  value = value + 1L #if yes, add 1
}
#then add the result back into the hashmap
HH$insert(lines[i], value)

}

#make a quick and dirty function to find the lines...
#... of one document and get the respective values from the hashmap
duplicateLinesHH <- function(k){
  return(HH$find(d$doc[cityindices][[k]]))
}

#do this for all the documents, creating a list of vectors
#one vector for each document
#each of which contains the number of times a line in that document is present in the city
docDuplicatesHH <- pblapply(1:nrow(d[cityindices,]), duplicateLinesHH)

return(docDuplicatesHH)

}

#iterate over all cities; parallelize
#this creates a list of lists, one for each city
library('parallel')
cl <- makeForkCluster(11) #detectCores()-1
docDuplicates <- pbsapply(1:length(citytable), proc_city, cl = cl)
stopCluster(cl)

#unlist only the outer list
docDuplicates <- unlist(docDuplicates, recursive = F)



#------------------------------------------------------------------------------

#Everything from here on out is just old stuff that doesn't necessarily work (well)


profvis({

  duplicatedLines <- function(k){
    
    cityindices <- 1:nrow(d)
    cityindices <- cityindices[-k]
    
    #for each document, create the vector indicating the number of times a line occurs elsewhere
    b <- numeric(length = length(unique(d$doc[[k]])))
    #loop over all other documents
    for(i in cityindices){
      #check against one other document
      a <- match(unique(d$doc[[k]]), unique(d$doc[[i]]))
      #if there is a match, advance duplicate counter by one
      b[is.na(a)==F] <- b[is.na(a)==F]+1
    }
    
    return(b)
  }
  
  
  t <- Sys.time()
  docDuplicates <- list()
  docDuplicates <- pbsapply(1:nrow(d), duplicatedLines)
  Sys.time()-t

})

#------------------------------------------------------------------------------

identical(docDuplicates, docDuplicatesH)

#------------------------------------------------------------------------------

a <- d$doc[[9]]
b <- d$doc[[10]]

a = unique(a)
b = unique(b)

match(a, b)

head(a)
head(b)

length(a)
length(b)

H <- hashmap(a, rep(0, length(a)))
H$size()
length(H$values())

H2 <- hashmap(b, rep(0, length(b)))
H2$size()

length(b)
length(H$find(b))

length(a)
length(H2$find(a))

new <- H2$find(a)+1
new[is.na(new)==T] <- 0
new
length(new)

H[[H$keys()]] <- new

H$insert(H$keys(), new)

H$values()

#------------------------------------------------------------------------------

profvis({
  
  duplicatedLinesHash <- function(k){
    
    cityindices <- 1:nrow(d)
    cityindices <- cityindices[-k]
    
    a <- d$doc[[k]]
    a <- unique(a)
    H <- hashmap(keys = a, rep(0, length(a)))
    
    #b <- numeric(length = length(d$doc[[k]]))
    
    for(i in cityindices){
      b <- d$doc[[i]]
      b <- unique(b)
      H2 <- hashmap(b, rep(0, length(b)))
      new <- H2$find(a)+1
      new[is.na(new)==T] <- 0
      new <- H$values() + new
      H$insert(H$keys(), new)
    }
    b <- H$values()
    return(b)
  }
  
  t <- Sys.time()
  docDuplicatesH <- list()
  docDuplicatesH <- pbsapply(1:nrow(d), duplicatedLinesHash)
  Sys.time()-t
  
})

#------------------------------------------------------------------------------

library('readtext')
library('doParallel')

#function to convert every pdf, html, doc, or docx in a folder to text
# and copy to a different folder
# this is basically a wrapper for readtext::readtext
# with two additions: (1) error handling, and (2) parallelization

convertToText <- function(paths, website_id){
  
  #register 11 parallel threads
  registerDoParallel(cores=11)
  #parallel loop over the documents in the current website
  a <- foreach(j=(1:length(paths))) %dopar% {
    
    #error handling, in case readtext can't handle a specific document
    try({
      #readtext
      b <- readtext(paths[j])
      #record the document number
      #b$iter <- j
      #record the filepath
      b$path <- paths[j]
      #record the website id
      #b$website_id <- website_id
      
      return(b)
      
    })

  }
  
  #kick the data frames out of the list that don't have three columns
  broken <- -which(unlist(lapply(a, length))!=3)
  if(length(broken)!=0){
    a <- a[broken]
  }
  #combine the list of data frames from the foreach loop into one big data frame
  a <- do.call(rbind, a)
  
  #save the results to a website-specific rdata file
  save(a, file = paste0("out/city_chunks_unprocessed/", website_id, ".rdata"))
  
}

load("out/citydocs.rdata")
#character vector of State_City
cities <- unique(d$State_City)
#iterate through each city, based on that vector
#and read in and convert to text all of its documents
#using the function above
for (i in 1:length(cities)){
  d_city <- d[d$State_City==cities[i],]
  convertToText(d_city$path, cities[i])
}
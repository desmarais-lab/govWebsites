library("rvest")
library("stringr")

#For some states (i.e. WA), the first row needs to be deleted because it still belongs to the table header
#for other states (i.e. OR), it does not
#df <- df[-1,]

get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

#Most basic function to extract a city website

extractWebsite <- function(pageHTML){
  citylink <- pageHTML %>% html_nodes(".url .text") %>% html_attr("href")
  if(identical(citylink, character(0))){
    citylink <- "" #empty if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}

#Function to find the text of an element in a wikipedia table
#mainly intended to be used to find the names of mayors
#could also be used to find city website URLs BUT ONLY IF the link is plaintext
findTableElement <- function(pageTables, element){
  
  #how many tables are there?
  numTables <- length(pageTables)
  #iterate through all tables on the page
  for(i in 1:numTables){
    #string to store the result in
    tableResult <- ""
    #turn the current table into a dataframe
    try(tableDF <- pageTables[[i]] %>% html_table(fill=T))
    #does the current table contain the element we are looking for?
    test <- any(str_detect(tableDF[,1], element))
    test <- ifelse(is.na(test)==T, F, test)
    #if yes:
    if(test){
      #get the line that contains the element
      tableResultLine <- tableDF[which(str_detect(tableDF[,1], element)),]
      #remove everything else
      tableResult <- str_replace(tableResultLine, element, "")
      tableResult <- str_replace(tableResult, "[^A-Za-z \\.-]", "")
      #the line usually contains multiple elements, find the longest
      tableResult <- tableResult[which.max(nchar(tableResult))]
    }
    #if it actually found something in this table:
    if(tableResult!=""){
      #check whether it already found something in a previous table
      if(exists("result")){
        #if so, concatenate the two into a list
        result <- c(result, tableResult)
        #if not, store the new result
      }else{
        result <- tableResult
      }
    }
  }
  #if no applicable element was found, return an empty string
  if(exists("result")==F){
    result = ""
  }
  return(result)
}


#Same function as above, but explicitly returns a link
#don't use on text that doesnt have a link

findTableLink <- function(pageTables, element){
  
  #how many tables are there?
  numTables <- length(pageTables)
  #iterate through all tables on the page
  for(i in 1:numTables){
    #string to store the result in
    tableResult <- ""
    #turn the current table into a dataframe
    tableDF <- pageTables[[i]] %>% html_table(fill=T)
    #does the current table contain the element we are looking for?
    test <- any(str_detect(tableDF[,1], element))
    test <- ifelse(is.na(test)==T, F, test)
    #if yes:
    if(test){
      #get the line that contains the element
      tableResultLine <- tableDF[which(str_detect(tableDF[,1], element)),]
      #remove everything else
      tableResult <- str_replace(tableResultLine, element, "")
      #not using the following line, since websites can contain some weird stuff
      #might be better to use it anyway though
      #tableResult <- str_replace(tableResult, "[^A-Za-z \\.-]", "")
      #the line usually contains multiple elements, find the longest
      tableResult <- tableResult[which.max(nchar(tableResult))]
      #extract the link
      tableResult <- pageTables[[i]] %>%
        html_nodes(xpath=paste0("//a[text()='", tableResult, "']")) %>% 
        .[[1]] %>% 
        html_attr("href")
    }
    
    #if it actually found something in this table:
    if(tableResult!=""){
      #check whether it already found something in a previous table
      if(exists("result")){
        #if so, concatenate the two into a list
        result <- c(result, tableResult)
        #if not, store the new result
      }else{
        result <- tableResult
      }
    }
  }
  #if no applicable element was found, return an empty string
  if(exists("result")==F){
    result = ""
  }
  return(result)
}
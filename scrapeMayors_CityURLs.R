library("rvest")
library("stringr")

#extract the links to the cities' wikipedia pages
url <- "https://en.wikipedia.org/wiki/List_of_cities_in_Washington"
mytable <- read_html(url) %>% html_nodes("table") %>% .[[2]] 
df <- mytable %>% html_table(fill=T)
df <- df[-1,]

get_link <- function(html_table, team){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", team, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$wiki_link <- sapply(df$Name, function(x)get_link(mytable, x))

#------------------------------------------------------------------------------

df$mayor <- NA

# for(i in 1:nrow(df)){
# link <- str_c("https://en.wikipedia.org", df$wiki_link[7])
# 
# url2 <- link
# mytable2 <- read_html(url2) %>% html_nodes("table") %>% .[[1]] 
# df3 <- mytable2 %>% html_table(fill=T)
# mayorline <- df3[which(str_detect(df3[,1], "Mayor")),]
# #print(mayorline)
# mayor <- str_replace(mayorline, "Mayor", "")
# mayor <- str_replace(mayor, "[^A-Za-z ]", "")
# #pick the string thats the longest
# mayor <- mayor[which.max(nchar(mayor))]
# #print(mayor)
# df$mayor[i] <- mayor
# }

#length(mytable2)

#nrow(df)
# for(j in 1:15){
#   #j=8
#   link <- str_c("https://en.wikipedia.org", df$wiki_link[j])
#   page <- read_html(link)
#   mytable2 <- page %>% html_nodes("table")
#   mayor <- NA
#   for(i in 1:length(mytable2)){
#     mytable3 <- mytable2[[i]]
#     df3 <- mytable3 %>% html_table(fill=T)# %>% .[[1]]
#     test <- any(str_detect(df3[,1], "Mayor"))
#     test <- ifelse(is.na(test)==T, F, test)
#     if(test){
#      mayorline <- df3[which(str_detect(df3[,1], "Mayor")),]
#      mayor <- str_replace(mayorline, "Mayor", "")
#      mayor <- str_replace(mayor, "[^A-Za-z ]", "")
#      mayor <- mayor[which.max(nchar(mayor))]
#      print(mayor)
#    }
#  }
#  df$mayor[j] <- mayor
#
#}

#------------------------------------------------------------------------------

extractWebsite <- function(wikipage){
  link <- str_c("https://en.wikipedia.org", wikipage)
  citylink <- read_html(link) %>% html_nodes(".url .text") %>% html_attr("href")
  if(identical(citylink, character(0))){
    citylink <- NA #NA if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}
df$website <- sapply(df$wiki, extractWebsite)

citylink <- read_html(link) %>% html_nodes(".url .text") %>% html_attr("href")


#------------------------------------------------------------------------------


#page link
link <- str_c("https://en.wikipedia.org/wiki/Bainbridge_Island,_Washington")
#read the page html
page <- read_html(link)
#get all the tables on the page
allTables <- page %>% html_nodes("table")



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

findTableElement(allTables, "Mayor")
findTableElement(allTables, "Website")


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

findTableLink(allTables, "Mayor")
findTableLink(allTables, "Website")

#------------------------------------------------------------------------------

extractWebsite <- function(pageHTML){
  citylink <- pageHTML %>% html_nodes(".url .text") %>% html_attr("href")
  if(identical(citylink, character(0))){
    citylink <- "" #empty if no link
  }
  citylink <- citylink[[1]] #in some cases, it returns the link twice, this takes care of that
  return(citylink)
}

#------------------------------------------------------------------------------

df$mayor <- NA
df$CityWebsite <- NA

for(i in 1:nrow(df)){
  
  #page link
  link <- str_c("https://en.wikipedia.org", df$wiki_link[i])
  #read the page html
  page <- read_html(link)
  #get all the tables on the page
  allTables <- page %>% html_nodes("table")
  
  ## Get the mayor
  df$mayor[i] <- findTableElement(allTables, "Mayor")
  
  ## Get the city website URL
  #First try:
  cityWebsiteURL <- extractWebsite(page)
  #Second try:
  if(cityWebsiteURL==""){
    cityWebsiteURL <- findTableLink(allTables, "Website")
  }
  #Last try:
  if(cityWebsiteURL==""){
    cityWebsiteURL <- findTableElement(allTables, "Website")
  }
  
  df$CityWebsite[i] <- cityWebsiteURL
  
}

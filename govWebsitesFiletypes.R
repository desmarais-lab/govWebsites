library("tools") #for file_ext()
require("xtable")
require('stringr')

#This script extracts the filetypes, folder sizes, etc.
#from the 10 test cases scraped from the Wayback Machine

filteypes <- function(wd){
  
  a <- list.files(wd, recursive = T) #create a list of all files in all subdirectories

  #file types
  b <- file_ext(a) #get file extension
  b <- data.frame(sort(table(b), decreasing = T)) #turn into table/data frame
  names(b) <- c("Filetype", "Frequency") #rename columns
  b <- b[1:25,] #only get top 25
  
  return(b)
  
}

#turn table into latex object


filetypes_current <- filteypes("./websites/current/")
filetypes_before <- filteypes("./websites/before/")
filetypes_after <- filteypes("./websites/after/")

names(filetypes_current)[2] <- "current"
names(filetypes_before)[2] <- "before"
names(filetypes_after)[2] <- "after"

filetypes_all <- merge(filetypes_current, filetypes_before, by = "Filetype")
filetypes_all <- merge(filetypes_all, filetypes_after, by = "Filetype")

filetypes_all <- filetypes_all[order(filetypes_all$current, decreasing = T),]

print(xtable(filetypes_all, caption="The most common file types in scraped websites"), include.rownames = F)

filesfolders <- function(wd){

  #folder size
  #get folder names
  d <- list.files(wd) #actually lists the folders
  e <- vector()
  #print directory size with Linux command 'du -s'
  for(i in 1:length(d)){
    e[i] <- system(paste("du ", wd, d[i]," -s", sep = ""), intern=T)
  }
  #split character vector into dataframe
  require(reshape2)
  f <- colsplit(e,"\\t",c("Size (MB)","Website"))
  
  
  ##number of files
  g <- vector()
  #number of files per website/folder
  for(i in 1:length(d)){
    g[i] <- length(list.files(paste(wd, list.files(wd)[i], sep = ""),recursive = T, all.files=T))
  }
  #add to data frame created above
  f$Files <- g
  #re-order and turn table into latex object

  f$Website <- gsub(wd, "" ,f$Website)
   
  return(f) 
  
}

filesfolders_current <- filesfolders("./websites/current/")
filesfolders_before <- filesfolders("./websites/before/")
filesfolders_after <- filesfolders("./websites/after/")

names(filesfolders_current)[c(1,3)] <- c("current_size","current_files")
names(filesfolders_before)[c(1,3)] <- c("before_size","before_files")
names(filesfolders_after)[c(1,3)] <- c("after_size","after_files")

filesfolders_current$Website[filesfolders_current$Website=="batesvilleindiana.us"] <- "www.batesvilleindiana.us"
filesfolders_current$Website[filesfolders_current$Website=="bloomington.in.gov"] <- "www.bloomington.in.gov"
filesfolders_current$Website[filesfolders_current$Website=="brazil.in.gov"] <- "www.brazil.in.gov"
filesfolders_current$Website[filesfolders_current$Website=="elwoodcity-in.org"] <- "www.elwoodcity-in.org"

filesfolders_all <- merge(filesfolders_current, filesfolders_before, by = "Website")
filesfolders_all <- merge(filesfolders_all, filesfolders_after, by = "Website")

filesfolders_all$size_change <- filesfolders_all$after_size/filesfolders_all$before_size
filesfolders_all$files_change <- filesfolders_all$after_files/filesfolders_all$before_files

load('./data/URLs_IN.rdata')
URLs <- subset(URLs, select = c("foldername", "control_change"))
filesfolders_all2 <- merge(filesfolders_all, URLs, by.x = "Website", by.y = "foldername")

print(xtable(filesfolders_all, caption="Number of files and size of websites"), include.rownames = F)


library('tools')
library('tibble')
library('stringr')
library('dplyr')
library('pbapply')
#library('purrr')

# Function: fixExtensions
# Purpose: given a folder, find the correct extension for each html and pdf file



# get the first non-empty line of a file
firstLine <- function(path){
  
  i <- 0
  first.line <- ""
  while(first.line == ""){
    i <- i+1
    first.line <- readLines(path, n=i)
    # in case a file is completely empty
    if(length(first.line)==0){
      first.line <- ""
      break
    }else{
      first.line <- first.line[length(first.line)]
    }
  }  
  
  return(first.line)
}

#fix file extensions
fixExtensions <- function(path, fixPDF = T, fixHTML = T, fixXML = T, fixEMPTY = T){
  
  f <- list.files(path = filepath, recursive = T) #create a list of all files in all subdirectories
  
  #file types
  ext <- file_ext(f) #get file extension
  folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]
  
  #store objects in tibble
  d <- tibble(path = str_c(filepath, f, sep = "/"), 
              folder = str_c(filepath, folder, sep = "/"),
              filename,
              ext)
  d <- filter(d, filename != "")
  
  #read in the first line of each document
  library('parallel')
  cl <- makeForkCluster(detectCores()-1)
  #d$doc <- pbsapply(d$path, function(x){str_c(readLines(x)[1], sep = " ", collapse = " ")}, cl = cl)
  d$doc <- pbsapply(d$path, firstLine, cl = cl)
  stopCluster(cl)
  d$doc <- as.character(d$doc)

  #delete all files that are not htmls or pdfs without an ending from the data frame
  d$newext <- ""
  d$newext[str_detect(d$doc, regex("DOCTYPE html", ignore_case = T))] <- "html"
  d$newext[str_detect(d$doc, regex("PDF", ignore_case = T))] <- "pdf"
  d$newext[str_detect(d$doc, regex("xml version", ignore_case = T))] <- "xml"
  
  dConflict <- d[d$ext!=d$newext,]
  
  dConflict <- dConflict[dConflict$ext %in% c("html", "pdf", "xml") | 
                           dConflict$newext %in% c("html", "pdf", "xml"),]
  
  #create the new file names, with the new extensions
  dConflict$newpath <- str_c(dConflict$folder, "/", 
                             str_replace(dConflict$filename, paste0(".", dConflict$ext), ""),
                             paste0(".", dConflict$newext))
  #the above step messed up files that are not supposed to have an extension
  dConflict$newpath[dConflict$newext==""] <- str_replace(dConflict$newpath[dConflict$newext==""], "\\.$", "")
  
  if(fixPDF==T){
    purrr::map2(dConflict$path[dConflict$newext=="pdf"], 
                dConflict$newpath[dConflict$newext=="pdf"], file.rename)
    print("Fixed PDFs.")
  }
  if(fixHTML==T){
    purrr::map2(dConflict$path[dConflict$newext=="html"], 
                dConflict$newpath[dConflict$newext=="html"], file.rename)
    print("Fixed HTMLs.")
  }
  if(fixXML==T){
    purrr::map2(dConflict$path[dConflict$newext=="xml"], 
                dConflict$newpath[dConflict$newext=="xml"], file.rename)
    print("Fixed XMLs.")
  }
  if(fixEMPTY==T){
    purrr::map2(dConflict$path[dConflict$newext==""], 
                dConflict$newpath[dConflict$newext==""], file.rename)
    print("Fixed files that are not supposed to have an extension.")
  }
  
}

#fixExtensions("./websites/mayors")

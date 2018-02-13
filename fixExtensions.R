
library('tools')
library('tibble')
library('stringr')
library('dplyr')
library('pbapply')
#library('purrr')

# Function: fixExtensions
# Purpose: given a folder, find the correct extension for each html and pdf file

#some files just can't be fixed so it might be better to just remove them
findInvalidFiles <- function(files){
  invalid_file_names <- files[which(str_detect(files, "[^A-Za-z0-9\\.\\/\\=\\-\\+\\_\\?\\%\\&\\(\\)\\@\\s\\;]"))]
  return(invalid_file_names)
}

# get the first non-empty line of a file
firstLine <- function(path){
  
  i <- 0
  first.line <- ""
  while(first.line == ""){
    i <- i+1
    #some files just can't be read at all
    #break after reading the first 10 lines to make sure it doesn't try forever
    if(i>10){
      break
    }
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
  
  f <- list.files(path, recursive = T) #create a list of all files in all subdirectories
  #ignore files whose names can't be read
  f <- f[!f%in%findInvalidFiles(f)]
  
  #file types
  ext <- file_ext(f) #get file extension
  folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]
  
  #store objects in tibble
  
  path_folder <- str_c(path, folder, sep = "/")
  
  d <- tibble::tibble(path = str_c(path, f, sep = "/"), 
              #WHY DOES THIS NOT WORK ASDPHDIASHFDJKFNDJN
              #folder = (str_c(path, folder, sep = "/")),
              folder = path_folder,
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
  d$newext[str_detect(d$doc, regex("%PDF-", ignore_case = T))] <- "pdf"
  d$newext[str_detect(d$doc, regex("xml version", ignore_case = T))] <- "xml"
  
  dConflict <- d[d$ext!=d$newext,]
  
  dConflict <- dConflict[dConflict$ext %in% c("html", "pdf", "xml") | 
                           dConflict$newext %in% c("html", "pdf", "xml"),]
  
  #create the new file names, with the new extensions
  dConflict$newpath <- str_c(dConflict$folder, "/", 
                             str_replace(dConflict$filename, paste0("\\.", dConflict$ext), ""),
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

fixFileNames <- function(path){
  f <- list.files(path, recursive = T)
  invalid_file_names <- f[which(str_detect(f, "\\["))]
  fixed_file_names <- str_replace_all(invalid_file_names, "[\\[\\]]", "")
  invalid_file_names <- paste0(path, "/", invalid_file_names)
  fixed_file_names <- paste0(path, "/", fixed_file_names)
  purrr::map2(invalid_file_names, 
              fixed_file_names, file.rename)
}



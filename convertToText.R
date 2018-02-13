#function to convert every pdf, html, doc, or docx in a folder to text
# and copy to a different folder

convertToText <- function(path, newfolder = paste0(path, "TXT")){
  
  f <- list.files(path, recursive = T) #create a list of all files in all subdirectories
  
  #file types
  ext <- file_ext(f) #get file extension
  folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]
  
  #store objects in tibble
  d <- data.frame(path = str_c(path, f, sep = "/"), 
              folder = str_c(path, folder, sep = "/"),
              filename,
              ext,
              stringsAsFactors = F)
  d <- subset(d, filename != "")
  
  #new files
  
  #only txt, pdf, html, doc, or docx
  d <- d[d$ext %in% c('txt', 'pdf', 'html', 'doc', 'docx'),]
  
  #create new folder
  d$newfolder <- str_replace(d$folder, path, newfolder)
  pbsapply(d$newfolder, dir.create, showWarnings = F, recursive = T)
  
  #create new filename
  newfilename <- str_replace(d$filename, str_c(d$ext, "$"), "txt")
  newfile <- str_c(d$newfolder, "/", newfilename)
  
  #read in the old file
  a <- readtext::readtext(d$path)
  
  #write out the new text files
  for(i in 1:length(newfile)){
    fileConn <- file(newfile[i])
    writeLines(a$text[i], fileConn)
    close(fileConn)
  }
}

#convertToText("./websites/mayors")

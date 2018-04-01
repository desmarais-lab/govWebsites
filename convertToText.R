library(stringr)
library(tools)
library(pbapply)
library(readtext)

#function to convert every pdf, html, doc, or docx in a folder to text
# and copy to a different folder

convertToText <- function(path, newfolder = paste0(path, "TXT")){
  
  f <- list.files(path, recursive = T) #create a list of all files in all subdirectories
  f <- f[!stringr::str_detect(f, "[^\\x00-\\x7F]")]
  
  #file types
  ext <- file_ext(f) #get file extension
  folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]
  
  #store objects in a data frame
  d <- data.frame(path = str_c(path, f, sep = "/"), 
              folder = str_c(path, folder, sep = "/"),
              filename,
              ext,
              stringsAsFactors = F)
  d <- subset(d, filename != "")
  
  #new files
  
  #only txt, pdf, html, doc, or docx
  d <- d[d$ext %in% c('txt', 'pdf', 'html', 'doc', 'docx'),]
  d$iter <- 1:nrow(d)
  save(d, file = "rfiles/doc_chunks/docnames.rdata")
  
  #create new folder
  #d$newfolder <- str_replace(d$folder, path, newfolder)
  
  # RE-ENABLE THIS LATER
  # !!!
  # OR NOT
  
  #pbsapply(d$newfolder, dir.create, showWarnings = F, recursive = T)
  
  #create new filename
  #newfilename <- str_replace(d$filename, str_c(d$ext, "$"), "txt")
  #newfile <- str_c(d$newfolder, "/", newfilename)
  
  #read in the old file
  chunk_size <- 10000
  for (i in seq(1, nrow(d), chunk_size)) {
    seq_size <- chunk_size
    if ((i + seq_size) > nrow(d)) seq_size <- nrow(d) - i + 1
    
    a <- readtext(d$path[seq(i, (i+seq_size-1))])
    a$iter <- seq(i, (i+seq_size-1))
    save(a, file = paste0("rfiles/doc_chunks/parsedtexts_", i, "_", (i+seq_size-1), ".rdata"))
    
  }
  
  
  #write out the new text files
  #for(i in 1:length(newfile)){
  #  fileConn <- file(newfile[i])
  #  writeLines(a$text[i], fileConn)
  #  close(fileConn)
  #  print(paste("Written file ", i, " out of ", length(newfile), "."))
  #}
}

convertToText("/home/mneumann/hd2/govWebsites/", "/home/mneumann/hd2/govWebsitesTXT/")

library(stringr)
library(tools)
library(pbapply)
library(readtext)
library(doParallel)

#function to convert every pdf, html, doc, or docx in a folder to text
# and copy to a different folder

convertToText <- function(path){
  
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
  
  #d$path <- str_replace(d$path, "\\/\\/", "\\/")
  
  #remove some files that crash readtext
  #everything in this folder causes some problems
  d <- d[-which(stringr::str_detect(d$path, "/home/mneumann/hd2/govWebsites/bloomington.in.gov/trades/parcel/(.*?).pdf")),]
  
  d$path <- str_replace_all(d$path, "\\[", "\\\\[")
  d$path <- str_replace_all(d$path, "\\]", "\\\\]")
  
  #save
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
  #chunk_size <- 10000
  #for (i in seq(100001, nrow(d), chunk_size)) {
  #  seq_size <- chunk_size
  #  if ((i + seq_size) > nrow(d)) seq_size <- nrow(d) - i + 1
  #  #for debugging, record which file we are on
  #  #write(i, file="myfile.txt",append=TRUE)
  #  a <- readtext(d$path[seq(i, (i+seq_size-1))])
  #  a$iter <- seq(i, (i+seq_size-1))
  #  save(a, file = paste0("rfiles/doc_chunks/parsedtexts_", i, "_", (i+seq_size-1), ".rdata"))
  #  
  #}
  
  #non-parallelized version of the code
  
  # chunk_size <- 10000
  # for (i in seq(140001, nrow(d), chunk_size)) {
  #   seq_size <- chunk_size
  #   if ((i + seq_size) > nrow(d)) seq_size <- nrow(d) - i + 1
  #   #for debugging, record which file we are on
  #   #write(i, file="myfile.txt",append=TRUE)
  #   
  #   ab <- data.frame(doc_id = character(length = 0),
  #                    text = character(length = 0),
  #                    iter = numeric(length = 0))
  #   
  #   for(j in i:(i+seq_size-1)){
  #     
  #     try({
  #       a <- readtext(d$path[j])
  #       a$iter <- j
  #       ab <- rbind(ab, a)
  #     })
  #     
  #   }
  #   
  #   save(a, file = paste0("rfiles/doc_chunks/parsedtexts_", i, "_", (i+seq_size-1), ".rdata"))
  #   
  # }
  
  #parallelized version of the code
  
  chunk_size <- 10000
  for (i in seq(1, nrow(d), chunk_size)) {
    seq_size <- chunk_size
    if ((i + seq_size) > nrow(d)) seq_size <- nrow(d) - i + 1
    
    #register 11 parallel threads
    registerDoParallel(cores=11)
    #parallel loop over the documents in the current chunk
    a <- foreach(j=i:(i+seq_size-1)) %dopar% {
      
      #error handling, in case readtext can't handle a specific document
      try({
        #readtext
        a <- readtext(d$path[j])
        #record the document number
        a$iter <- j
        #record the filepath
        a$path <- d$path[j]
      })
      
      #the foreach loop returns a 1x4 data.frame for each text
      return(as.data.frame(a))
      
    }
    
    #kick the data frames out of the list that don't have three columns
    broken <- -which(unlist(lapply(a, length))!=4)
    if(length(broken)!=0){
      a <- a[broken]
    }
    #combine the list of data frames from the foreach loop into one big data frame
    a <- do.call(rbind, a)
    
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

convertToText("/home/mneumann/hd2/govWebsites")

options(java.parameters="-Xmx12g")
set.seed(1)

#path <- "/home/mneumann/hd2/govWebsites"

extractText <- function(path){

  f <- list.files(path, recursive = T)
  #kick out files with non-US-ASCII filenames
  f <- f[!stringr::str_detect(f, "[^\\x00-\\x7F]")]
  
  #store objects in a data frame
  d <- data.frame(path = str_c(path, f, sep = "/"), 
                  folder = str_c(path, folder, sep = "/"),
                  filename,
                  ext,
                  stringsAsFactors = F)
  
  #only txt, pdf, html, doc, or docx
  d <- d[d$ext %in% c('txt', 'pdf', 'html', 'doc', 'docx'),]
  
  #get the filesize
  d$filesize <- file.size(d$path)
  
  #readtext needs square brackets to be escaped
  #some other r functions, like file.info or file.size need the exact opposite
  #so file.size was done before
  d$path <- stringr::str_replace_all(d$path, "\\[", "\\\\[")
  d$path <- stringr::str_replace_all(d$path, "\\]", "\\\\]")
  
  #----------------------------------------------------------------------------------------
  # HTML
  
  d <- d[d$ext=="html",]
  
  #Function to extract the article text with boilerpipeR
  #Can fail if there is something wrong with the HTML, so wrapped in try()
  extractArticle <- function(filepath){
    try({
      content <- paste(readLines(filepath, warn = F), collapse="\n")
      content <- boilerpipeR::ArticleExtractor(content)
      return(content)
    })
  }
  
  #Apply the function
  #No parallelization, seems to work better without
  extracts <- list()
  for(i in 1:nrow(d)){
    extracts[[i]] <- extractArticle(d$path[i])
    #Do some occasional garbage collection to ensure that nothing breaks
    if(i%%1000 == 0) {
      gc()
      print(i)
    }
  }
  #put the results, along with ids, into a file
  extracts <- unlist(extracts)
  ids <- d$id
  results_html <- data.frame(text = extracts, id = ids, stringsAsFactors = F)
  
  #remove documents with embedded javascript, json, other html things, etc. 
  html_docs <- which(stringr::str_detect(results_html$text, "/*! jQuery"))
  html_docs <- c(html_docs, which(stringr::str_detect(results_html$text, ".className")))
  html_docs <- c(html_docs, which(stringr::str_detect(results_html$text, "\\{\\\""))) #json
  html_docs <- c(html_docs, which(stringr::str_detect(results_html$text, "wp-embedded-content")))
  html_docs <- c(html_docs, which(stringr::str_detect(results_html$text, "blockquote")))
  html_docs <- unique(html_docs)
  if(length(html_docs)>0){
    results_html <- results_html[-html_docs,]
  }
  rm(html_docs)
  
  #----------------------------------------------------------------------------------------
  # Other Filetypes
  
  convertToText <- function(paths, id){
    
    #register 11 parallel threads
    doParallel::registerDoParallel(cores=11)
    #parallel loop over the documents in the current website
    a <- foreach::foreach(j=(1:length(paths))) %dopar% {
      
      #error handling, in case readtext can't handle a specific document
      try({
        #readtext
        b <- readtext::readtext(paths[j])
        #record the file id
        b$id <- id[j]
        
        return(b)
        
      })
      
    }
    
    #kick the data frames out of the list that don't have three columns
    broken <- -which(unlist(lapply(a, length))!=3)
    if(length(broken)!=0){
      a <- a[broken]
    }
    #combine the list of data frames from the foreach loop into one big data frame
    a <- data.table::rbindlist(a)
    a <- subset(a, select = -doc_id)
    
    return(a)
    
  }
  
  d <- d[d$ext!="html",]
  
  #Flag files that are too large (>10Mb)
  too_big <- d$filesize>1e+7
  #print(sum(d$filesize[which(too_big)])) #combined size of the file we kick out
  #print(sum(d$filesize[which(!too_big)])) #combined size of the file we keep
  #remove them
  d <- d[!too_big,]
  
  #sort by size
  #the advantage of this should be that we don't have 10 cores sitting idle while one works on a huge file
  d <- d[order(d$filesize),]
  
  #remove robots.txt files
  d <- d[d$filename!="robots.txt",]
  
  #use ff to make 5k row chunks of the dataframe
  #iterate over them and read in and convert to text all of its documents
  #using the function above
  #the downside to this approach is that it often seems to get stuck on one thing while all the other cores are already done
  for(i in ff:chunk(from = 1, to = nrow(d), by = 5000)){
    d_chunk <- d[min(i):max(i), ]
    text_chunk <- convertToText(d_chunk$path, d_chunk$id)
    save(text_chunk, file = paste0("out_non_html_chunk_", min(i), "_", max(i), ".rdata"))
  }
  
  #combine the chunks into one file
  f <- list.files("out_non_html_chunk_*", full.names = T)
  files <- list()
  for(i in 1:length(f)){
    load(f[i])
    files[[i]] <- text_chunk
  }
  results_nonhtml <- data.table::rbindlist(files)
  file.remove(f)
  
  #----------------------------------------------------------------------------------------
  # Merge
  
  #combine
  results_extracted <- rbind(results_html, results_nonhtml)
  rm(results_html, results_nonhtml)
  
  #remove empties
  results_extracted <- results_extracted[results_extracted$text!="",]
  
  #sort by id
  results_extracted <- results_extracted[order(as.numeric(stringr::str_remove(results_extracted$id, "file"))),]
  
  return(results_extracted)

}
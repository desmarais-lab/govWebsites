#purpose of the script: a lot of the files in nola are pdfs, but have an html file ending by default
#this script renames them

library('tools')
library('tibble')
library('stringr')
library('dplyr')
library('pbapply')

state <- "LA" #IN or LA

filepath <- str_c("./websites/scraping/LA_backup")

## Reading in the data
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
d$doc <- pbsapply(d$path, function(x){str_c(readLines(x)[1], sep = " ", collapse = " ")}, cl = cl)
stopCluster(cl)
d$doc <- as.character(d$doc)

#delete all files that are not htmls or pdfs without an ending from the data frame
d$newext <- NA
d$newext[str_detect(d$doc, regex("DOCTYPE html", ignore_case = T))] <- "html"
d$newext[str_detect(d$doc, regex("PDF", ignore_case = T))] <- "pdf"

dConflict <- d[d$ext=="html" & d$newext!="html",]
dConflict$newpath <- str_c(dConflict$folder, "/", str_replace(dConflict$filename, paste0(".",dConflict$ext), "."), dConflict$newext)

#rename
library('purrr')
map2(dConflict$path, dConflict$newpath, file.rename)

library('readtext')
readtext(dConflict$newpath[2])$text

newdocs <- lapply(dConflict$newpath, readtext)
newdocs <- newdocs$text

newdocs2 <- list()
for(j in 1:length(newdocs)){
  newdocs2[[j]] <- newdocs[[j]]$text
}

newdocs3 <- unlist(newdocs2)

newtxts <- str_c(str_replace(dConflict$folder, "LA_backup", "LA/websites"), "/", str_replace(dConflict$filename, paste0(".",dConflict$ext), "."), "txt")

#write the new files
for(i in 1:length(newtxts)){
  fileConn <- file(newtxts[i])
  writeLines(newdocs3[i], fileConn)
  close(fileConn)
}

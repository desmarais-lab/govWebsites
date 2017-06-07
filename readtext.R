library('readtext')
library('tools')
library('tibble')
library('stringr')

#setwd("~/Dropbox/4_RA/govWebsites")
setwd("./websites/websites")

#rename html files to always have an html at the end
#BUT ONLY IF we are actually somewhere in the govWebsites directory
if(grepl("govWebsites", getwd())==T){
  system("find ./ -name '*.html*' -type f -exec mv '{}' '{}'.html \\;")
}

f <- list.files(recursive = T) #create a list of all files in all subdirectories

#file types
ext <- file_ext(f) #get file extension
folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]

d <- tibble(f, folder, filename, ext)
d <- d[d$ext%in%c("doc","docx","pdf","html",""),]

#d$filename[d$ext==""] <- str_c(d$filename[d$ext==""], ".html")
#d$ext[d$ext==""] <- "html"

for(i in 1:nrow(d)){

  newfilename <- str_replace(d$filename[i], str_c(d$ext[i], "$"), "txt")
  newfile <- str_c(d$folder[i], "/", newfilename)
  
  a <- readtext(d$f[i])

  #write.table(a$text, file="text.txt", sep="\t",row.names=FALSE)
  fileConn <- file(newfile)
  writeLines(a$text, fileConn)
  close(fileConn)

}

#delete everything that is not a text file
if(grepl("govWebsites", getwd())==T){
  system("find . -type f ! -name '*.txt' -delete")
}
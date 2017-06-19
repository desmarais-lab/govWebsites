library('readtext')
library('tools')
library('tibble')
library('stringr')

#setwd("~/Dropbox/4_RA/govWebsites")
setwd("./websites/websites")


f <- list.files(recursive = T) #create a list of all files in all subdirectories

#file types
ext <- file_ext(f) #get file extension
folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]

d <- tibble(f, folder, filename, ext)
d <- d[d$ext%in%c("doc","docx","pdf","html",""),]

#determine the size of the file (in bytes)
d$filesize <- file.size(f)

#determine the number of lines in the file
#length(readLines()) is supposed to be the slowest, but for some reason
#in this case, it is the fastest of the three approaches I tried

#profvis::profvis({

d$numlines <- sapply(f, function(x){length(readLines(x))})
#a <- sapply(f, R.utils::countLines)

#a <- sapply(f, function(x){
#  as.integer(system2("wc",
#                     args = c("-l",
#                              x,
#                              " | awk '{print $1}'"),
#                     stdout = TRUE))
#}
#)


#})

d$fileratio <- d$filesize/d$numlines

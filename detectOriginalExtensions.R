library('tibble')
library('stringr')
library('tools')
library('dplyr')

filepath2 <- "./websites_original/current"

## Reading in the data

f <- list.files(path = filepath2, recursive = T) #create a list of all files in all subdirectories

#file types
ext <- file_ext(f) #get file extension
folder <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(f, "\\/(?=[^\\/]+$)", simplify = T)[,2]

#store objects in tibble
d2 <- tibble(path = str_c("./websites/current", f, sep = "/"), 
            folder = str_c("./websites/current", folder, sep = "/"),
            filename,
            ext)

d2$path2 <- d2$path

d2$path2 <- str_replace(d2$path2, ".html", ".txt")
d2$path2 <- str_replace(d2$path2, ".pdf", ".txt")
d2$path2 <- str_replace(d2$path2, ".doc", ".txt")
d2$path2 <- str_replace(d2$path2, ".dox", ".txt")

d2 <- d2[d2$ext%in%c("html","pdf","doc","docx"),]

d2 <- select(d2, c("path","path2","ext"))

names(d2) <- c("originalpath", "path","extension")

#save(d2, file = "rfiles/originalpaths.Rdata")

d3 <- merge(d, d2, by = "path", all.x = T)

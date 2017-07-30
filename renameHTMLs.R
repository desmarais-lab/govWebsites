#find and rename html files without a file ending

#library('readtext')
library('tools')
library('tibble')
library('stringr')
library('dplyr')

filepath <- "./websites2/websites"

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

#we only care about files without ending
d <- filter(d, ext == "")
d$doc <- sapply(d$path, function(x){str_c(readLines(x)[1], sep = " ", collapse = " ")})
d$doc <- as.character(d$doc)

#delete all files that are not htmls without an ending
d$ext[d$doc=="<!DOCTYPE html>"] <- ".html"
d <- filter(d, ext == ".html")
d$newpath <- str_c(d$folder, d$filename, d$ext)

#rename
library('purrr')
map2(d$path, d$newpath, file.rename)

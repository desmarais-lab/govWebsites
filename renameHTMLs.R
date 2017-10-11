#find and rename html files without a file ending

#library('readtext')
library('tools')
library('tibble')
library('stringr')
library('dplyr')
library('pbapply')

state <- "LA" #IN or LA

filepath <- str_c("./websites/scraping/", state, "/websites")

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
library('parallel')
cl <- makeForkCluster(detectCores()-1)
d$doc <- pbsapply(d$path, function(x){str_c(readLines(x)[1], sep = " ", collapse = " ")}, cl = cl)
stopCluster(cl)
d$doc <- as.character(d$doc)

#delete all files that are not htmls or pdfs without an ending from the data frame
d$ext[str_detect(d$doc, regex("DOCTYPE html", ignore_case = T))] <- ".html"
d$ext[str_detect(d$doc, regex("PDF", ignore_case = T))] <- ".pdf"
d <- filter(d, ext != "")
d$newpath <- str_c(d$folder, "/", d$filename, d$ext)

#rename
library('purrr')
map2(d$path, d$newpath, file.rename)

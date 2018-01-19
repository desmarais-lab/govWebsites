#library('readr') #write_lines function

# URLs is a character vector of the URLs
# sites.dir is the name of the directory the files should be stored in,
# starting in the govWebsites folder

wget <- function(URLs, sites.dir){

  #create folder to download websites in
  #recursive option enables intermediate directories to be created
  dir.create(sites.dir, recursive = T)
  
  #write URLs to a text file
  readr::write_lines(x = URLs, path = paste0(sites.dir, "/URLs.txt"))
  
  #set new wd
  setwd(sites.dir)
  
  #scrape
  system("wget -r -N -P ./ -i ./URLs.txt")
  
  #clean up the text file created above
  file.remove("URLs.txt")

}

# Example: mayors

#load URLs
#load("rfiles/mayors.rdata")

#use wget function created above
#wget(mayors$website, "./websites/mayors")


#testing
#test.url <- "https://betticeforbatesville.com"

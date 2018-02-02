#library('readr') #write_lines function

# URLs is a character vector of the URLs
# sites.dir is the name of the directory the files should be stored in,
# starting in the govWebsites folder

wget <- function(URLs, sites.dir){

  #create folder to download websites in
  #recursive option enables intermediate directories to be created
  dir.create(sites.dir, recursive = T)
  
  #write URLs to a text file
  readr::write_lines(x = URLs, path = paste0(sites.dir, "/urls.txt"))
  
  #set new wd
  setwd(sites.dir)
  
  #scrape
  system("<urls.txt xargs -n 1 -P 8 -I % wget -r -N -P ./ %")
  
  #<urls.txt passes the file to xargs
  #xargs -n 1 -P 8 starts 8 parallel processes
  #-I % takes the input file and puts it into a variable %
  #wget downloads websites
  #-r recursive
  #-N if re-doing the download, only get files if they are newer
  #-P put everything into its own folders, in the current directory
  # % takes the input file from above
  
  ###clean up the text file created above
  ###file.remove("urls.txt")

}

# Example: mayors

#load URLs
#load("rfiles/mayors.rdata")

#use wget function created above
#wget(mayors$website, "./websites/mayors")


#testing
#test.url <- "https://betticeforbatesville.com"

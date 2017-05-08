options(stringsAsFactors = FALSE)
require(rvest)

scrapeHTML <- function(url){
  
  ## change Phantom.js scrape file
  #url <- "https://web.archive.org/web/20150101000000*/https://attica-in.gov/"
  lines <- readLines("scrape_final.js")
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, "scrape_final.js")
  
  ## Download website
  system("phantomjs scrape_final.js")
  
  ### use Rvest to scrape the downloaded website.
  website <- read_html("1.html")
  return(website)
}

scrapeSnapshot <- function(website){
  
  #extract snapshot dates
  text <- html_nodes(website, '.captures') %>% 
    str_extract("href=\"(.*?)\"") %>%
    str_extract("([0-9]+)")
  
  return(text)
}
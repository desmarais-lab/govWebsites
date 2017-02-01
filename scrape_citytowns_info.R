regexpr("\\.", "wtf.")
####
#####
library(rvest)

sitelist <- paste("http://citytown.info/",state.name,".htm", sep="")
sitelist <- sub(" ","-",sitelist)

#loop through all states
for (i in 1:length(sitelist)){
  url <- sitelist[i]
  
  
}

##
url='http://citytown.info/Alabama.htm'
text1 <- read_html(url)
text2 <- html_nodes(text1, 'li')
text3 <- html_text(text2)
text3

##
text4 <- read_html(url)
text5 <- html_nodes(text4, 'a')
text6 <- html_text(text5)
text6

##
text7 <- read_html(url)
text8 <- html_nodes(text7, 'a')
text9 <-html_attrs(text8)
text9# <- unlist(text9)
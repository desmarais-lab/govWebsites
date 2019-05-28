library(stringr)
library(pbapply)
set.seed(1)

load("out/citydocs.rdata")
d <- d[d$ext=="html",]
exists <- file.exists(d$path)
d <- d[-which(exists==F),]

#Count the number of times the <script> tag appears in an html file
countScripts <- function(html_path){
  try({
  text <- readLines(html_path, warn = F)
  return(length(which(str_detect(text, "<script>"))))
  })
}

#try it out with a sample
# sampledDocs <- sample(1:nrow(d), 10000)
# nScripts <- pblapply(d$path[sampledDocs], countScripts)
# nScripts <- unlist(nScripts)
# averageScripts_by_citypage <- aggregate(nScripts, by = list(d$State_City[sampledDocs]), FUN = mean)
# pop_by_city <- aggregate(d$B01001_001E[sampledDocs], by = list(d$State_City[sampledDocs]), FUN = mean)
# inc_by_city <- aggregate(d$B19013_001E[sampledDocs], by = list(d$State_City[sampledDocs]), FUN = mean)
# names(averageScripts_by_citypage)[names(averageScripts_by_citypage)=="x"] <- "scripts"
# averageScripts_by_citypage$pop <- pop_by_city$x
# averageScripts_by_citypage$inc <- inc_by_city$x
# averageScripts_by_citypage$pop <- averageScripts_by_citypage$pop/10000
# averageScripts_by_citypage$inc <- averageScripts_by_citypage$inc/10000
# 
# cor(averageScripts_by_citypage$scripts, averageScripts_by_citypage$pop)
# cor(averageScripts_by_citypage$scripts, averageScripts_by_citypage$inc)
# 
# summary(lm(scripts ~ pop + inc, data = averageScripts_by_citypage))


#application of the function
#faster without parallelization (probably because hard drive speed matters?)
nScripts <- pblapply(d$path, countScripts)

averageScripts_by_citypage <- aggregate(nScripts, by = list(d$State_City), FUN = mean)

pop_by_city <- aggregate(d$B01001_001E, by = list(d$State_City), FUN = mean)
inc_by_city <- aggregate(d$B19013_001E, by = list(d$State_City), FUN = mean)
names(averageScripts_by_citypage)[names(averageScripts_by_citypage)=="x"] <- "scripts"
averageScripts_by_citypage$pop <- pop_by_city$x
averageScripts_by_citypage$inc <- inc_by_city$x
averageScripts_by_citypage$pop <- averageScripts_by_citypage$pop/10000
averageScripts_by_citypage$inc <- averageScripts_by_citypage$inc/10000

cor(averageScripts_by_citypage$scripts, averageScripts_by_citypage$pop)
cor(averageScripts_by_citypage$scripts, averageScripts_by_citypage$inc)

summary(lm(scripts ~ pop + inc, data = averageScripts_by_citypage))

save.image("out/countScriptTags.rdata")

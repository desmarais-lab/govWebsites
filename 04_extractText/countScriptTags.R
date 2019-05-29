## estimate the impact of <script> tags
## result: effectively irrelevant
## script requires preprocessing to be done

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

#application of the function
#faster without parallelization (probably because hard drive speed matters?)
nScripts <- pblapply(d$path, countScripts)
save.image("out/countScriptTags.rdata")
nScripts <- unlist(nScripts)

#average number of <script> tags per city html file
averageScripts_by_citypage <- aggregate(nScripts, by = list(d$State_City), FUN = mean)
#get city pop/income
pop_by_city <- aggregate(d$B01001_001E, by = list(d$State_City), FUN = mean)
inc_by_city <- aggregate(d$B19013_001E, by = list(d$State_City), FUN = mean)
#put it all in one dataframe
names(averageScripts_by_citypage)[names(averageScripts_by_citypage)=="x"] <- "scripts"
averageScripts_by_citypage$pop <- pop_by_city$x
averageScripts_by_citypage$inc <- inc_by_city$x
averageScripts_by_citypage$pop <- averageScripts_by_citypage$pop/10000
averageScripts_by_citypage$inc <- averageScripts_by_citypage$inc/10000

#correlation between average # of <script> tags and pop/inc
cor(averageScripts_by_citypage$scripts, averageScripts_by_citypage$pop)
cor(averageScripts_by_citypage$scripts, averageScripts_by_citypage$inc)
#ols
summary(lm(scripts ~ pop + inc, data = averageScripts_by_citypage))

#merge in token counts
dd <- data.table::data.table(id = d$id, nScripts)
load("../06_preprocessing/out/preprocessed.rdata")
library(quanteda)
which.tks <- match(dd$id, docvars(tks)$id)

token_counts <- unlist(lapply(tks[which.tks[is.na(which.tks)==F]], length))
ddf <- data.table::data.table(token_counts, names(token_counts))

dd3 <- merge(dd, ddf, by.x = "id", by.y = "V2", all.x = T)
dd3$token_counts[is.na(dd3$token_counts)] <- 0

cor(dd3$nScripts, dd3$token_counts)
cor(dd3$nScripts[dd3$token_counts!=0], dd3$token_counts[dd3$token_counts!=0])







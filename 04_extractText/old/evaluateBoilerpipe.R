library(stringr)
set.seed(123)

load("out/boilerpipe_img.rdata")

extracts2 <- unlist(extracts)

problems_jquery <- which(str_detect(extracts, "/*! jQuery"))
substr(extracts2[problems_jquery], 1, 50)

problems_html1 <- which(str_detect(extracts, ".className"))
substr(extracts2[problems_html1], 1, 50)

problems <- unique(c(problems_jquery, problems_html1))
d$successfully_parsed <- T
d$successfully_parsed[problems] <- F

test <-  sample(d$path[d$successfully_parsed==T], 50)
test_str <- extracts2[match(test, d$path)]

file.copy(test, paste0("exampleHTMLs_boilerpipe/file", 1:length(test), ".html"))
for(i in 1:length(test)){
  writeLines(test_str[i], paste0("exampleHTMLs_boilerpipe/file", i, ".txt"))
}

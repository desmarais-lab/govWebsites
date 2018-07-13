# Purpose of the script:
# make the table in the paper that shows the distribution of file types

library(xtable)

#re-use the script to detect filetypes
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}
source_lines(inferFiletype, c(21,66))

#create tables of filetype frequencies
filetypes_after <- data.frame(sort(table(magicResults$extensions), decreasing = T), stringsAsFactors = F)
filetypes_before <- data.frame(sort(table(magicResults$ext), decreasing = T), stringsAsFactors = F)
names(filetypes_after) <- c("Filetype", "Occurances")
names(filetypes_before) <- c("Filetype", "Occurances")
filetypes_after$Filetype <- as.character(filetypes_after$Filetype)
filetypes_before$Filetype <- as.character(filetypes_before$Filetype)

#only show the top 10 types, everything else into an 'other' category
#get the top 10
top10types <- c(filetypes_after$Filetype[1:10], "")

#put everything else into the other category, then delete everything else
filetypes_after <- rbind(filetypes_after, c("Other", sum(filetypes_after$Occurances[!filetypes_after$Filetype%in%top10types])))
filetypes_after <- filetypes_after[-which(!filetypes_after$Filetype%in%c(top10types, "Other")),]
filetypes_after$Filetype[filetypes_after$Filetype=="NA"] <- NA

#same for the before filetypes
filetypes_before <- rbind(filetypes_before, c("Other", sum(filetypes_before$Occurances[!filetypes_before$Filetype%in%top10types])))
filetypes_before <- filetypes_before[-which(!filetypes_before$Filetype%in%c(top10types, "Other")),]
filetypes_before$Filetype[filetypes_before$Filetype==""] <- NA

#merge the two to go into one table
filetypes_before_after <- merge(filetypes_before, filetypes_after, by = "Filetype", all = T)
filetypes_before_after$Occurances.x[is.na(filetypes_before_after$Occurances.x)==T] <- 0

filetypes_before_after$Occurances.x <- as.numeric(as.character(filetypes_before_after$Occurances.x))
filetypes_before_after$Occurances.y <- as.numeric(as.character(filetypes_before_after$Occurances.y))
filetypes_before_after <- filetypes_before_after[order(filetypes_before_after$Occurances.y, decreasing = T),]

filetypes_before_after <- rbind(filetypes_before_after, c("TOTAL", sum(filetypes_before_after$Occurances.x), sum(filetypes_before_after$Occurances.y)))

#rename columns
names(filetypes_before_after) <- c("Filetype", "Occurances After", "Occurances After")

###

xtTopwords <- print(xtable(filetypes_before_after, 
                           caption = "Number of files per type, before and after detecing them via their magic number. The table shows that a lot of files originally have the wrong type, and that converting them correctly has a large impact on how many of them end up being usable."), 
                    sanitize.text.function = identity,
                    label = "tabFiletypeFrequencies",
                    #size = "scriptsize",
                    include.rownames = FALSE)

writeLines(xtTopwords, con = 'paper/tables/tabFiletypeFrequencies.tex')

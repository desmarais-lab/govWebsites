# Purpose of the script:
# make the table in the paper that shows the distribution of file types

library(xtable)

load("../03_correctFiletypeErrors/out/trueFileTypesOriginal.rdata")

#----
# Re-use the part of the filetype correction script
#get the file extensions as they currently are

magicResults$ext <- tools::file_ext(magicResults$file)
magicResults$extensions <- as.character(magicResults$extensions)

magicResults$extensions <- stringr::str_replace_all(magicResults$extensions, "\"", "")
magicResults$extensions <- stringr::str_replace_all(magicResults$extensions, "c\\(", "")
magicResults$extensions <- stringr::str_replace_all(magicResults$extensions, "\\)", "")

magicResults$extensions[magicResults$extensions=="ai, eps, ps"] <- "ps"
magicResults$extensions[magicResults$extensions=="asf, asx"] <- "asf"
magicResults$extensions[magicResults$extensions=="asm, s"] <- "asm"
magicResults$extensions[magicResults$extensions=="bin, bpk, buffer, deb, deploy, dist, distz, dll, dmg, dms, dump, elc, exe, img, iso, lrf, mar, msi, msm, msp, pkg, so"] <- "bin"
magicResults$extensions[magicResults$extensions=="conf, def, in, ini, list, log, text, txt"] <- "txt"
magicResults$extensions[magicResults$extensions=="doc, dot"] <- "doc"
magicResults$extensions[magicResults$extensions=="eml, mime"] <- "eml"
magicResults$extensions[magicResults$extensions=="f, f77, f90, for"] <- "f"
magicResults$extensions[magicResults$extensions=="htm, html, shtml"] <- "html"
magicResults$extensions[magicResults$extensions=="ics, ifb"] <- "ics"
magicResults$extensions[magicResults$extensions=="jpe, jpeg, jpg"] <- "jpg"
magicResults$extensions[magicResults$extensions=="m1v, m2v, mpe, mpeg, mpg"] <- "mpeg"
magicResults$extensions[magicResults$extensions=="m2a, m3a, mp2, mp2a, mp3, mpga"] <- "mp3"
magicResults$extensions[magicResults$extensions=="mov, qt"] <- "mov"
magicResults$extensions[magicResults$extensions=="mp4, mp4v, mpg4"] <- "mp4"
magicResults$extensions[magicResults$extensions=="pot, pps, ppt"] <- "ppt"
magicResults$extensions[magicResults$extensions=="rng, xml, xsd, xsl"] <- "xml"
magicResults$extensions[magicResults$extensions=="svg, svgz"] <- "svg"
magicResults$extensions[magicResults$extensions=="tif, tiff"] <- "tif"
magicResults$extensions[magicResults$extensions=="ttc, ttf"] <- "ttf"
magicResults$extensions[magicResults$extensions=="xla, xlc, xlm, xls, xlt, xlw"] <- "xls"

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
names(filetypes_before_after) <- c("Filetype", "Occurances Before", "Occurances After")

###

xtTopwords <- print(xtable(filetypes_before_after, 
                           caption = "Number of files per type, before and after detecing them via their magic number. The table shows that a lot of files originally have the wrong type, and that converting them correctly has a large impact on how many of them end up being usable."), 
                    sanitize.text.function = identity,
                    label = "tabFiletypeFrequencies",
                    #size = "scriptsize",
                    include.rownames = FALSE)

writeLines(xtTopwords, con = 'paper/tables/tabFiletypeFrequencies.tex')

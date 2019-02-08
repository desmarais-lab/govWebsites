#devtools::install_github("hrbrmstr/wand")

library(wand)

path = "/home/mneumann/hd2/govWebsites/"
f <- list.files(path, recursive = T) #create a list of all files in all subdirectories
#ignore files whose names can't be read
f <- f[!stringr::str_detect(f, "[^\\x00-\\x7F]")]
#absolute file paths
fAbs <- paste(path, f, sep = "")

#determine file type with the wand pagacke (relying on libmagic)
#and check how long it takes
start.time <- Sys.time()
magicResults <- incant(fAbs)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
#~9 hours

save(magicResults, file = "out/trueFileTypes.rdata")
load("out/trueFileTypes.rdata")

library(tools)

magicResults$ext <- file_ext(magicResults$file)
magicResults$extensions <- as.character(magicResults$extensions)
#magicResults$inferredExt[magicResults$extensions=="pdf"] <- "pdf"
#magicResults$inferredExt[magicResults$extensions=="docx"] <- "docx"
#magicResults$inferredExt[magicResults$extensions=="c(\\\"ttc\\\", \\\"ttf\\\")"] <- "ttf"

#magicresultsDoc <- magicResults[magicResults$extensions=="c(\\\"doc\\\", \\\"dot\\\")",]

library(stringr)
#magicResults$mime <- str_replace(magicResults$mime_type, "^(.+?)/", "")

#changes that need to be made:
#files that currently have a pdf/txt/html/doc/docx extension and shouldn't have one
#files that currently have a no extension and should have a pdf/txt/html/doc/docx
#files that have the wrong pdf/txt/html/doc/docx extension

magicResults$extensions <- str_replace_all(magicResults$extensions, "\"", "")
magicResults$extensions <- str_replace_all(magicResults$extensions, "c\\(", "")
magicResults$extensions <- str_replace_all(magicResults$extensions, "\\)", "")


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

magicResults$conflict <- F
#one exception: we don't change anything to text files, because that would result in the file not getting parsed
magicResults$conflict[magicResults$ext!=magicResults$extensions & magicResults$extensions!="txt"] <- T
#magicResults$conflict

#file types
folder <- str_split(magicResults$file, "\\/(?=[^\\/]+$)", simplify = T)[,1]
filename <- str_split(magicResults$file, "\\/(?=[^\\/]+$)", simplify = T)[,2]

#create the new file names, with the new extensions
magicResults$newpath <- str_c(folder, "/", 
                           str_replace(filename, paste0("\\.", magicResults$ext), ""),
                           paste0(".", magicResults$extensions))
#the above step messed up files that are not supposed to have an extension
#dConflict$newpath[dConflict$newext==""] <- str_replace(dConflict$newpath[dConflict$newext==""], "\\.$", "")

#exclude files whose names are too long
magicResults$nChar <- nchar(magicResults$file)
magicResults <- magicResults[magicResults$nChar<500,]

#when re-running this script, uncomment the following two lines 
#to only rename files that haven't already been renamed
#magicResults$file_exists_new <- file.exists(magicResults$newpath)
#magicResults <- magicResults[magicResults$file_exists_new==F,]

purrr::map2(magicResults$file[magicResults$conflict==T], 
            magicResults$newpath[magicResults$conflict==T], file.rename)

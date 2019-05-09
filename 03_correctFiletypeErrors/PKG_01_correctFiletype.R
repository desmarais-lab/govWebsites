
correctFiletypes <- function(path){

  #create a list of all files in all subdirectories
  #f <- list.files(path, recursive = T)
  #ignore files whose names can't be read
  #f <- f[!stringr::str_detect(f, "[^\\x00-\\x7F]")]
  #absolute file paths
  fAbs <- list.files(path, recursive = T, full.names = T)
  fAbs <- fAbs[!stringr::str_detect(fAbs, "[^\\x00-\\x7F]")]
  
  #determine file type with the wand package (relying on libmagic)
  magicResults <- wand::incant(fAbs)

  #get the file extensions as they currently are
  magicResults$ext <- tools::file_ext(magicResults$file)
  magicResults$extensions <- as.character(magicResults$extensions)

  #changes that need to be made:
  #files that currently have a pdf/txt/html/doc/docx extension and shouldn't have one
  #files that currently have a no extension and should have a pdf/txt/html/doc/docx
  #files that have the wrong pdf/txt/html/doc/docx extension
  
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
  
  #one exception: we don't change files to txt, because it would want to change js and css to text, which would be wrong
  magicResults$conflict <- F
  magicResults$conflict[magicResults$ext!=magicResults$extensions & magicResults$extensions!="txt"] <- T
  
  #file types
  folder <- stringr::str_split(magicResults$file, "\\/(?=[^\\/]+$)", simplify = T)[,1]
  filename <- stringr::str_split(magicResults$file, "\\/(?=[^\\/]+$)", simplify = T)[,2]
  
  #create the new file names, with the new extensions
  magicResults$newpath <- stringr::str_c(folder, "/",
                                         stringr::str_replace(filename, paste0("\\.", magicResults$ext), ""),
                                         paste0(".", magicResults$extensions))

  #exclude files whose names are too long
  magicResults$nChar <- nchar(magicResults$file)
  magicResults <- magicResults[magicResults$nChar<500,]
  
  #Print info
  print("Making the following changes:")
  print(paste(magicResults$file[magicResults$conflict==T], "  ----->  ", magicResults$extensions[magicResults$conflict==T]))
  
  #rename everything
  purrr::map2(magicResults$file[magicResults$conflict==T], 
              magicResults$newpath[magicResults$conflict==T], file.rename)

}

path = "/media/mneumann/ec574740-a4f4-4bd0-b624-6ff2c4ac59e9/testGovWebsitesPackage/cityofboonvilleindiana.com"
correctFiletypes(path)

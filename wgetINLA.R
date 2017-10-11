library('readr') #write_lines function

###-----------------------------------------------------------------------------------------------------
### INDIANA

#load IN URLs
load("data/URLs_IN.rdata")

#create folder to download websites in
#recursive option enables intermediate directories to be created
dir.create("./websites/scraping/IN/websites", recursive = T)

#write URLs to a text file
write_lines(x = URLs$Website, path = "websites/scraping/IN/websites/URLs.txt")

#set new wd
setwd("websites/scraping/IN/websites")

#scrape
system("wget -r -N -P ./ -i ./URLs.txt")

#wget
#explanation of options:
#-r,  --recursive                 specify recursive download
#-E,  --adjust-extension          save HTML/CSS documents with proper extensions
#-H,  --span-hosts                go to foreign hosts when recursive
#-k,  --convert-links             make links in downloaded HTML or CSS point to local files
#-K,  --backup-converted          before converting file X, back up as X.orig
#-p,  --page-requisites           get all images, etc. needed to display HTML page
#-e,  --execute=COMMAND           execute a `.wgetrc'-style command
#robots=off,  turn off the robot exclusion; somewhat risky because it can cause a lot of traffic
#-P,  --directory-prefix=PREFIX   save files to PREFIX/..
#-i,  --input-file=FILE           download URLs found in local or external FILE
#-nc, --no-clobber                skip downloads that would download to existing files (overwriting them)
#-N,  --timestamping              don't re-retrieve files unless newer than local

#system("wget -E -H -k -K -p -e robots=off -P ./Downloads/ -i ./URLs.txt")
#system("wget -E -H -k -K -p -r -P ./Downloads2/ -i ./URLs.txt")
#system("wget -p -P ./Downloads22/ -i ./URLs.txt")

#run these in separate terminal windows
#r doesn't parllelize (too lazy to write up with parallel package, this is way simpler)
#system("wget -r -P ./DownloadsP1/ -i ./URLs1.txt")
#system("wget -r -P ./DownloadsP2/ -i ./URLs2.txt")
#system("wget -r -P ./DownloadsP3/ -i ./URLs3.txt")
#system("wget -r -P ./DownloadsP4/ -i ./URLs4.txt")


###-----------------------------------------------------------------------------------------------------
### LOUISIANA

#go back to govWebsites folder
setwd("../../..")

#load LA URLs
load("data/louisiana.rdata")

#create folder to download websites in
#recursive option enables intermediate directories to be created
dir.create("./websites/scraping/LA/websites", recursive = T)

#write URLs to a text file
write_lines(x = louisiana$Website, path = "websites/scraping/LA/websites/URLs.txt")

#set new wd
setwd("websites/scraping/LA/websites/")

#scrape
system("wget -r -N -P ./ -i ./URLs.txt")

#FINISHED --2017-10-07 13:07:05--
#Total wall clock time: 11h 31m 20s
#Downloaded: 37452 files, 25G in 5h 1m 21s (1.41 MB/s)
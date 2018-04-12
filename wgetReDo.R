load("rfiles/citydocs_todo.rdata")

sites.dir <- "/home/mneumann/hd2/cityWebsitesReDo"

dir.create(sites.dir, recursive = T)

#write URLs to a text file
readr::write_lines(x = todo$cityWebsite, path = paste0(sites.dir, "/urls.txt"))

filetype.remove <- paste("'*.flv'","'*.mov'","'*.swf'","'*.xml'","'*.js'","'*.css'","'*.zip'","'*.gz'","'*.rar'","'*.7z'","'*.tgz'","'*.tar'","'*.z'","'*.gzip'","'*.bzip'","'*.tar'","'*.mp3'","'*.mp4'","'*.aac'","'*.wav'","'*.au'","'*.wmv'","'*.avi'","'*.mpg'","'*.mpeg'","'*.xls'","'*.xlsx'","'*.ppt'","'*.pptx'","'*.jpg'","'*.jpeg'","'*.png'","'*.gif'","'*.psd'","'*.ico'","'*.bmp'","'*.odt'","'*.ods'","'*.odp'","'*.odb'","'*.odg'","'*.odf'", sep = ", ")

#scrape
wget <- paste0("<urls.txt xargs -n 1 -P 30 -I % wget -r -N -R '", filetype.remove, "' -P ./ %")

readr::write_lines(x = wget, path = paste0(sites.dir, "/wget.sh"))

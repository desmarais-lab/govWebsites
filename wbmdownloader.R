#Function to use the Wayback Machine Downloader from within R
#Downloads website(s) from the Wayback Machine to ./websites folder

#Wayback Machine Downloader from
#https://github.com/hartator/wayback-machine-downloader
#needs to be installed for this to work

wbmdownloader <- function(website, concurrency = 50, from = 20170401, to = 20170501, path = "./WBM/Misc"){

  #pasting input for Ruby package, then executing it
  #--concurrency 20 causes 20 items to be downloaded at the same time
  #the default is 1, this takes WAY too long (i.e. one hour for a website...)
  #--from 201610 downloads a snapshot from October 2016, or, if not available, later
  WBMD_base <- paste("wayback_machine_downloader --concurrency ", concurrency,
                     " --from ", from, 
                     " --to ", from, 
                     " --directory ", path,
                     sep = "")
  WBMD_site <- paste(WBMD_base, website)
  system(WBMD_site, intern = T) #just ignore the printout if running outside of loop


}
wbmdownloader <- function(websites, concurrency = 20, from = 20170501){

  #loop through websites, results automatically get saved into 'websites' folder inside wd
  for (i in 1:length(websites)){
    
    website <- websites[i] #loop through websites
    
    #pasting input for Ruby package, then executing it
    #--concurrency 20 causes 20 items to be downloaded at the same time
    #the default is 1, this takes WAY too long (i.e. one hour for a website...)
    #--from 201610 downloads a snapshot from October 2016, or, if not available, later
    WBMD_base <- paste("wayback_machine_downloader --concurrency ", concurrency, " --from ", from, sep="")
    WBMD_site <- paste(WBMD_base, website)
    system(WBMD_site, intern = T) #just ignore the printout if running outside of loop
  }

}
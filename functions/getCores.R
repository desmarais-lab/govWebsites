#free RAM
#freeRAM <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))

#total RAM
#system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE)

#given the memory required by doing X (i.e. objectsize)
#how many cores can we use?
getCores <- function(objectsize = 2000){
  
  #available RAM, in megabytes
  availRAM <- as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo", intern=TRUE))/1000
  #how many cores, given the RAM
  nthreads <- as.integer(availRAM/objectsize)
  #how many threads does the computer have?
  maxthreads <- (parallel::detectCores())-1
  
  return(min(nthreads, maxthreads))
  
}

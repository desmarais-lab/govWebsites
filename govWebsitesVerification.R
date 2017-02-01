setwd("D:/Dropbox/4_RA/govWebsites")
setwd("/home/markus/Dropbox/4_RA/govWebsites")

results1 <- read.table("results/results1.txt")
results2 <- read.table("results/results2.txt")
results3 <- read.table("results/results3.txt")
results4 <- read.table("results/results4.txt")
results5 <- read.table("results/results5.txt")
results6 <- read.table("results/results6.txt")

result <- rbind(results1,results2,results3,results4,results5,results6)

data <- read.csv("data/current-full.csv")

data$redirect <- as.character(result$V1)

#devtools::install_github("jayjacobs/tldextract")
#library(tldextract)
#data$tld <- tldextract(data$redirect.V1)

data$timeout <- 0
data$timeout[data$redirect=="TimeoutException"] <- 1

data$dead <- 0
data$dead[data$redirect=="WebDriverException"] <- 1

data$redirect[data$redirect%in%c("TimeoutException","WebDriverException")] <- NA

data$loaded <- 0
data$loaded[is.na(data$redirect)==F] <- 1

save(data, file="data/govWebsitesVerified.Rdata")

xtabs(~loaded+Domain.Type, data=data)
#loaded City County Federal Agency Native Sovereign Nation State/Local Govt
#0  292    100            301                      39              221
#1 2133    497           1030                     130              903


#number of municipalities etc. in the US as of 2007:
#source: http://www.nlc.org/number-of-municipal-governments-population-distribution

#39,044 general purpose local governments
#19,492 municipal governments
#16,519 township governments
#3.033 county governments

#citytown.info
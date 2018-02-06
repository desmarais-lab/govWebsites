library("rvest")
library("stringr")

f <- list.files("websites/ny_campaign_finance", full.names = T)

for(j in 1:length(f)){

  #j=1
  
  #extract the links to the cities' wikipedia pages
  url <- f[j]
  mytable <- read_html(url) %>% html_nodes("table")# %>% .[[1]]
  if(length(mytable)>0){
    mytable <- mytable[[1]]
    df <- mytable %>% html_table(fill=T)
    
    df$dem_count <- NA
    df$rep_count <- NA
    df$dates <- NA
    df$contr_names <- NA
    df$amounts <- NA
    df$ncontribs <- NA
    
    for(i in 1:nrow(df)){
      
      #i=1
      
      k <- df$`FilerID Detail Link`[i]
      
      candidateURL <- paste0("http://www.elections.ny.gov:8080/plsql_browser/CONTRIBUTORA_COUNTY?ID_in=", k, "&date_From=01/01/2000&date_to=01/01/2018&AMOUNT_From=0&AMOUNT_to=1000000&ZIP1=&ZIP2=&ORDERBY_IN=N&CATEGORY_IN=ALL")
      
      mytable2 <- read_html(candidateURL) %>% html_nodes("table") %>% .[[2]] 
      df2 <- mytable2 %>% html_table(fill=T)
      
      dem_ind <- which(str_detect(df2$Contributor, "democrat|DEMOCRAT"))
      rep_ind <- which(str_detect(df2$Contributor, "republican|REPUBLICAN"))
      
      df$dem_count[i] <- length(dem_ind)
      df$rep_count[i] <- length(rep_ind)
      df$dates[i] <- list(c(df2$`Contr. Date`[dem_ind],df2$`Contr. Date`[rep_ind]))
      df$contr_names[i] <- list(c(df2$Contributor[dem_ind],df2$Contributor[rep_ind]))
      df$amounts[i] <- list(c(df2$Amt[dem_ind],df2$Amt[rep_ind]))
      df$ncontribs[i] <- nrow(df2)
      
    }
    
    if(j==1){
      data <- df
    }else{
      data <- rbind(data, df)
    }
  }else{
    print(paste0("No candidates in county ", j))
  }
  

}

data$REP <- F
data$DEM <- F
data$BOTH <- F
data$NEITHER <- F

data$REP[data$dem_count==0 & data$rep_count>0] <- T
data$DEM[data$dem_count>0 & data$rep_count==0] <- T
data$BOTH[data$dem_count>0 & data$rep_count>0] <- T
data$NEITHER[data$dem_count==0 & data$rep_count==0] <- T


save(data, file = "rfiles/scrapedCampaignFinanceNY.rdata")

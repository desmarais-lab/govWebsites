library(tidyr)
library(ggplot2)

df <- unlist(docDuplicates)


lens <- lapply(docDuplicates, length)
lens2 <- rep(1:16947, lens)

df <- cbind(df, lens2)
df <- as.data.frame(df)



df$City <- rep(d$City, lens)



#df2 <- df[df$City%in%sample(unique(df$City), 5),]
#unique(df$City)[(a[2]):(a[2]+5)]

#a <- seq(1, 21, length.out = 5)

# Make one density plot per city

cities <- names(table(d$City))[table(d$City)>10]
citylengths <- table(d$City)[table(d$City)>10]

for(i in 1:length(cities)){

  df2 <- df[df$City%in%cities[i],]

  ggplot(df2, aes(x = df)) + geom_density() + 
    labs(title = paste0(cities[i], " (", citylengths[i], " documents)"))

  ggsave(paste0("paper/figures/cutoffs/LA/", cities[i], ".pdf"))
}

# All cities

ggplot(df, aes(x = df)) + geom_density() + 
  labs(title = "all cities")

ggsave("paper/figures/cutoffs/LA/all_cities.pdf")





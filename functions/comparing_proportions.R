zcomparison <- function(Rep = republican, Dem = democratic){
  
  n_1 <- sum(colSums(Rep))
  y_1 <- colSums(Rep)
  p_1 <- y_1/n_1
  
  n_2 <- sum(colSums(Dem))
  y_2 <- colSums(Dem)
  p_2 <- y_2/n_2
  
  phat <- (y_1+y_2)/(n_1+n_2)
  
  z <- (p_1-p_2)/sqrt((phat*(1-phat))*(1/n_1+1/n_2))

  return(z)
  
}


#one-sided: 
#pnorm(-abs(zcomparison()))
#two-sided:
#2*pnorm(-abs(zcomparison()))

#with R's prop.test()

#a <- numeric(length = 200)
#for(i in 1:length(a)){
#  a[i] <- prop.test(x = c(colSums(republican)[i], colSums(democratic)[i]), n = c(sum(colSums(republican)), sum(colSums(democratic))), correct = F)$p.value
#  }
#
#
#a2 <- pnorm(-abs(zcomparison()))
#
#identical(a,a2)
#cor(a,a2)

#The two methods don't produce identical results, 
#but the Pearson's R correlation between the two is 0.9999999 with the continuity correction, 
#and 1 without (but nevertheless not identical)
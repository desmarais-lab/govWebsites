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


#one-sided: pnorm(-abs(zcomparison()))
#pval <- 2*pnorm(-abs(zcomparison()))

#Function for controlling if the p-value is < alpha.
#If p<alpha, null hyp is rejected and is returned TRUE.
pvalue_control <- function(x, alpha){
  ogg<-summary(x)
  coeff<-coef(ogg)
  if(coeff[2,4]>=alpha) pbool <- FALSE
  else pbool <- TRUE
  return(pbool)
}
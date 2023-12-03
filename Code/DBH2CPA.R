
# Estimate CPA from DBH 

CPA <- function(x){
  c <- exp(4.5680-31.6134/x)
  return(c)

}


DBH <- data.frame(DBH = rnorm(10, mean = 30, sd=1) )
DBH

CPA <- apply(DBH,1, FUN =  (CPA))
CPA

# Estimate DBH from Crown Projected Area (CPA) 

DBH <- function(c){
  x = 31.6134/ (-log(c) + 4.5680)
    return(x)
}


CPA <- data.frame(CPA = rnorm(10, mean = 30, sd=1) )
CPA

DBH <- apply(CPA,1, FUN =  (DBH))
DBH

## Transformation matrix
## Using Transformation Matrices with Point Clouds

# Load library ------------------------------------------------------------

library(dplyr)


# Matrix Form -------------------------------------------------------------


# abcTX
# defTY
# ghiTZ
# 0001


# Input angles ------------------------------------------------------------

alpha <- 0.1018 #deg
beta <- -0.0421 #deg
gamma <- -132.5821 #deg


# Degree to Radian conversion ---------------------------------------------

deg2rad <- function(x){
  rad <- pi/180*x
  return(rad)
}

alpha <- deg2rad(alpha)
beta <- deg2rad(beta)
gamma <- deg2rad(gamma)

# Input translation data --------------------------------------------------


Tx <- 491878.3232
Ty <- 4393217.6072
Tz <- 1467.0904


# Compute Rotation Matrices -----------------------------------------------

yaw <- matrix(c(cos(alpha),-sin(alpha), 0, 
                sin(alpha), cos(alpha), 0, 
                0, 0, 1), nrow = 3, byrow = TRUE);yaw

pitch <- matrix(c(cos(beta),0, sin(beta), 
                  0, 1, 0, 
                  -sin(beta), 0, cos(beta)), nrow = 3, byrow = TRUE);pitch

roll <- matrix(c(1, 0, 0, 
                 0, cos(gamma), -sin(gamma), 
                 0, sin(gamma), cos(gamma)), nrow = 3, byrow = TRUE);roll

Rt <- yaw*pitch*roll;Rt


# Compute Transformation (Full) Matrice ----------------------------------

Tr.mat <- cbind(Rt, c(Tx,Ty,Tz)) %>% rbind(c(0,0,0,1)); Tr.mat


# References --------------------------------------------------------------

# Luhmann, T., Robson, S., Kyle, S., & Harley, I. (2006). Close Range Photogrammetry. 
# http://www.kwon3d.com/theory/transform/rot.html


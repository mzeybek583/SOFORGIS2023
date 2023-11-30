
## 3DR Export File format

### Read Reshaper Least square cylinder Fit data ####

library(stringr)
library(dplyr)
library(tidyr)

file3 <- "E:/O3-DBH_3DR.txt"
export.cylinder <- "E:/reference_HMLS_cylinder.csv"

CylinderFit <- read.csv(file = file3)

class(CylinderFit)
colnames(CylinderFit) <- c("data")
CylinderFit <- str_split_fixed(CylinderFit$data, pattern = ":", 2)

CylinderFit <- as.data.frame(CylinderFit)
colnames(CylinderFit) <- c("Name", "Variable")

cy_fit.center <- CylinderFit[which(CylinderFit$Name == "Center", arr.ind = TRUE),]

cy_fit.r <- CylinderFit[which(CylinderFit$Name == "Radius", arr.ind = TRUE),]
cy_fit.r$Variable <- gsub("[a-zA-Z ]", "", cy_fit.r$Variable)
cy_fit.r$Variable<-as.numeric(as.character(cy_fit.r$Variable))
#cy_fit.r$Variable <- sort(cy_fit.r$Variable)

cy_fit.r <- cy_fit.r[,2]*100

## center

class(cy_fit.center$Variable)

centers <- data.frame(str_split(cy_fit.center$Variable ,n = 4, pattern = " ", simplify = TRUE)[,2:4])
center.rad <- data.frame(centers, cy_fit.r)
center.dia <- cy_fit.r*2
DBH <- 1.30
ID <- seq(1, nrow(centers))
center.rad <- cbind(ID,center.rad,center.dia,DBH)
colnames(center.rad) <- c("ID","Y", "X", "Z", "Radius (cm)", "Diameter (cm)", "DBH(m)")

min(center.rad$`Diameter (cm)`)
max(center.rad$`Diameter (cm)`)

write.table(center.rad, export.cylinder, quote = FALSE, sep = ";", row.names = FALSE)



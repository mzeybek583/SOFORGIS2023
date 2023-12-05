
## Calculate GAP Closure in Plot Level

library(svMisc)
library(lidR)
library(raster)
library(terra)
library(ggplot2)
library(tidyterra)

time <- proc.time()

#apply(average_slope,)
#average_slope(plot1)
 closure <- data.frame(matrix(ncol = 4, nrow = 0, 
                              dimnames = list(NULL, c("Plot ID", "Plot Area", "Canopy Area", "Closure (Percentage)"))))

 las <- readLAS("E:/2022-09-15_20-21-Beynam_koordinatli_30O3.las")
 
 # Get info about las
 
 #density(las)
 #d <- rasterize_density(las, 1)
 #plot(d)
 #plot(las)
 
 
 # Clip circle -------------------------------------------------------------
 
 las <- clip_circle(las, 491884.259298, 4393185.862824, 42)  
 
 # Decimation PCL ----------------------------------------------------------
 thinned <- decimate_points(las, homogenize(200,1))
 
 # Classification ----------------------------------------------------------
 thinned <- classify_noise(thinned, ivf(1,1))
 #plot(thinned)
 las_denoise <- filter_poi(thinned, Classification != LASNOISE)
 #plot(las_denoise)
 
 mycsf <- csf(TRUE, 1, 1, time_step = 1)
 lasground <- classify_ground(las_denoise, mycsf)
 
 dtm <- rasterize_terrain(lasground, 1, knnidw(k = 6L, p = 2))
 nlas <- normalize_height(lasground, dtm)
 las <- nlas
 plot(las,color = "Z",legend=TRUE)
 
 f <- function(x) {x * 0.1 + 3}
 chm <- rasterize_canopy(las, 0.1, pitfree(subcircle = 0.2), pkg = "terra")
 w <- matrix(0.5, 3, 3)
 smoothed <- terra::focal(chm, w, fun = mean, na.rm = TRUE)
 plot(smoothed) 
 chm <- smoothed 
 ## Find the min and max z-values
 rng <- range(chm[],na.rm = TRUE)
  
  ## Construct a list of arguments to be used for the color table
  arg <- list(at=rng, labels=round(rng, 1))
  
  ## Pass them in to your call to plot
  #plot(r1,breaks=brks,col=gray(seq(0,1,length=n)), axis.args=arg)
  brks<-seq(0,1,0.1)
  arg <- list(at=brks, labels=round(brks, 1))
  
  plot(chm, breaks=brks, col = height.colors(4), axis.args=arg)
  
  #algo = watershed(chm, th = 1)
  #las  = lastrees(las, algo)
  
  # remove points that are not assigned to a tree
  #trees = lasfilter(las, !is.na(treeID))
  #plot(trees, color = "treeID", colorPalette = pastel.colors(100))
  #print(sort(unique(trees@data$treeID)))
  
  #hulls  = tree_hulls(las, func = .stdmetrics)
  #spplot(hulls, "zmax")
  
  crowns = watershed(chm, th = 0.1)()
  
  contour = terra::as.polygons(crowns, aggregate=TRUE)
  png_file <-paste("result/Canopy closure", ".png")
  png(filename = png_file, width = 480, height = 480, units = "px")
  plot(chm, breaks=brks, col = height.colors(5))
  dev.off()
  
  png_file2 <-paste("result/Crowns", ".png")
  png(filename = png_file2, width = 480, height = 480, units = "px")
  plot(crowns, col = pastel.colors(200))
  plot(contour, add = T)
  dev.off()
  #max(chm@data@values, na.rm = TRUE)
  

  area <- expanse(contour, unit="m")

    closure[1,1] <- 1
    closure[1,2] <- lidR::area(las)
    closure[1,3] <- sum(area)
    closure[1,4] <- sum(area)/lidR::area(las)

#  if(i==max(ind)) cat("Done!\n")
  print(proc.time()- time)

 

# Write result to file ----------------------------------------------------

write.table(file = "./result/closure_result.csv", closure, sep = ";") 


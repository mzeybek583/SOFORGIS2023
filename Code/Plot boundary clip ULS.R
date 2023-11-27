

# Libs --------------------------------------------------------------------
library(lidR)

# Read Data ---------------------------------------------------------------
#Plot1
plt <- readLAS("E:/2022-09-15_20-33-Beynam_koordinatli_52O4.las")
UAV <- readLAS("E:/StripYeni-20220111-091648.las")

# Downsampling HMLS -------------------------------------------------------
thinned <- decimate_points(plt, homogenize(1,5))
plot(grid_density(thinned, 10))
plot(thinned)

# Create Boundary ---------------------------------------------------------
hull = st_concave_hull(thinned, length_threshold = 100)
plot(hull)
rm(plt)
rm(thinned)

# Clip UAV LiDAR Data -----------------------------------------------------
clipped_UAV <- clip_polygon(UAV, hull[[1]][[1]][,1], hull[[1]][[1]][,2])
epsg(clipped_UAV)
#st_crs(clipped_UAV) <- 5255
plot(clipped_UAV)

# Export Clipped LiDAR Data -----------------------------------------------
writeLAS(clipped_UAV, "UAV_Plot4.las")


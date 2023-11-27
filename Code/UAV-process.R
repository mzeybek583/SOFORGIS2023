# UAV LiDAR Processing
# v.09.05.2023
# mzeybek@selcuk.edu.tr


# Load Libs ---------------------------------------------------------------
library(lidR)
library(raster)
library(terra)
library(sf)

# Load data ---------------------------------------------------------------
las <- readLAS("UAV_Plot3.las")

# Get info about las

  density(las)
  d <- rasterize_density(las, 1)
  plot(d)
  plot(las)
# Decimation PCL ----------------------------------------------------------
  thinned <- decimate_points(las, homogenize(200,1))

# Classification ----------------------------------------------------------
thinned <- classify_noise(thinned, ivf(1,1))
plot(thinned)
las_denoise <- filter_poi(thinned, Classification != LASNOISE)
plot(las_denoise)

mycsf <- csf(TRUE, 1, 1, time_step = 1)
lasground <- classify_ground(las_denoise, mycsf)
plot(lasground,color = "Classification")

# Calculate Height above ground level -------------------------------------
dtm <- rasterize_terrain(lasground, 1, knnidw(k = 6L, p = 2))
nlas <- normalize_height(lasground, dtm)
plot(dtm)
plot(nlas)

# Individual Tree Detection -----------------------------------------------
# Using raster because focal does not exist in stars

# ITD 1
  # chm <- rasterize_canopy(nlas, res = 0.5, p2r(0.3), pkg = "raster")
  # ker <- matrix(1,3,3)
  # chm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
  # las_tree <- segment_trees(nlas, watershed(chm))
  # col <- pastel.colors(250)
  # plot(las_tree, color = "treeID", colorPalette = col)
#
# ITD2
# Very custom variable windows size
  # f <- function(x, y, z) { x * 0.07 + y * 0.01 + z}
  # ws_args <- list(x = "Z", y = "Intensity", z = 3)
  # ttops <- locate_trees(nlas, lmf(f, ws_args = ws_args))

# ITD3

  # chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
  # ttops <- locate_trees(nlas, lmf(ws = 3))

  # plot(chm, col = height.colors(50))
  # plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

  # x <- plot(nlas, bg = "white", size = 4)
  # add_treetops3d(x, ttops, col="black")

# ITD 4
  # f <- function(x) {x * 0.1 + 3}
  # heights <- seq(0,30,5)
  # ws <- f(heights)
  # plot(heights, ws, type = "l", ylim = c(0,6))
  # ttops <- locate_trees(nlas, lmf(f))
  # plot(chm, col = height.colors(50))
  # plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

# ITD 5
f <- function(x) {x * 0.1 + 3}
chm_pitfree_05_2 <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2), pkg = "terra")
ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(f))
plot(chm_pitfree_05_2, main = "CHM PITFREE 2", col = height.colors(50)); plot(sf::st_geometry(ttops_chm_pitfree_05_2), add = T, pch =3)

x <- plot(nlas, bg = "white", size = 4)
add_treetops3d(x, ttops_chm_pitfree_05_2, col="black")


# Calculate Tree ID -------------------------------------------------------
algo <- dalponte2016(chm_pitfree_05_2, ttops_chm_pitfree_05_2)
las_tree <- segment_trees(nlas, algo) # segment point cloud
plot(las_tree, bg = "white", size = 4, color = "treeID") # visualize trees
max(las_tree$treeID, na.rm = TRUE)

# Calculate tree crown area -----------------------------------------------
m <- ~list(avgI = mean(Intensity))
c <- crown_metrics(las_tree, m, geom = "convex")

crowns <- crown_metrics(las_tree, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

hist(crowns$convhull_area)

p.metrics <- crown_metrics(las_tree, .stdtreemetrics)
plot(p.metrics["Z"], pch = 19,add=TRUE)
plot(p.metrics["convhull_area"], main = "Crown area (convex hull)", add=TRUE)



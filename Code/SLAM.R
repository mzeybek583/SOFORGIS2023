
library(lidR)
library(TreeLS)

# Read Data ---------------------------------------------------------------

las <- readLAS(files = "E:/.../O1_clip.las")
#plot(las)
las@header
las@header@PHB[["X scale factor"]] <- 0.001
las@header@PHB[["Y scale factor"]] <- 0.001
las@header@PHB[["Z scale factor"]] <- 0.001

thinned <- decimate_points(las, random_per_voxel(1,600))
# plot(thinned)

thinned <- filter_duplicates(thinned)
#plot(thinned)

mycsf <- csf(TRUE, 0.1, 1, time_step = 1)
thinned <- classify_ground(thinned, mycsf)
# plot(thinned, color = "Classification")

tls = tlsNormalize(thinned, 0.1, TRUE)
writeTLS(tls,"normalizedLAS_medium_O1.las")

x = plot(tls)

# map <- treeMap(tls, method = map.hough(min_h = 0.5,
#                                        max_h = 1.5,
#                                        h_step = 0.5,
#                                        pixel_size = 0.025,
#                                        min_density = 0.005,
#                                        min_votes = 3), merge = 0.1, positions_only = FALSE)

# map <- treeMap(tls, method = map.hough(min_h = 0.5, max_h = 1.5,min_density = 0.02))
# map <- treeMap(tls, map.hough(min_density = 0.1), 0)
# plot(map)

# add_treeMap(x, map, color='red')


all_metrics = fastPointMetrics.available()
my_metrics = all_metrics[c(16, 11)]

Stems = fastPointMetrics(tls, ptm.knn(50), my_metrics)
#plot(Stems)
tlsfilter <- filter_poi(Stems, Verticality > 80, Verticality < 95)
#tlsPlot(tlsfilter)
tlsfilter <- filter_poi(tlsfilter, Eigentropy < .03)
#tlsPlot(tlsfilter)

map = treeMap(tlsfilter, map.hough(min_h = 1, max_h= 3, h_step = 0.5, min_votes = 1, pixel_size=0.05), merge = 0.2)
plot(map)

#x = plot(tls)
#add_treeMap(x, map, color='yellow', size=2)

tls = treePoints(tls, map, trp.crop(circle=FALSE))
plot(tls)

tls = stemPoints(tls, stm.hough(pixel_size = 0.02,h_base = c(0.5, 1.5), min_votes = 10))
plot(tls, color="TreeID")

dmt = shapeFit(shape = 'circle', algorithm='ransac', n=20)
inv = tlsInventory(tls, d_method = dmt)
tlsPlot(tls, inv)

write.table(inv, "clean_trees_medium_inventory_O1.csv", sep = ";")







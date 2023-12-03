
# treecrown and height model

library(ForestTools)
library(terra)

chm <- rast("HMLS_chm_pitfree_04_2.tif")
hmls <- vect("Plot3_HMLS_export.shp")

ttops <- vwf(chm, winFun = function(x){x * 0.06 + 1}, minHeight = 4)
plot(hmls)

# Segment tree crowns
segs <- mcws(ttops, chm, minHeight = 1, format = "polygon")
st_crs(segs) <- "epsg:5255"
(segs_geom <- st_geometry(segs))

summary(segs)

segs <- as(segs, "Spatial")
segs <- vect(segs)
area <- expanse(segs, unit="m")
summary(area)

plot(chm)
plot(segs, add=TRUE)
plot(ttops["height"], add=TRUE)
plot(hmls, add=TRUE, col="red", pch=19)


### Info
# http://www.gadm.org/ - map data
# http://www.gdal.org/ - coord transformer
# http://cran.r-project.org/web/packages/rgdal/index.html - gdal in R
# http://spatialreference.org/ref/epsg/3059/ - LKS92 info
# http://neogeo.lv/ekartes/koord2/ - test

# http://laacz.lv/2009/11/10/pagasti-novadi-un-to-esamiba-gugles-kartes/#c67237
# http://www.gisnet.lv/gisnet/2007/04/sesu-miljonu-opera/comment-page-1/#comment-86


## you will need the sp-package
library(sp)
library(rgdal)
require(bigmemory)
library(grid)
library(ggplot2)

## load a file from GADM (you just have to specify the countries "special part" of the file name, like "ARG" for Argentina. Optionally you can specify which level you want to have

loadGADM <- function (fileName, level = 0, ...) {
  load(url(paste("http://gadm.org/data/rda/", fileName, "_adm", level, ".RData", sep = "")))
  gadm
}

LV0 <- loadGADM("LVA", 0)
LV1 <- loadGADM("LVA", 1)
LV2 <- loadGADM("LVA", 2)

plot(LV0)
plot(LV1)
plot(LV2)

bbox(LV0)

coordinates(LV0)
coordinates(LV1)
coordinates(LV2)

LV0_LKS92 <- spTransform(LV0, CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

LV1_LKS92 <- spTransform(LV1, CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

LV2_LKS92 <- spTransform(LV2, CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

bbox(LV0_LKS92)
bbox(LV1_LKS92)
bbox(LV2_LKS92)

plot(LV0_LKS92)
points(500000, 300000, pch = 3, cex = 0.4, col = "red")

plot(LV1_LKS92)
points(500000, 300000, pch = 3, cex = 0.4, col = "red")

plot(LV2_LKS92)
points(500000, 300000, pch = 3, cex = 0.4, col = "red")




class(LV0)
class(LV0_LKS92)

# plot(LV0)
# points(LV0_LKS92)
# 
# ?overlay
# 
# overlay(LV0, LV0_LKS92)
# 
# temp <- rbind(LV0, LV0_LKS92)
# 
# 
# getCountries <- function (fileNames, level = 0, ...) {
#   polygon <- sapply(fileNames, loadChangePrefix, level)
#   polyMap <- do.call("rbind", polygon)
#   polyMap
# }
# 
# spdf <- getCountries(c("LV0","LV0_LKS92"))
# plot(spdf) # should draw a map with Brasil, Argentina and Chile on it.
# 
# 
# 
# plot(LV0)
# 
# 
# plot
# 
# ?spplot
# ?grid.raster
# 
# help.start()


#### Save maps

# setwd("~/Dropbox/LU/Darbs/Simulation/050_Simulation/20_Procedures")
setwd(projwd)
save(LV0_LKS92, LV1_LKS92, LV2_LKS92, file = "LV_maps.Rdata")


#### Data

dir.data = "~/DATA/LU/Work"
setwd(dir.data)


frame.h <- attach.big.matrix("frame.h.desc")
head(frame.h)

geo <- frame.h[sample(nrow(frame.h), 1e3), c("coord_x_p", "coord_y_p")]


setwd("~/temp")

pdf("map_Latvia.pdf", paper="a4r")

plot(LV0_LKS92)
points(geo, pch = 3, cex = 0.4, col = "red")
title("Latvia")

plot(LV1_LKS92)
points(geo, pch = 3, cex = 0.4, col = "red")
title("Latvia (Regions)")

plot(LV2_LKS92)
points(geo, pch = 3, cex = 0.4, col = "red")
title("Latvia (Rajoni)")

dev.off()


### END ###

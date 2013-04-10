### Info
# http://www.gadm.org/ - map data
# http://www.gdal.org/ - coord transformer
# http://cran.r-project.org/web/packages/rgdal/index.html - gdal in R
# http://spatialreference.org/ref/epsg/3059/ - LKS92 info
# http://neogeo.lv/ekartes/koord2/ - test

# http://laacz.lv/2009/11/10/pagasti-novadi-un-to-esamiba-gugles-kartes/#c67237
# http://www.gisnet.lv/gisnet/2007/04/sesu-miljonu-opera/comment-page-1/#comment-86


## you will need the sp-package
# require(sp)
# require(rgdal)

require(bigmemory)
require(bigtabulate)

# require(grid)
require(ggplot2)
require(scales)

### Procedures
sourceDir(dir.proc, F)


### Settings
pdf.options(family = "Times")


# Function for ggplot2 colour scales
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

## load a file from GADM (you just have to specify the countries "special part" of the file name, like "ARG" for Argentina. Optionally you can specify which level you want to have

# loadGADM <- function (fileName, level = 0, ...) {
#   load(url(paste("http://gadm.org/data/rda/", fileName, "_adm", level, ".RData", sep = "")))
#   gadm
# }
# 
# LV0 <- loadGADM("LVA", 0)
# LV1 <- loadGADM("LVA", 1)
# LV2 <- loadGADM("LVA", 2)
# 
# plot(LV0)
# plot(LV1)
# plot(LV2)
# 
# bbox(LV0)
# 
# coordinates(LV0)
# coordinates(LV1)
# coordinates(LV2)
# 
# LV0_LKS92 <- spTransform(LV0, CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# 
# LV1_LKS92 <- spTransform(LV1, CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# 
# LV2_LKS92 <- spTransform(LV2, CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# 
# bbox(LV0_LKS92)
# bbox(LV1_LKS92)
# bbox(LV2_LKS92)
# 
# plot(LV0_LKS92)
# points(500000, 300000, pch = 3, cex = 0.4, col = "red")
# 
# plot(LV1_LKS92)
# points(500000, 300000, pch = 3, cex = 0.4, col = "red")
# 
# plot(LV2_LKS92)
# points(500000, 300000, pch = 3, cex = 0.4, col = "red")
# 
# class(LV0)
# class(LV0_LKS92)
# 
# 
# #### Save maps
# 
# setwd(projwd)
# save(LV0_LKS92, LV1_LKS92, LV2_LKS92, file = "LV_maps.Rdata")



#### Example ####

# dir.data = "~/DATA/LU/Work"
# setwd(dir.data)
# 
# frame.h <- attach.big.matrix("frame.h.desc")
# head(frame.h)
# 
# geo <- frame.h[sample(nrow(frame.h), 1e3), c("coord_x_p", "coord_y_p")]
# 
# 
# setwd("~/temp")
# 
# pdf("map_Latvia.pdf", paper="a4r")
# 
# plot(LV0_LKS92)
# points(geo, pch = 3, cex = 0.4, col = "red")
# title("Latvia")
# 
# plot(LV1_LKS92)
# points(geo, pch = 3, cex = 0.4, col = "red")
# title("Latvia (Regions)")
# 
# plot(LV2_LKS92)
# points(geo, pch = 3, cex = 0.4, col = "red")
# title("Latvia (Rajoni)")
# 
# dev.off()



#### Produce population and sample examples

###

setwd(dir.data)

load("frame.h.Rdata")
head(frame.h.df)

load("frame.int.Rdata")
head(frame.int)

load("frame.PSU.Rdata")
head(frame.PSU)

###

setwd(file.path(projwd, "Data"))
load("LV_maps.Rdata")

# plot(LV2_LKS92)
# head(as.data.frame(LV2_LKS92))

LV2_LKS92_df <- fortify(LV2_LKS92)
dim(LV2_LKS92_df)
head(LV2_LKS92_df)
tail(LV2_LKS92_df)

ind <- sample(nrow(frame.h.df), 50e3)
ind <- 1:nrow(frame.h.df)

a <- .2
s <- .5

pl1 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
  geom_point(size = .5, colour = "gray") +
  geom_point(aes(coord_x_p, coord_y_p),
             frame.h.df[ind, ], alpha = a, size = s,
             colour = gg_color_hue(3)[3]) +
  labs(x = "LKS92 E", y = "LKS92 N") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_bw()
# pl1

pl2 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
  geom_point(size = .5, colour = "gray") +
  geom_point(aes(coord_x_p, coord_y_p, colour = factor(strata3)),
             frame.h.df[ind, ], alpha = a, size = s) +
  labs(x = "LKS92 E", y = "LKS92 N") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_bw() + theme(legend.position="none")
# pl2

# head(frame.int)
# nrow(frame.int)
# table(frame.int$raj)
# 
# pl3 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
#   geom_point(size = .5, colour = "gray") +
#   geom_point(aes(coord_x_p, coord_y_p),
#              frame.h.df[ind, ], alpha = a, size = s,
#              colour = gg_color_hue(3)[3]) +
#   geom_point(aes(x_int, y_int),
#              frame.int, shape = 2,
#              colour = "red") +
#   labs(x = "LKS92 E", y = "LKS92 N") +
#   scale_y_continuous(labels = comma) +
#   scale_x_continuous(labels = comma) +
#   theme_bw()
# pl3

# head(frame.h.df)
# 
# pl4 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
#   geom_point(size = .5, colour = "gray") +
#   geom_point(aes(coord_x_p, coord_y_p, colour = factor(int.ID)),
#              frame.h.df[ind, ], alpha = a, size = s) +
#   geom_point(aes(x_int, y_int, colour = factor(int.ID)),
#              frame.int, shape = 2) +
#   labs(x = "LKS92 E", y = "LKS92 N") +
#   scale_y_continuous(labels = comma) +
#   scale_x_continuous(labels = comma) +
#   theme_bw() + theme(legend.position="none")
# pl4

head(frame.h.df)

pl5 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
  geom_point(size = .5, colour = "gray") +
  geom_point(aes(coord_x_p, coord_y_p, colour = factor(strata3)),
             frame.h.df[ind, ], alpha = a, size = s) +
  geom_point(aes(x_int, y_int),
             frame.int, shape = 2) +
  labs(x = "LKS92 E", y = "LKS92 N") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_bw() + theme(legend.position="none")
pl5

str <- factor(frame.h.df[, "strata"],
                 labels = c("Riga", "Cities", "Towns", "Rural a."))
table(str)

length(ind)

ind <- sample(nrow(frame.h.df), 50e3)
ind <- 1:nrow(frame.h.df)

pl6 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
  geom_point(size = .5, colour = "gray") +
  geom_point(aes(coord_x_p, coord_y_p,
                 colour = factor(strata,
                                 labels = c("Riga", "Cities",
                                            "Towns", "Rural a."))),
             data = frame.h.df[ind, ], alpha = a, size = s) +
  labs(x = "LKS92 E", y = "LKS92 N") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(colour = "strata") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  theme_bw(base_size = 18) + coord_fixed()
# pl6

setwd(dir.res)

# ggsave(pl1, file = "Map_LV_all.png")
# ggsave(pl2, file = "Map_LV_strata3.png")
# ggsave(pl5, file = "Map_LV_strata3_int.png")
ggsave(pl6, file = "Map_LV_strata.png", dpi = 150)


### Rural PSU serpentine #####

head(frame.PSU[frame.PSU$strata == 4, ])

pl6 <- ggplot(LV2_LKS92_df, aes(long, lat)) +
  geom_point(size = .5, colour = "gray") +
  geom_point(aes(x_PSU, y_PSU, colour = "red"),
            data = frame.PSU[frame.PSU$strata == 4, ]) +
  geom_path(aes(x_PSU, y_PSU, linetype = "dotted"),
            data = frame.PSU[frame.PSU$strata == 4, ]) +
  labs(x = "LKS92 E", y = "LKS92 N") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_bw(base_size = 24) + theme(legend.position="none") + coord_fixed()
pl6

setwd(dir.res)

ggsave(pl6, file = "PSU_serpentine_rural.pdf")



#### Sample

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")

load("frame.PSU.Rdata")
load("frame.int.Rdata")

M <- nrow(frame.h)
M

M.h <- as.vector(bigtable(frame.h, "strata"))
M.h
sum(M.h)
sum(M.h) == M

weeks <- 1

sampl.des.par.1 <- data.frame(s = 1:4,
        A = 8,
        B = c(1, 2, 2, 2),
        W = 13,
        d = 0,
        Q = c(6, 2, 7, 11),
        w = weeks,
        M = M.h,
        m = c(10, 7, 8, 9))

sampl.des.par.1

m <- sum(sampl.des.par.1$A * sampl.des.par.1$B * sampl.des.par.1$w * sampl.des.par.1$m)
m

n1 <- c(1261, 1781, 2834) / 13
n2 <- c(1001, 1404, 2340) / 13
n1
n2

des1 <- 'SamplingSRSStrWeek(frame.1 = frame.p, n = n1, name.strata = "strata3", weeks = 13)'
des2 <- 'SamplingClusterStrWeek(frame.1 = frame.p, frame.2 = frame.h, n = n2, name.cluster = "H_ID", name.strata = "strata3", weeks = 13)'
des3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par.1)'

set.seed(20130220)
s1 <- lapply(1:4, function(x) eval(parse(text = des1)))
s2 <- lapply(1:4, function(x) eval(parse(text = des2)))
s3 <- lapply(1:4, function(x) eval(parse(text = des3)))

samples <- c(s1, s2, s3)
length(samples)

head(samples[[1]])

plot.Trip(samples[[1]], "coord_x_p", "coord_y_p", "int.ID",
  coord.int = frame.int[c("int.ID", "x_int", "y_int")], map = LV2_LKS92)

plots <- lapply(samples, plot.Trip, "coord_x_p", "coord_y_p", "int.ID",
  coord.int = frame.int[c("int.ID", "x_int", "y_int")], map = LV2_LKS92)

plots[[1]]
plots[[5]]
plots[[9]]

# Save
setwd(dir.res)

pdf.options()$encoding
pdf.options(encoding = "CP1257")
pdf.options()$encoding

lapply(1:12, function(i) ggsave(plots[[i]],
    filename = sprintf("Plot_sample_%03d.pdf", i)))

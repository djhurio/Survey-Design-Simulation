############# Population data


### Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")



### Libs

# 2013-03-09 Windows support is temporarily suspended
# http://cran.r-project.org/web/packages/bigmemory/NEWS
# require(bigmemory)
# require(bigtabulate)

require(ggplot2)
require(data.table)

require(TSP)
require(maps)
require(sp)
require(maptools)
require(rgdal)

require(plotrix)

require(cluster)



# Files and Directories

# Original data (TXT)
file.data.in <- file.path(dir.data.source, "Population.txt")

dir.maps <- file.path(dir.data, "maps")


### Functions

### Function to draw circles

plot.circles <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)

  plot(x[, 1], x[, 2], pch = 20, cex = .01, asp = 1)
  for (i in 1:n) draw.circle(x[i, 1], x[i, 2], x[i, 3], border = "red")
}

# plot.circles(x)
# plot.circles(cbind(c(1:10), sample(10,10), runif(10)))
# plot.circles(cbind(1, 1, .2))







### Read data from source

setwd(dir.data)

# pop <- read.big.matrix(file.data.in,
#  header = TRUE,
#  type = "integer",
#  sep = "\t",
#  backingfile = "pop.bin",
#  descriptor = "pop.desc")

# pop <- as.big.matrix(as.matrix(read.delim(file.data.in)),
#                      type = "integer",
#                      backingfile = "pop.bin",
#                      descriptorfile = "pop.desc")

pop <- fread(file.data.in)
pop

save(pop, file = "pop.Rdata")

# rm(pop)
# gc()


######################
### Create frame.p ###
######################

setwd(dir.data)

# pop.orig <- attach.big.matrix("pop.desc")

load("pop.Rdata")

head(pop)

N <- nrow(pop)
N

pop
sapply(pop, function(x) sum(is.na(x)))

# frame.p <- big.matrix(N,
#                       ncol(pop.orig) + 11,
#                       type = "integer",
#                       backingfile = "frame.p.0.bin",
#                       descriptorfile = "frame.p.0.desc",
#                       dimnames = list(NULL,
#                                       c(colnames(pop.orig),
#                                         "const",
#                                         "strata",
#                                         "raj",
#                                         "int.ID",
#                                         "All",
#                                         "Empl",
#                                         "Unempl",
#                                         "Inact",
#                                         "Act",
#                                         "Work.time",
#                                         "Reg.unempl"
#                                         )
#                                       )
#                       )
# 
# nrow(frame.p)
# 
# class(pop.orig)
# class(frame.p)
# 
# frame.p[, colnames(pop.orig)] <- pop.orig[order(pop.orig[,"iec2010"],
#                                                 pop.orig[,"H_ID"],
#                                                 pop.orig[,"P_ID"]
#                                                 ), ]



### Deleting records without coord

# frame.p <- as.big.matrix(frame.p[!is.na(frame.p[, "coord_x_p"]) & !is.na(frame.p[, "coord_y_p"]), ],
#                          backingfile = "frame.p.bin",
#                          descriptorfile = "frame.p.desc")

pop[, .N / N * 100, keyby = is.na(pop$coord_x_p)]
pop[, .N / N * 100, keyby = is.na(pop$coord_y_p)]


# Select only records with known geo coordinates

pop <- pop[!is.na(coord_x_p) & !is.na(coord_y_p)]
nrow(pop)

N <- nrow(pop)


# Const
# frame.p[,"const"] <- 1L
pop[, const := 1L]


# Strata
# frame.p[, "strata"] <- as.integer(floor(frame.p[, "iec2010"] / 1e5))
pop[, strata := as.integer(floor(iec2010 / 1e5))]

# Rajons
# frame.p[, "raj"] <- as.integer(floor(frame.p[, "nov"] / 1e2))
# bigtable(frame.p, ccols = "raj")
pop[, raj := as.integer(floor(nov / 1e2))]

# Study variables
# frame.p[,"All"] <- 1L
# frame.p[,"Empl"] <- as.integer(frame.p[,"eka"] == 1)
# frame.p[,"Unempl"] <- as.integer(frame.p[,"eka"] == 2)
# frame.p[,"Inact"] <- as.integer(frame.p[,"eka"] == 3)
# frame.p[,"Act"] <- as.integer(frame.p[,"eka"] %in% c(1,2))
# frame.p[,"Work.time"] <- ifelse(frame.p[,"eka"] == 1, as.integer(frame.p[,"E59"]), 0L)


# Missing values imputed by 0
# frame.p[,"Work.time"][is.na(frame.p[,"Work.time"]) | frame.p[,"Work.time"] %in% c(998, 999)] <- 0L
# summary(frame.p[,"Work.time"])


# Reg.unempl
# summary(frame.p[, "J100"])
# frame.p[,"Reg.unempl"] <- as.integer(!is.na(frame.p[,"J100"]) & frame.p[,"J100"] == 1)
# summary(frame.p[,"Reg.unempl"])


# bigtable(frame.p, "const")

# bigtable(pop, "strata")
# bigtable(pop, "reg")
# bigtable(pop, "raj")

pop[, .N, keyby = strata]
pop[, .N, keyby = reg]
pop[, .N, keyby = raj]


head(pop)

save(pop, file = "pop.Rdata")



######################
### Create pop.h ###
######################

setwd(dir.data)

load("pop.Rdata")

head(pop)

N <- nrow(pop)



# setwd(dir.data)
# 
# frame.p <- attach.big.matrix("frame.p.desc")
# nrow(frame.p)
# head(frame.p)

var.pop.h <- c("H_ID",
               "strata",
               "iec2010",
               "reg",
               "raj",
               "nov",
               "coord_x_p",
               "coord_y_p",
               "const")

# pop.h <- as.big.matrix(frame.p[frame.p[, "P_ID"] == 1, variables.pop.h],
#                          backingfile = "pop.h.bin",
#                          descriptorfile = "pop.h.desc")
# 
# head(pop.h)
# nrow(pop.h)


pop.h <- pop[P_ID == 1, var.pop.h, with = F]

head(pop.h)
test.df(pop.h)

# s <- sample(1:nrow(pop.h), 1000)
# 
# tmp <- bigtable(pop.h, c("iec2010", "raj"))
# head(tmp)
# 
# krasa <- as.factor(pop.h[, "raj"])
# head(krasa)
# 
# # plot(pop.h[, "coord_x_p"], pop.h[, "coord_y_p"], col = pop.h[, "raj"])
# plot(pop.h[s, "coord_x_p"], pop.h[s, "coord_y_p"], pch = 20, cex = 0.1, col = "red")
# plot(pop.h[pop.h[, "raj"] == 5, "coord_x_p"], pop.h[pop.h[, "raj"] == 5, "coord_y_p"])



#####################
### Checking PSUs ###
#####################

### PSU coord

head(pop.h)

M <- nrow(pop.h)
M


# PSU.coord <- aggregate(pop.h[, c("coord_x_p", "coord_y_p")], pop.h["iec2010"], mean)
# names(PSU.coord)
# names(PSU.coord)[2:3] <- c("coord_x_PSU", "coord_y_PSU")
# names(PSU.coord)
# head(PSU.coord)
# 
# nrow(PSU.coord)
# 
# plot(PSU.coord$coord_x_PSU, PSU.coord$coord_y_PSU, pch = 20, cex = .1)
# 
# PSU.coord$strata <- floor(PSU.coord$iec2010 / 1e5)
# head(PSU.coord)
# 
# table(PSU.coord$strata)
# nrow(PSU.coord)
# 
# ### Match PSU coord to HH
# 
# head(pop.h)
# head(PSU.coord)
# 
# pop.h <- merge(pop.h, PSU.coord)
# head(pop.h)
# 
# pop.h$dist <- sqrt((pop.h$coord_x_p - pop.h$coord_x_PSU)^2 + (pop.h$coord_y_p - pop.h$coord_y_PSU)^2)
# head(pop.h)
# 
# PSU.dist <- aggregate(pop.h[, "dist"], list(pop.h[, "iec2010"]), max)
# 
# head(PSU.dist)
# 
# names(PSU.dist) <- c("iec2010", "dist")
# 
# PSU <- merge(PSU.coord, PSU.dist)
# head(PSU)



# x <- cbind(c(rep(1,5), rep(2,5)), runif(10), runif(10))
# x

PSU.info <- function(x) {
  x <- as.data.table(x)
  setnames(x, c("clust", "x", "y"))
  setkey(x, clust)
  
  centre <- x[, lapply(.SD, mean), keyby = clust,
              .SDcols = c("x", "y")]
  names(centre)
  setnames(centre, c("x", "y"), c("x.c", "y.c"))
  names(centre)
  
  x <- merge(x, centre)
  head(x)
  
  x[, dist := sqrt((x - x.c)^2 + (y - y.c)^2)]
  head(x)
  
  max.dist <- x[, list(max.dist = max(dist)), keyby = clust]
  head(max.dist)
  
  centre <- merge(centre, max.dist)
  head(centre)
  
  return(centre)
}


PSU <- PSU.info(pop.h[, list(iec2010, coord_x_p, coord_y_p)])
PSU

setnames(PSU, names(PSU)[1], "iec2010")
setnames(PSU, names(PSU)[4], "max.dist")

head(PSU)
test.df(PSU)

PSU[, strata := floor(iec2010 / 1e5)]
head(PSU)
test.df(PSU)

head(pop.h)
head(PSU)

setkey(pop.h, iec2010)
key(PSU)

dist <- merge(pop.h[, c("H_ID", "iec2010", "strata",
                        "coord_x_p", "coord_y_p"), with = F],
              PSU[, c("iec2010", "x.c", "y.c"), with = F])

nrow(dist) == nrow(pop.h)

head(dist)
test.df(dist)

dist[, dist := sqrt((coord_x_p - x.c)^2 + (coord_y_p - y.c)^2)]
head(dist)
test.df(dist)


plot.circles(PSU[, c(2,3,4), with = F])
plot.circles(PSU[strata == 1, c(2,3,4), with = F])
plot.circles(PSU[strata == 2, c(2,3,4), with = F])
plot.circles(PSU[strata == 3, c(2,3,4), with = F])
plot.circles(PSU[strata == 4, c(2,3,4), with = F])



### Filtering of dwellings

head(dist)

hist(dist[strata == 1, dist])
hist(dist[strata == 1 & dist < 3e3, dist])

hist(dist[strata == 2, dist])
hist(dist[strata == 2 & dist < 3e3, dist])

hist(dist[strata == 3, dist])
hist(dist[strata == 3 & dist < 3e3, dist])

hist(dist[strata == 4, dist])
hist(dist[strata == 4 & dist < 20e3, dist])

lim <- c(5e3, 5e3, 5e3, 20e3)

t <- dist$dist < lim[dist$strata]
table(t)
table(dist$strata)
table(dist$strata, t)
prop.table(table(dist$strata, t), 1)


### H_ID to be kept

head(dist)
H_ID_keep <- dist$H_ID[t]
M - length(H_ID_keep)


### Test

head(pop.h)
PSU <- PSU.info(pop.h[H_ID %in% H_ID_keep,
                      c("iec2010", "coord_x_p", "coord_y_p"),
                      with = F])
head(PSU)
test.df(PSU)

PSU[, strata := floor(clust / 1e5)]
head(PSU)
test.df(PSU)

plot.circles(PSU[, c(2,3,4), with = F])
plot.circles(PSU[strata == 1, c(2,3,4), with = F])
plot.circles(PSU[strata == 2, c(2,3,4), with = F])
plot.circles(PSU[strata == 3, c(2,3,4), with = F])
plot.circles(PSU[strata == 4, c(2,3,4), with = F])


pop.h <- pop.h[H_ID %in% H_ID_keep]
M - nrow(pop.h)
M <- nrow(pop.h)
test.df(pop.h)

# Test
PSU <- PSU.info(pop.h[, c("iec2010", "coord_x_p", "coord_y_p"),
                      with = F])
PSU[, strata := floor(clust / 1e5)]
head(PSU)
test.df(PSU)

plot.circles(PSU[, c(2,3,4), with = F])
plot.circles(PSU[strata == 1, c(2,3,4), with = F])
plot.circles(PSU[strata == 2, c(2,3,4), with = F])
plot.circles(PSU[strata == 3, c(2,3,4), with = F])
plot.circles(PSU[strata == 4, c(2,3,4), with = F])


### Filtering persons

head(pop)
pop <- pop[H_ID %in% H_ID_keep]
N - nrow(pop)
N <- nrow(pop)

head(pop)
test.df(pop)


save(pop, file = "pop.Rdata")
save(pop.h, file = "pop.h.Rdata")



####################
### Interviewers ###
####################

setwd(dir.data)

load("pop.Rdata")
load("pop.h.Rdata")

N <- nrow(pop)
N
M <- nrow(pop.h)
M

head(pop)
head(pop.h)

test.df(pop)
test.df(pop.h)

pop.h[, .N, keyby = is.na(strata)]
pop.h[, .N, keyby = is.na(iec2010)]


frame.PSU <- pop.h[, .N, keyby = list(strata, iec2010)]
setnames(frame.PSU, c("strata", "iec2010", "size"))
head(frame.PSU)
test.df(frame.PSU)

M - sum(frame.PSU$size)

coord.PSU <- pop.h[, lapply(.SD, mean), keyby = iec2010,
                   .SDcols = c("coord_x_p", "coord_y_p")]
setnames(coord.PSU, names(coord.PSU)[2:3], c("x_PSU", "y_PSU"))
head(coord.PSU)
test.df(coord.PSU)


frame.PSU[, const := 1]

test.df(frame.PSU)
test.df(coord.PSU)

nrow(frame.PSU)
nrow(coord.PSU)

setkey(frame.PSU, iec2010)
setkey(coord.PSU, iec2010)

key(frame.PSU) == key(coord.PSU)

frame.PSU <- merge(frame.PSU, coord.PSU, all = T)
test.df(frame.PSU)

nrow(frame.PSU)

plot(frame.PSU$x_PSU, frame.PSU$y_PSU, pch = 20, cex=.5,
     col = frame.PSU$strata, asp = 1)

plot(frame.PSU[strata == 1, x_PSU],
     frame.PSU[strata == 1, y_PSU],
     pch = 20, cex=.5, asp = 1)

tmp <- pop.h[, .N, keyby = list(strata, iec2010, raj)]
setnames(tmp, c("strata", "iec2010", "raj", "size"))
names(tmp)
nrow(tmp)
test.df(tmp)

tmp <- tmp[order(iec2010, -size)]
tmp

tmp <- tmp[, head(raj, n = 1), keyby = iec2010]
nrow(tmp)
head(tmp)
setnames(tmp, c("iec2010", "raj"))
head(tmp)

key(frame.PSU)
key(tmp)

frame.PSU <- merge(frame.PSU, tmp, all = T)
test.df(frame.PSU)
nrow(frame.PSU)
sum(frame.PSU$size)


plot(frame.PSU$x_PSU, frame.PSU$y_PSU,
     pch = 20, cex=.5, asp = 1, col = as.factor(frame.PSU$raj))

frame.PSU[, .N, keyby = raj]

tab <- frame.PSU[, lapply(.SD, sum), keyby = raj,
                 .SDcols = c("const", "size")]
head(tab)
setnames(tab, names(tab)[1:2], c("raj", "count.iec"))
head(tab)
test.df(tab)
colSums(tab[2:3])


tab[, count.int := trunc(size / 20e3) + 1]
head(tab)
sum(tab$count.int)
tab[tab$count.int > 1, ]


#### Asignment of PSU to interviewers

frame.PSU[, int.ID := NULL]

head(frame.PSU)
head(tab)

# Test
R <- 1
set.seed(20120315)
tmp <- pam(frame.PSU[raj == R, list(x_PSU, y_PSU)], 11, stand=F)
plot(frame.PSU$x_PSU[frame.PSU$raj == R],
     frame.PSU$y_PSU[frame.PSU$raj == R],
     pch = 20, cex=.5, asp = 1,
     col = tmp$clustering)
points(tmp$medoids[,1], tmp$medoids[,2],
       pch = 16, col = "red", cex = 2)

head(frame.PSU)

int <- frame.PSU[, c("iec2010", "raj"), with = F]
int[, int.ID := as.numeric(NA)]
test.df(int)

a <- 0
for (R in tab$raj) {
  set.seed(R)
  tmp <- pam(frame.PSU[raj == R, list(x_PSU, y_PSU)],
             tab[raj == R, count.int], stand = F)
  int[raj == R, int.ID := tmp$clustering + a]
  a <- a + tab[raj == R, count.int]
}

test.df(int)
table(int$int.ID)
table(int$raj, int$int.ID)

test.df(frame.PSU)
test.df(int)

frame.PSU <- merge(frame.PSU, int,
                   by = intersect(names(frame.PSU), names(int)),
                   all = T)
test.df(frame.PSU)
nrow(frame.PSU)
sum(frame.PSU$size)

tab[count.int > 1, ]


test.df(frame.PSU)

frame.int <- frame.PSU[, lapply(.SD, mean), keyby = c("raj", "int.ID"),
                       .SDcols = c("x_PSU", "y_PSU")]
setnames(frame.int, names(frame.int)[3:4], c("x_int", "y_int"))
test.df(frame.int)

plot(frame.int$x_int, frame.int$y_int, pch = 20, asp = 1)


test.df(frame.PSU)
test.df(frame.int)

sum(frame.PSU$size)

save(frame.PSU, file = "frame.PSU.Rdata")
save(frame.int, file = "frame.int.Rdata")






#################### Add int.ID to pop and pop.h ####

setwd(dir.data)

load("frame.PSU.Rdata")
load("frame.int.Rdata")

test.df(frame.PSU)
test.df(frame.int)

load("pop.Rdata")
load("pop.h.Rdata")

test.df(pop)
test.df(pop.h)

tmp <- frame.PSU[, c("iec2010", "int.ID"), with = F]

setkey(pop, iec2010)
setkey(pop.h, iec2010)
setkey(tmp, iec2010)

nrow(pop)
pop <- merge(pop, tmp, all = T)
nrow(pop)

nrow(pop.h)
pop.h <- merge(pop.h, tmp, all = T)
nrow(pop.h)


# Sort

head(pop)
head(pop.h)

setkey(pop, strata, iec2010, H_ID, P_ID)
setkey(pop.h, strata, iec2010, H_ID)

test.df(pop)
test.df(pop.h)


# Add casenum

head(pop)
head(pop.h)

class(pop)
class(pop.h)

pop[, casenum := .I]
pop.h[, casenum := .I]


# Add strata3
pop[, strata3 := ifelse(strata == 4, 3, strata)]
pop.h[, strata3 := ifelse(strata == 4, 3, strata)]

# Coordinates
# head(pop)
# geo.coord <- pop[, c("coord_x_p", "coord_y_p"), with = F]
# head(geo.coord)
# 
# coordinates(geo.coord) <- c("coord_x_p", "coord_y_p")
# proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# head(as.data.frame(geo.coord))
# 
# geo.coord <- data.table(spTransform(geo.coord, CRS("+proj=latlon")))
# head(geo.coord)
# names(geo.coord) <- c("Lon", "Lat")
# 
# pop <- data.frame(pop, geo.coord["Lat"], geo.coord["Lon"])
# head(pop)
# tail(pop)

###

# head(pop.h)
# geo.coord <- pop.h[c("coord_x_p", "coord_y_p")]
# head(geo.coord)
# 
# coordinates(geo.coord) <- c("coord_x_p", "coord_y_p")
# proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# head(as.data.frame(geo.coord))
# 
# geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
# head(geo.coord)
# names(geo.coord) <- c("Lon", "Lat")
# 
# pop.h <- data.frame(pop.h, geo.coord["Lat"], geo.coord["Lon"])
# head(pop.h)
# tail(pop.h)

###

# head(frame.PSU)
# geo.coord <- frame.PSU[c("x_PSU", "y_PSU")]
# head(geo.coord)
# 
# coordinates(geo.coord) <- c("x_PSU", "y_PSU")
# proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# head(as.data.frame(geo.coord))
# 
# geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
# head(geo.coord)
# names(geo.coord) <- c("Lon", "Lat")
# 
# frame.PSU <- data.frame(frame.PSU, geo.coord["Lat"], geo.coord["Lon"])
# head(frame.PSU)
# tail(frame.PSU)

###

# head(frame.int)
# geo.coord <- frame.int[c("x_int", "y_int")]
# head(geo.coord)
# 
# coordinates(geo.coord) <- c("x_int", "y_int")
# proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# head(as.data.frame(geo.coord))
# 
# geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
# head(geo.coord)
# names(geo.coord) <- c("Lon", "Lat")
# 
# frame.int <- data.frame(frame.int, geo.coord["Lat"], geo.coord["Lon"])
# head(frame.int)
# tail(frame.int)

###

head(pop)
head(pop.h)

tail(pop)
tail(pop.h)

class(pop)
class(pop.h)


##############
### Saving ###
##############



frame.p <- pop
frame.h <- pop.h

nrow(frame.p)
nrow(frame.h)

test.df(frame.p)
test.df(frame.h)

save(frame.p, file = "frame.p.Rdata")
save(frame.h, file = "frame.h.Rdata")

save(frame.PSU, file = "frame.PSU.Rdata")
save(frame.int, file = "frame.int.Rdata")

### Bigmatrix

# head(frame.p.df)
# 
# N <- nrow(frame.p.df)
# N
# 
# frame.p <- big.matrix(N, ncol(frame.p.df),
#                       backingfile = "frame.p.bin",
#                       descriptorfile = "frame.p.desc",
#                       dimnames = list(NULL, colnames(frame.p.df)))
# 
# frame.p[,] <- as.matrix(frame.p.df)
# flush(frame.p)
# 
# head(frame.p)
# nrow(frame.p)
# 
# 
# head(frame.h.df)
# 
# M <- nrow(frame.h.df)
# M
# 
# frame.h <- big.matrix(M, ncol(frame.h.df),
#                       backingfile = "frame.h.bin",
#                       descriptorfile = "frame.h.desc",
#                       dimnames = list(NULL, colnames(frame.h.df)))
# 
# frame.h[,] <- as.matrix(frame.h.df)
# flush(frame.h)
# 
# head(frame.h)





#################
### Maps etc. ###
#################


setwd(dir.data)

load("frame.p.Rdata")
head(frame.p)
nrow(frame.p)

load("frame.h.Rdata")
head(frame.h)
nrow(frame.h)

ls()
head(frame.h)
nrow(frame.h)
test.df(frame.h)

frame.h.df[, coord_x_p := coord_x_p / 1e3]
frame.h.df[, coord_y_p := coord_y_p / 1e3]

head(frame.h)
nrow(frame.h)

frame.h[, strata.2 := 5 - strata]



setwd(dir.maps)

p <- ggplot(frame.h, aes(coord_x_p, coord_y_p)) +
  geom_point(alpha = 1/10, aes(colour = factor(strata.2))) +
  coord_equal(ratio = 1)

p.2 <- ggplot(frame.h, aes(coord_x_p, coord_y_p)) +
  geom_point(alpha = 1/10, aes(colour = factor(int.ID))) +
  coord_equal(ratio = 1)

p.Riga <- ggplot(frame.h[raj == 1], aes(coord_x_p, coord_y_p)) +
  geom_point(alpha = 1/10, aes(colour = factor(int.ID))) +
  coord_equal(ratio = 1)

png("Map.h.png", width = 1920/2, height = 1080/2)
p
dev.off()

png("Map.h.int.ID.png", width = 1920/2, height = 1080/2)
p.2
dev.off()

png("Map.h.int.ID.Riga.png", width = 1920/2, height = 1080/2)
p.Riga
dev.off()


###########
### TSP ###
###########

setwd(dir.data)

load("frame.h.Rdata")
ls()
head(frame.h)
nrow(frame.h)
test.df(frame.h)

min(table(frame.h.df$raj))

geo <- frame.h.df[iec2010 == 400030, c("coord_x_p", "coord_y_p"),
                  with = F]
head(geo)
nrow(geo)

# Distanču matrica
dist.matrix <- dist(geo)


## create a TSP
tsp <- TSP(dist.matrix)
tsp
## use some methods
n_of_cities(tsp)
labels(tsp)

image(tsp)
tour_length(tsp)

t <- solve_TSP(tsp)
t
str(t)
attr(t, "tour_length")

image(tsp)
image(tsp, order = t)

plot(geo, axes = TRUE)
points(geo, pch = 3, cex = 0.4, col = "red")
path_line <- SpatialLines(list(Lines(list(Line(geo[t,])),
                                     ID = "1")))
plot(path_line, add = TRUE, col = "black")
points(geo[c(head(t, 1), tail(t, 1)),], pch = 19, col = "black")


### Testing result

setwd(dir.data)

load("frame.p.Rdata")
head(frame.p)
test.df(frame.p)
nrow(frame.p)

load("frame.h.Rdata")
head(frame.h)
test.df(frame.h)
nrow(frame.h)

load("frame.PSU.Rdata")
test.df(frame.PSU)
nrow(frame.PSU)
sum(frame.PSU$size)

load("frame.int.Rdata")
test.df(frame.int)
nrow(frame.int)

############# Population data


### Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")



### Libs

require(bigmemory)
require(bigtabulate)

require(ggplot2)

require(TSP)
require(maps)
require(sp)
require(maptools)
require(rgdal)

require(plotrix)

require(cluster)
# require(flexclust)



# Files and Directories

# Original data (TXT)
file.data.in <- "~/DATA/LU/Source/Population.txt"


### Functions

### Function to draw circles

# # Matrix with x,y coordinates at the 1:2 columns, circle diameter in the 3rd col
# x <- cbind(c(1:10), sample(10,10), .5)
# x

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

setwd(dir.data.out)

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

pop <- read.delim(file.data.in)

# head(pop)
# class(pop)
# head(pop)

save(pop, file = "pop.Rdata")

# rm(pop)
# gc()


######################
### Create frame.p ###
######################

setwd(dir.data.out)

# pop.orig <- attach.big.matrix("pop.desc")

load("pop.Rdata")

head(pop)
N <- nrow(pop)
N

head(pop)
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

table(is.na(pop$coord_x_p)) / N * 100
table(is.na(pop$coord_y_p)) / N * 100


# Select only records with known geo coordinates

pop <- pop[!is.na(pop$coord_x_p) & !is.na(pop$coord_y_p), ]
nrow(pop)

N <- nrow(pop)


# Const
# frame.p[,"const"] <- 1L
pop$const <- 1L


# Strata
# frame.p[, "strata"] <- as.integer(floor(frame.p[, "iec2010"] / 1e5))
pop$strata <- as.integer(floor(pop$iec2010 / 1e5))


# Rajons
# frame.p[, "raj"] <- as.integer(floor(frame.p[, "nov"] / 1e2))
# bigtable(frame.p, ccols = "raj")
pop$raj <- as.integer(floor(pop$nov / 1e2))


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
bigtable(pop, "strata")
bigtable(pop, "reg")
bigtable(pop, "raj")


head(pop)

save(pop, file = "pop.Rdata")



######################
### Create pop.h ###
######################

setwd(dir.data.out)

load("pop.Rdata")

head(pop)

N <- nrow(pop)



# setwd(dir.data.out)
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


pop.h <- pop[pop$P_ID == 1, var.pop.h]

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

  x <- as.data.frame(x)
  names(x) <- c("clust", "x", "y")
  
  centre <- aggregate(x[c("x", "y")], x["clust"], mean)
  names(centre)
  names(centre)[2:3] <- c("x.c", "y.c")
  names(centre)
  
  x <- merge(x, centre)
  head(x)
  
  x$dist <- sqrt((x$x - x$x.c)^2 + (x$y - x$y.c)^2)
  head(x)
  
  max.dist <- aggregate(x["dist"], x["clust"], max)
  head(max.dist)
  
  centre <- merge(centre, max.dist)
  head(centre)
  
  centre

}


PSU <- PSU.info(pop.h[c("iec2010", "coord_x_p", "coord_y_p")])
names(PSU)[1] <- "iec2010"
names(PSU)[4] <- "max.dist"
head(PSU)
test.df(PSU)

PSU$strata <- floor(PSU$iec2010 / 1e5)
head(PSU)
test.df(PSU)

head(pop.h)
head(PSU)

dist <- merge(pop.h[c("H_ID", "iec2010", "strata", "coord_x_p", "coord_y_p")],
              PSU[c("iec2010", "x.c", "y.c")])

nrow(dist) == nrow(pop.h)

head(dist)
test.df(dist)

dist$dist <- sqrt((dist$coord_x_p - dist$x.c)^2 + (dist$coord_y_p - dist$y.c)^2)
head(dist)
test.df(dist)


plot.circles(PSU[, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 1, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 2, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 3, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 4, c(2,3,4)])



### Filtering of dwellings

head(dist)

hist(dist[dist$strata == 1, "dist"])
hist(dist[dist$strata == 1 & dist$dist < 3e3, "dist"])

hist(dist[dist$strata == 2, "dist"])
hist(dist[dist$strata == 2 & dist$dist < 3e3, "dist"])

hist(dist[dist$strata == 3, "dist"])
hist(dist[dist$strata == 3 & dist$dist < 3e3, "dist"])

hist(dist[dist$strata == 4, "dist"])
hist(dist[dist$strata == 4 & dist$dist < 20e3, "dist"])

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
PSU <- PSU.info(pop.h[pop.h$H_ID %in% H_ID_keep, c("iec2010", "coord_x_p", "coord_y_p")])
head(PSU)
test.df(PSU)

PSU$strata <- floor(PSU$clust / 1e5)
head(PSU)
test.df(PSU)

plot.circles(PSU[, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 1, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 2, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 3, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 4, c(2,3,4)])


pop.h <- pop.h[pop.h$H_ID %in% H_ID_keep, ]
M - nrow(pop.h)
M <- nrow(pop.h)
test.df(pop.h)

# Test
PSU <- PSU.info(pop.h[c("iec2010", "coord_x_p", "coord_y_p")])
PSU$strata <- floor(PSU$clust / 1e5)
head(PSU)
test.df(PSU)

plot.circles(PSU[, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 1, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 2, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 3, c(2,3,4)])
plot.circles(PSU[PSU[, "strata"] == 4, c(2,3,4)])


### Filtering persons

head(pop)
pop <- pop[pop$H_ID %in% H_ID_keep, ]
N - nrow(pop)
N <- nrow(pop)

head(pop)
test.df(pop)


save(pop, file = "pop.Rdata")
save(pop.h, file = "pop.h.Rdata")



####################
### Interviewers ###
####################

setwd(dir.data.out)

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

sum(is.na(pop.h[, "strata"]))
sum(is.na(pop.h[, "iec2010"]))

frame.PSU <- aggregate(rep(1, M), list(pop.h[, "strata"], pop.h[, "iec2010"]), sum)
names(frame.PSU) <- c("strata", "iec2010", "size")
head(frame.PSU)
test.df(frame.PSU)

M - sum(frame.PSU$size)

coord.PSU <- aggregate(pop.h[c("coord_x_p", "coord_y_p")], pop.h["iec2010"], mean)
names(coord.PSU)[2:3] <- c("x_PSU", "y_PSU")
head(coord.PSU)
test.df(coord.PSU)


frame.PSU$const <- 1

test.df(frame.PSU)
test.df(coord.PSU)

nrow(frame.PSU)
nrow(coord.PSU)

frame.PSU <- merge(frame.PSU, coord.PSU, all = T)
test.df(frame.PSU)

nrow(frame.PSU)

plot(frame.PSU$x_PSU, frame.PSU$y_PSU, pch = 20, cex=.5, col = frame.PSU$strata, asp = 1)

plot(frame.PSU$x_PSU[frame.PSU$strata == 1],
     frame.PSU$y_PSU[frame.PSU$strata == 1],
     pch = 20, cex=.5, asp = 1)

tmp <- aggregate(rep(1, M), list(pop.h[, "strata"], pop.h[, "iec2010"], pop.h[, "raj"]), sum)
names(tmp) <- c("strata", "iec2010", "raj", "size")
names(tmp)
nrow(tmp)
test.df(tmp)

tmp <- tmp[order(tmp$iec2010, -tmp$size), ]

tmp <- aggregate(tmp$raj, list(tmp$iec2010), head, n=1)
nrow(tmp)
head(tmp)
names(tmp) <- c("iec2010", "raj")
head(tmp)

frame.PSU <- merge(frame.PSU, tmp, all = T)
test.df(frame.PSU)
nrow(frame.PSU)
sum(frame.PSU$size)


plot(frame.PSU$x_PSU, frame.PSU$y_PSU, pch = 20, cex=.5, asp = 1, col = as.factor(frame.PSU$raj))

table(frame.PSU$raj)

tab <- aggregate(frame.PSU[, c("const", "size")], list(frame.PSU$raj), sum)
head(tab)
names(tab)[1:2] <- c("raj", "count.iec")
head(tab)
test.df(tab)
colSums(tab[2:3])


tab$count.int <- trunc(tab$size / 20e3) + 1
head(tab)
sum(tab$count.int)
tab[tab$count.int > 1, ]

#### Asignment of PSU to interviewers

frame.PSU$int.ID <- NULL

head(frame.PSU)
head(tab)

# Test
R <- 1
set.seed(20120315)
tmp <- pam(frame.PSU[frame.PSU$raj == R, c("x_PSU", "y_PSU")], 11, stand=F)
plot(frame.PSU$x_PSU[frame.PSU$raj == R],
     frame.PSU$y_PSU[frame.PSU$raj == R],
     pch = 20, cex=.5, asp = 1,
     col = tmp$clustering)
points(tmp$medoids[,1], tmp$medoids[,2], pch = 16, col = "red", cex = 2)

head(frame.PSU)

int <- frame.PSU[c("iec2010", "raj")]
int$int.ID <- NA
test.df(int)

a <- 0
for (R in tab$raj) {
  set.seed(R)
  tmp <- pam(frame.PSU[frame.PSU$raj == R, c("x_PSU", "y_PSU")],
             tab$count.int[tab$raj == R], stand = F)
  int[int$raj == R, "int.ID"] <- tmp$clustering + a
  a <- a + tab$count.int[tab$raj == R]
}

test.df(int)
table(int$int.ID)
table(int$raj, int$int.ID)

test.df(frame.PSU)
test.df(int)

frame.PSU <- merge(frame.PSU, int, all = T)
test.df(frame.PSU)
nrow(frame.PSU)
sum(frame.PSU$size)

tab[tab$count.int > 1,]

# # Test
# R <- 80
# x <- frame.PSU[frame.PSU$raj == R, c("x_PSU", "y_PSU", "int.ID")]
# plot(x$x_PSU, x$y_PSU, pch = 20, cex=.5, col = x$int.ID, asp = 1)


test.df(frame.PSU)

frame.int <- aggregate(frame.PSU[c("x_PSU", "y_PSU")], frame.PSU[c("raj", "int.ID")], mean)
names(frame.int)[3:4] <- c("x_int", "y_int")
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

tmp <- frame.PSU[c("iec2010", "int.ID")]

nrow(pop)
pop <- merge(pop, tmp, all = T)
nrow(pop)

nrow(pop.h)
pop.h <- merge(pop.h, tmp, all = T)
nrow(pop.h)


# Sort

head(pop)
head(pop.h)

pop <- pop[order(pop$strata, pop$iec2010, pop$H_ID, pop$P_ID), ]
pop.h <- pop.h[order(pop.h$strata, pop.h$iec2010, pop.h$H_ID), ]

test.df(pop)
test.df(pop.h)


# Add casenum

head(pop)
head(pop.h)

class(pop)
class(pop.h)

pop <- data.frame(casenum = 1:nrow(pop), pop)
pop.h <- data.frame(casenum = 1:nrow(pop.h), pop.h)

rownames(pop) <- NULL
rownames(pop.h) <- NULL


# Add strata3
pop$strata3 <- ifelse(pop$strata == 4, 3, pop$strata)
pop.h$strata3 <- ifelse(pop.h$strata == 4, 3, pop.h$strata)


# Coordinates

head(pop)
geo.coord <- pop[c("coord_x_p", "coord_y_p")]
head(geo.coord)

coordinates(geo.coord) <- c("coord_x_p", "coord_y_p")
proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
head(as.data.frame(geo.coord))

geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
head(geo.coord)
names(geo.coord) <- c("Lon", "Lat")

pop <- data.frame(pop, geo.coord["Lat"], geo.coord["Lon"])
head(pop)
tail(pop)

###

head(pop.h)
geo.coord <- pop.h[c("coord_x_p", "coord_y_p")]
head(geo.coord)

coordinates(geo.coord) <- c("coord_x_p", "coord_y_p")
proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
head(as.data.frame(geo.coord))

geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
head(geo.coord)
names(geo.coord) <- c("Lon", "Lat")

pop.h <- data.frame(pop.h, geo.coord["Lat"], geo.coord["Lon"])
head(pop.h)
tail(pop.h)

###

head(frame.PSU)
geo.coord <- frame.PSU[c("x_PSU", "y_PSU")]
head(geo.coord)

coordinates(geo.coord) <- c("x_PSU", "y_PSU")
proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
head(as.data.frame(geo.coord))

geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
head(geo.coord)
names(geo.coord) <- c("Lon", "Lat")

frame.PSU <- data.frame(frame.PSU, geo.coord["Lat"], geo.coord["Lon"])
head(frame.PSU)
tail(frame.PSU)

###

head(frame.int)
geo.coord <- frame.int[c("x_int", "y_int")]
head(geo.coord)

coordinates(geo.coord) <- c("x_int", "y_int")
proj4string(geo.coord) <- CRS("+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
head(as.data.frame(geo.coord))

geo.coord <- data.frame(spTransform(geo.coord, CRS("+proj=latlon")))
head(geo.coord)
names(geo.coord) <- c("Lon", "Lat")

frame.int <- data.frame(frame.int, geo.coord["Lat"], geo.coord["Lon"])
head(frame.int)
tail(frame.int)

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



frame.p.df <- pop
frame.h.df <- pop.h

nrow(frame.p.df)
nrow(frame.h.df)

test.df(frame.p.df)
test.df(frame.h.df)

save(frame.p.df, file = "frame.p.Rdata")
save(frame.h.df, file = "frame.h.Rdata")

save(frame.PSU, file = "frame.PSU.Rdata")
save(frame.int, file = "frame.int.Rdata")

### Bigmatrix

head(frame.p.df)

N <- nrow(frame.p.df)
N

frame.p <- big.matrix(N, ncol(frame.p.df),
                      backingfile = "frame.p.bin",
                      descriptorfile = "frame.p.desc",
                      dimnames = list(NULL, colnames(frame.p.df)))

frame.p[,] <- as.matrix(frame.p.df)
flush(frame.p)

head(frame.p)
nrow(frame.p)


head(frame.h.df)

M <- nrow(frame.h.df)
M

frame.h <- big.matrix(M, ncol(frame.h.df),
                      backingfile = "frame.h.bin",
                      descriptorfile = "frame.h.desc",
                      dimnames = list(NULL, colnames(frame.h.df)))

frame.h[,] <- as.matrix(frame.h.df)
flush(frame.h)

head(frame.h)





#################
### Maps etc. ###
#################


setwd(dir.data.out)

frame.p <- attach.big.matrix("frame.p.desc")
head(frame.p)
nrow(frame.p)

frame.h <- attach.big.matrix("frame.h.desc")
head(frame.h)
nrow(frame.h)

load("frame.h.Rdata")
ls()
head(frame.h.df)
nrow(frame.h.df)

frame.h.df <- frame.h.df[!is.na(frame.h.df$coord_x_p) & !is.na(frame.h.df$coord_y_p), ]
head(frame.h.df)
nrow(frame.h.df)

frame.h.df$coord_x_p <- frame.h.df$coord_x_p / 1e3
frame.h.df$coord_y_p <- frame.h.df$coord_y_p / 1e3

# frame.h.df <- frame.h.df[sample(1:nrow(frame.h.df), 100), ]
head(frame.h.df)
nrow(frame.h.df)

# frame.h.df <- frame.h.df[order(frame.h.df$strata, decreasing = T), ]
head(frame.h.df)
nrow(frame.h.df)

frame.h.df$strata.2 <- 5-frame.h.df$strata



setwd(dir.maps)


p <- ggplot(frame.h.df, aes(coord_x_p, coord_y_p)) +
  geom_point(alpha = 1/10, aes(colour = factor(strata.2))) +
  coord_equal(ratio = 1) +
  opts(legend.position="none")

p.2 <- ggplot(frame.h.df, aes(coord_x_p, coord_y_p)) +
  geom_point(alpha = 1/10, aes(colour = factor(int.ID))) +
  coord_equal(ratio = 1) +
  opts(legend.position="none")

p.Riga <- ggplot(frame.h.df[frame.h.df$raj == 1, ], aes(coord_x_p, coord_y_p)) +
  geom_point(alpha = 1/10, aes(colour = factor(int.ID))) +
  coord_equal(ratio = 1) +
  opts(legend.position="none")

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

setwd(dir.data.out)

load("frame.h.Rdata")
ls()
head(frame.h.df)
nrow(frame.h.df)

frame.h.df <- frame.h.df[!is.na(frame.h.df$coord_x_p) & !is.na(frame.h.df$coord_y_p), ]
head(frame.h.df)
nrow(frame.h.df)

min(table(frame.h.df$raj))

geo <- frame.h.df[frame.h.df$iec2010 == 400030, c("coord_x_p", "coord_y_p")]
# geo <- frame.h.df[frame.h.df$raj == 98, c("coord_x_p", "coord_y_p")]
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
path_line <- SpatialLines(list(Lines(list(Line(geo[t,])), ID = "1")))
plot(path_line, add = TRUE, col = "black")
points(geo[c(head(t, 1), tail(t, 1)),], pch = 19, col = "black")


### Testing result

setwd(dir.data.out)

load("frame.p.Rdata")
frame.p <- attach.big.matrix("frame.p.desc")
head(frame.p)
test.df(frame.p.df)
nrow(frame.p)
nrow(frame.p.df)

load("frame.h.Rdata")
frame.h <- attach.big.matrix("frame.h.desc")
head(frame.h)
test.df(frame.h.df)
nrow(frame.h)
nrow(frame.h.df)

load("frame.PSU.Rdata")
test.df(frame.PSU)
nrow(frame.PSU)
sum(frame.PSU$size)

load("frame.int.Rdata")
test.df(frame.int)
nrow(frame.int)


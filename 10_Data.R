############# Population data

### Libs

require(bigmemory)
require(bigtabulate)

require(ggplot2)

library("TSP")
library("maps")
library("sp")
library("maptools")



# Files and Directories

# Original data (TXT)
# file.data.in <- "C:/Users/Martins Liberts/Documents/DATA/LU/Source/Population.txt"
file.data.in <- "~/DATA/LU/Source/Population.txt"
# file.data.in <- "C:/Users/djhurio/DATA/LU/Source/Population.txt"

# Directory for data files
# dir.data.out <- "C:/Users/Martins Liberts/Documents/DATA/LU/Work"
dir.data.out <- "~/DATA/LU/Work"
# dir.data.out <- "C:/Users/djhurio/DATA/LU/Work"

dir.maps <- "~/Dropbox/LU/Darbs/Simulation/050_Simulation/Maps"
# dir.maps <- "C:/Users/djhurio/Dropbox/LU/Darbs/Simulation/050_Simulation/Maps"



### Read data from source

setwd(dir.data.out)

# pop <- read.big.matrix(file.data.in,
#  header = TRUE,
#  type = "integer",
#  sep = "\t",
#  backingfile = "pop.bin",
#  descriptor = "pop.desc")

pop <- as.big.matrix(as.matrix(read.delim(file.data.in)), type = "integer", backingfile = "pop.bin", descriptorfile = "pop.desc")
head(pop)

class(pop[1,1])

rm(pop)


gc()


### Create frame.p

setwd(dir.data.out)

pop.orig <- attach.big.matrix("pop.desc")

head(pop.orig)
tail(pop.orig)

dim(pop.orig)

N <- nrow(pop.orig)

frame.p <- big.matrix(N,
 ncol(pop.orig) + 11,
 type = "integer",
 backingfile = "frame.p.0.bin",
 descriptorfile = "frame.p.0.desc",
 dimnames = list(NULL, c(colnames(pop.orig), "const", "strata", "raj", "int.ID", "All", "Empl", "Unempl", "Inact", "Act", "Work.time", "Reg.unempl")))

nrow(frame.p)

class(pop.orig)
class(frame.p)

frame.p[, colnames(pop.orig)] <- pop.orig[order(pop.orig[,"iec2010"], pop.orig[,"H_ID"], pop.orig[,"P_ID"]), ]

# Pagaidām izdzēšs tos ierakstus, kur nav zināmas geo koordinātas
frame.p <- as.big.matrix(frame.p[!is.na(frame.p[, "coord_x_p"]) & !is.na(frame.p[, "coord_y_p"]), ],
  backingfile = "frame.p.bin",
  descriptorfile = "frame.p.desc")
  
nrow(frame.p)

head(frame.p)
tail(frame.p)

nrow(frame.p)

# Const
frame.p[,"const"] <- 1L

# Strata
frame.p[, "strata"] <- as.integer(floor(frame.p[, "iec2010"] / 1e5))

# Rajons
frame.p[, "raj"] <- as.integer(floor(frame.p[, "nov"] / 1e2))
bigtable(frame.p, ccols = "raj")

# Intervētāja ID
# head(frame.p)
# frame.p[, "int.ID"] <- frame.p[, "raj"]


# Study variables
frame.p[,"All"] <- 1L
frame.p[,"Empl"] <- as.integer(frame.p[,"eka"] == 1)
frame.p[,"Unempl"] <- as.integer(frame.p[,"eka"] == 2)
frame.p[,"Inact"] <- as.integer(frame.p[,"eka"] == 3)
frame.p[,"Act"] <- as.integer(frame.p[,"eka"] %in% c(1,2))

frame.p[,"Work.time"] <- ifelse(frame.p[,"eka"] == 1, as.integer(frame.p[,"E59"]), 0L)

# Missing values imputed by 0
frame.p[,"Work.time"][is.na(frame.p[,"Work.time"]) | frame.p[,"Work.time"] %in% c(998, 999)] <- 0L
summary(frame.p[,"Work.time"])

summary(frame.p[, "J100"])
frame.p[,"Reg.unempl"] <- as.integer(!is.na(frame.p[,"J100"]) & frame.p[,"J100"] == 1)
summary(frame.p[,"Reg.unempl"])


head(frame.p)
tail(frame.p)

bigtable(frame.p, "const")
bigtable(frame.p, "strata")
bigtable(frame.p, "reg")
bigtable(frame.p, "raj")




### Create frame.h

setwd(dir.data.out)

frame.p <- attach.big.matrix("frame.p.desc")
nrow(frame.p)
head(frame.p)

variables.frame.h <- c("H_ID", "strata", "iec2010", "reg", "raj", "int.ID", "nov", "coord_x_p", "coord_y_p", "const")

frame.h <- as.big.matrix(frame.p[frame.p[, "P_ID"] == 1, variables.frame.h],
  backingfile = "frame.h.bin",
  descriptorfile = "frame.h.desc")

head(frame.h)
nrow(frame.h)

tmp <- bigtable(frame.h, c("iec2010", "raj"))
head(tmp)
 



### Create frame.PSU

setwd(dir.data.out)

frame.h <- attach.big.matrix("frame.h.desc")
head(frame.h)

M <- nrow(frame.h)

frame.PSU <- aggregate(rep(1, M), list(frame.h[, "strata"], frame.h[, "iec2010"]), sum)
class(frame.PSU)
head(frame.PSU)

names(frame.PSU) <- c("strata", "iec2010", "size")
names(frame.PSU)

frame.PSU$const <- 1
head(frame.PSU)

nrow(frame.PSU)

tmp <- aggregate(rep(1, M), list(frame.h[, "strata"], frame.h[, "iec2010"], frame.h[, "raj"]), sum)
names(tmp) <- c("strata", "iec2010", "raj", "size")
names(tmp)
nrow(tmp)
head(tmp)

tmp <- tmp[order(tmp$iec2010, -tmp$size), ]

tmp <- aggregate(tmp$raj, list(tmp$iec2010), head, n=1)
nrow(tmp)
head(tmp)
names(tmp) <- c("iec2010", "raj")
head(tmp)

frame.PSU <- merge(frame.PSU, tmp)
head(frame.PSU)
nrow(frame.PSU)

table(frame.PSU$raj)

tab <- aggregate(frame.PSU[, c("const", "size")], list(frame.PSU$raj), sum)
class(tab)
head(tab)
names(tab)[1:2] <- c("raj", "count.iec")
head(tab)

tab$count.int <- trunc(tab$size / 25e3) + 1
head(tab)
sum(tab$count.int)
tab

tab$count.iec.per.int <- trunc(tab$count.iec / tab$count.int)+1
head(tab)
sum(tab$count.int * tab$count.iec.per.int)

tab$count.iec <- NULL
tab$size <- NULL
tab$count.int <- NULL
head(tab)


head(frame.PSU)
frame.PSU <- merge(frame.PSU, tab)
head(frame.PSU)

# frame.PSU$id <- 1:nrow(frame.PSU)
# head(frame.PSU)
# tail(frame.PSU)
# frame.PSU$id <- NULL

frame.PSU <- frame.PSU[order(frame.PSU$raj, frame.PSU$iec2010), ]
head(frame.PSU)
tail(frame.PSU)

frame.PSU$num <- NA
frame.PSU$num[1] <- 1
for (i in 2:nrow(frame.PSU)) ifelse(frame.PSU$raj[i] == frame.PSU$raj[i-1],
  frame.PSU$num[i] <- frame.PSU$num[i-1] + 1, frame.PSU$num[i] <- 1)


head(frame.PSU)
# frame.PSU[frame.PSU$num == 1, ]

frame.PSU$tmp <- trunc((frame.PSU$num - 1) / frame.PSU$count.iec.per.int) + 1
head(frame.PSU)
max(frame.PSU$tmp)

# trunc(log10(134234234222))+1

frame.PSU$int.ID <- frame.PSU$raj * 10 ^ (trunc(log10(max(frame.PSU$tmp)))+1) + frame.PSU$tmp
head(frame.PSU)
tail(frame.PSU)


a <- min((1:nrow(frame.PSU))[frame.PSU$raj == 5])
frame.PSU[(a-5):(a+5), ]

a <- min((1:nrow(frame.PSU))[frame.PSU$raj == 42])
frame.PSU[(a-5):(a+5), ]


# frame.PSU[frame.PSU$size < 20, ]
# Atstāj tikai pietiekami lielus PSU !!!
# frame.PSU <- frame.PSU[frame.PSU$size >= 20, ]

dub <- as.numeric(names(table(frame.PSU$iec2010)[table(frame.PSU$iec2010)>1]))
tab <- frame.PSU[frame.PSU$iec2010 %in% dub, ]
tab[order(tab$iec2010, -tab$size), ]

nrow(frame.PSU)
table(frame.PSU$strata)

head(frame.PSU)

frame.PSU$count.iec.per.int <- NULL
frame.PSU$num <- NULL
frame.PSU$tmp <- NULL

head(frame.PSU)

frame.PSU <- frame.PSU[order(frame.PSU$iec2010), ]

save(frame.PSU, file = "frame.PSU.Rdata")






#################### Add int.ID to frame.p and frame.h

setwd(dir.data.out)

load("frame.PSU.Rdata")
head(frame.PSU)


### frame.p

frame.p <- attach.big.matrix("frame.p.desc")
nrow(frame.p)
head(frame.p)



tmp1 <- match(frame.p[, "iec2010"], frame.PSU$iec2010)

head(tmp1)
tail(tmp1)
length(tmp1)

head(frame.PSU$int.ID[tmp1])
tail(frame.PSU$int.ID[tmp1])

frame.p[, "int.ID"] <- as.integer(frame.PSU$int.ID[tmp1])
head(frame.p)
tail(frame.p)


# As data.frame
frame.p.df <- data.frame(frame.p[,])
class(frame.p.df)
head(frame.p.df)
save(frame.p.df, file = "frame.p.Rdata")

rm(frame.p.df)




### frame.h

frame.h <- attach.big.matrix("frame.h.desc")
nrow(frame.h)
head(frame.h)

tmp1 <- match(frame.h[, "iec2010"], frame.PSU$iec2010)
head(tmp1)
tail(tmp1)
length(tmp1)

head(frame.PSU$int.ID[tmp1])
tail(frame.PSU$int.ID[tmp1])

frame.h[, "int.ID"] <- as.integer(frame.PSU$int.ID[tmp1])

head(frame.h)
tail(frame.h)


# As data.frame
frame.h.df <- data.frame(frame.h[,])
class(frame.h.df)
head(frame.h.df)
save(frame.h.df, file = "frame.h.Rdata")

rm(frame.h.df)





### Maps etc.

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



# TSP

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


### END ###

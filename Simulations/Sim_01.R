############# Simulation of Cluster Sample, Two stage sampling

### Ver01
# Initial version


### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


### Options
l <- "home"

# options(digits = 3)
seed_value <- 14012011


### Libs
require(bigmemory)
require(bigtabulate)
# require(biganalytics)


### Workdir

loc1 <- c(loc = "home",
 dir.data = "~/DATA/LU/Work",
 dir.work = "~/temp/sim",
 dir_proc = paste(projwd, "Procedures", sep="/"))

loc2 <- c(loc = "csb",
 dir.data = "C:/DATA/LU/Results",
 dir.work = "C:/Documents and Settings/MLiberts/My Documents/My Dropbox/LU/Darbs/Simulation/050_Simulation/30_Simulation/SRS/Results",
 dir.proc = "C:/Documents and Settings/MLiberts/My Documents/My Dropbox/LU/Darbs/Simulation/050_Simulation/20_Procedures")

loc3 <- c(loc = "lenovo",
 dir.data = "C:/Users/djhurio/DATA/LU/Work",
 dir.work = "C:/Users/djhurio/temp/sim",
 dir.proc = "C:/Users/djhurio/Dropbox/LU/Darbs/Simulation/050_Simulation/20_Procedures")

loc <- as.matrix(rbind(loc1, loc2, loc3))

dir.data <- loc[loc[, "loc"] == l, 2]
dir.work <- loc[loc[, "loc"] == l, 3]
dir.proc <- loc[loc[, "loc"] == l, 4]



### Procedures

setwd(dir.proc)

source("Define_SamplingSRS.R")
source("Define_SamplingSRSWeek.R")

source("Define_SamplingCluster.R")
source("Define_SamplingClusterWeek.R")

source("Define_SamplingClusterStr.R")
source("Define_SamplingTwoStage.R")

source("Define_Estimation.R")
source("Define_Trip.R")
source("Define_Simulation.R")


### Maps

setwd(projwd)

load("LV_maps.Rdata")


### DATA

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")
load("frame.PSU.Rdata")

head(frame.p)
head(frame.h)
head(frame.PSU)
min(frame.PSU$size)



##################
### Simulation ###
##################

setwd(dir.work)

### Parameters

head(frame.p)

param <- rbind(c("sum","All",NA),
  c("sum","Empl",NA),
  c("sum","Unempl",NA),
  c("sum","Inact",NA),
  c("sum","Act",NA),
  c("sum","Work.time",NA),
  c("sum","Reg.unempl",NA))

param

save(param, file = "param.Rdata")

tv <- data.frame(Estimation(frame.p, 1, param))
tv



### Simulation designs

N <- nrow(frame.p)

M <- nrow(frame.h)
M.h <- as.vector(bigtable(frame.h, "strata"))

max.PSU.size.h <- aggregate(frame.PSU["size"], frame.PSU["strata"], max)[, 2]

delta <- 1 / max.PSU.size.h
delta

weeks <- 1

sampl.des.par <- data.frame(s = 1:4,
  A = 8,
  B = c(1, 2, 2, 2),
  W = 13,
  d = 0,
  Q = 0,
  w = weeks,
  M = M.h,
  m = c(10, 7, 8, 9))
sampl.des.par

m <- sum(sampl.des.par$A *sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
m

n <- round(m * N / M)
n

m.h <- round(m * M.h / M)  # Proportional sample allocation
m.h
sum(m.h)



# design.1 <- expression(SamplingSRS(frame.p, n, ".dw"))
# design.2 <- expression(SamplingCluster(frame.p, frame.h, m, ".dw", "H_ID"))
# design.3 <- expression(SamplingClusterStr(frame.p, frame.h, m.h, ".dw", "H_ID", "strata"))

design.1 <- expression(SamplingSRSWeek(frame.p, n, ".dw", weeks = weeks))
design.2 <- expression(SamplingClusterWeek(frame.p, frame.h, m, ".dw", "H_ID", weeks = weeks))
design.4 <- expression(SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week",
 "iec2010", "H_ID", "strata", sampl.des.par))


### Testing sampling and some plots

# pdf("Trip_weeks1.pdf", paper="a4r")
# pdf("Trip_weeks13.pdf", paper="a4r")

pdf("Trip_weeks1_SRS.pdf")
# pdf("Trip_weeks13_SRS.pdf")
s <- eval(design.1)
head(s)
table(s$.week)
s$int.week <- s$int.ID * 100 + s$.week
head(s)
t1 <- Trip(s, "coord_x_p", "coord_y_p", "int.week", T, map=LV2_LKS92)
title("SRS Individuals")
mtext(paste("Sample size = ", n, "; Trip = ", round(t1), " (km)", sep=""))
dev.off()

pdf("Trip_weeks1_Clust.pdf")
# pdf("Trip_weeks13_Clust.pdf")
s <- eval(design.2)
head(s)
s$int.week <- s$int.ID * 100 + s$.week
head(s)
t2 <- Trip(s, "coord_x_p", "coord_y_p", "int.week", T, map=LV2_LKS92)
title("Cluster Sampling of persons")
mtext(paste("Sample size = ", m, "; Trip = ", round(t2), " (km)", sep=""))
dev.off()

pdf("Trip_weeks1_TwoStage.pdf")
# pdf("Trip_weeks13_TwoStage.pdf")
s <- eval(design.4)
head(s)
s$int.week <- s$int.ID * 100 + s$.week
head(s)
t4 <- Trip(s, "coord_x_p", "coord_y_p", "int.week", T, map=LV2_LKS92)
title("Two Stage Sampling of Dwellings")
mtext(paste("Sample size = ", m, "; Trip = ", round(t4), " (km)", sep=""))
dev.off()

c(t1, t2, t4)

# Test

I <- 5

set.seed(seed_value)
t.time.sim1 <- Simulation(design.1, param, I, "t.SRS", T, T) / I
t.time.sim1

set.seed(seed_value)
t.time.sim2 <- Simulation(design.2, param, I, "t.Clust", T, T) / I
t.time.sim2

set.seed(seed_value)
t.time.sim4 <- Simulation(design.4, param, I, "t.TwoStage", T, T) / I
t.time.sim4

c(t.time.sim1, t.time.sim2, t.time.sim4)
sum(t.time.sim1, t.time.sim2, t.time.sim4)


# Estimation of processing time
I <- 20

set.seed(seed_value)
t.time.sim1 <- Simulation(design.1, param, I, "t.SRS", T, T) / I
t.time.sim1

set.seed(seed_value)
t.time.sim2 <- Simulation(design.2, param, I, "t.Clust", T, T) / I
t.time.sim2

set.seed(seed_value)
t.time.sim4 <- Simulation(design.4, param, I, "t.TwoStage", T, T) / I
t.time.sim4


# warnings()

t.time.sim <- sum(t.time.sim1, t.time.sim2, t.time.sim4)
c(t.time.sim1, t.time.sim2, t.time.sim4)
t.time.sim


# load("SRS.Rdata")
# load("Clust.Rdata")
# load("ClustStr.Rdata")

# head(SRS)
# head(Clust)
# head(ClustStr)

# nrow(rbind(SRS, Clust, ClustStr))

# Real sim

time.available <- 10  ### in hours

# Rounding base
base <- 10

I <- floor(time.available * 3600 / t.time.sim / base) * base
I

#I <- 10000
#I

set.seed(seed_value)
time.sim1 <- Simulation(design.1, param, I, "SRS", T, F) / I
time.sim1

set.seed(seed_value)
time.sim2 <- Simulation(design.2, param, I, "Clust", T, F) / I
time.sim2

# set.seed(seed_value)
# time.sim3 <- Simulation(design.3, param, I, "ClustStr", T, F) / I
# time.sim3

set.seed(seed_value)
time.sim4 <- Simulation(design.4, param, I, "TwoStage", T, F) / I
time.sim4

time.sim <- sum(time.sim1, time.sim2, time.sim4)
c(time.sim1, time.sim2, time.sim4)
time.sim



### END ###


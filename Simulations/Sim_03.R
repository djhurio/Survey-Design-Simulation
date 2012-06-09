############# Simulation of Cluster Sample, Two stage sampling

### Simulation of survey costs
### SRS of individuals

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
require(ggplot2)
# require(biganalytics)


### Workdir

loc1 <- c(loc = "home",
 dir.data = "~/DATA/LU/Work",
 dir.work = "~/Dropbox/DATA/sim",
 dir.proc = paste(projwd, "Procedures", sep="/"))

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

# source("Define_Estimation.R")
source("Define_Trip.R")
source("Define_Cost.R")

source("Define_Simulation_v2.R")


### Maps

# setwd(projwd)
# load("LV_maps.Rdata")


### DATA

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")
load("frame.PSU.Rdata")
load("frame.int.Rdata")

head(frame.p)
head(frame.h)
test.df(frame.PSU)
test.df(frame.int)

nrow(frame.h)
sum(frame.PSU$size)
nrow(frame.h) == sum(frame.PSU$size)


head(frame.int)
min(frame.PSU$size)



##################
### Simulation ###
##################

setwd(dir.work)

# ### Parameters
# 
# head(frame.p)
# 
# param <- rbind(c("sum","All",NA),
#   c("sum","Empl",NA),
#   c("sum","Unempl",NA),
#   c("sum","Inact",NA),
#   c("sum","Act",NA),
#   c("sum","Work.time",NA),
#   c("sum","Reg.unempl",NA))
# 
# param
# 
# save(param, file = "param.Rdata")
# 
# tv <- data.frame(Estimation(frame.p, 1, param))
# tv



### Simulation designs

N <- nrow(frame.p)
N

M <- nrow(frame.h)
M

M.h <- as.vector(bigtable(frame.h, "strata"))
M.h
sum(M.h)
sum(M.h) == M

max.PSU.size.h <- aggregate(frame.PSU["size"], frame.PSU["strata"], max)[, 2]

delta <- 1 / max.PSU.size.h
delta

weeks <- 13

sampl.des.par <- data.frame(s = 1:4,
  A = 8,
  B = c(1, 2, 2, 2),
  W = 13,
  d = delta,
  Q = 0,
  w = weeks,
  M = M.h,
  m = c(10, 7, 8, 9))

sampl.des.par

m <- sum(sampl.des.par$A * sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
m

n <- round(m * N / M)
n

m.h <- round(m * M.h / M)  # Proportional sample allocation
m.h
sum(m.h)
sum(m.h) == m



design.1 <- expression(SamplingSRSWeek(frame.p, n, ".dw", weeks = weeks))

design.2 <- expression(SamplingClusterWeek(frame.p, frame.h, m, ".dw", "H_ID",
                                           weeks = weeks))

design.4 <- expression(SamplingTwoStage(frame.PSU, frame.h, frame.p,
                                        ".dw1", ".dw2", ".dw", ".week",
                                        "iec2010", "H_ID", "strata",
                                        sampl.des.par))

# head(frame.PSU)
# head(frame.h)
# head(frame.p)

### Testing sampling and some plots

s <- eval(design.1)
test.df(s)
test.df(frame.int)
table(s$.week)
t1 <- vTrip.fast.3(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                   frame.int[c("int.ID", "x_int", "y_int")])
t1 / 1e3

s <- eval(design.2)
test.df(s)
test.df(frame.int)
table(s$.week)
t1 <- vTrip.fast.3(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                   frame.int[c("int.ID", "x_int", "y_int")])
t1 / 1e3

# undebug(SamplingTwoStage)

set.seed(1)
s <- eval(design.4)
test.df(s)
test.df(frame.int)
table(s$.week)
t1 <- vTrip.fast.3(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                   frame.int[c("int.ID", "x_int", "y_int")])
t1 / 1e3


set.seed(2)
s <- eval(design.4)
test.df(s)
test.df(frame.int)
table(s$.week)
t1 <- vTrip.fast.3(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                   frame.int[c("int.ID", "x_int", "y_int")])
t1 / 1e3



### Proc

run <- function(design) {
  s <- eval(design)
  #print(test.df(s))
  t <- vTrip.fast.3(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  return(t / 1e3)
}

run(design.1)
run(design.2)

# debugonce(run)
set.seed(1)
run(design.4)

set.seed(2)
run(design.4)

# Test

setwd(dir.work)

I <- 5

# set.seed(seed_value)
t.time.sim1 <- Sim(expression(run(design.1)), I, "t.SRS", T, T) / I
t.time.sim1

# set.seed(seed_value)
t.time.sim2 <- Sim(expression(run(design.2)), I, "t.Clust", T, T) / I
t.time.sim2

# set.seed(seed_value)
t.time.sim4 <- Sim(expression(run(design.4)), I, "t.TwoStage", T, T) / I
t.time.sim4

c(t.time.sim1, t.time.sim2, t.time.sim4)
sum(t.time.sim1, t.time.sim2, t.time.sim4)


# Estimation of processing time
I <- 20

# set.seed(seed_value)
t.time.sim1 <- Sim(expression(run(design.1)), I, "t.SRS", T, T) / I
t.time.sim1

# set.seed(seed_value)
t.time.sim2 <- Sim(expression(run(design.2)), I, "t.Clust", T, T) / I
t.time.sim2

# set.seed(seed_value)
t.time.sim4 <- Sim(expression(run(design.4)), I, "t.TwoStage", T, T) / I
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

time.available <- 1  ### in hours

# Rounding base
base <- 10

I <- floor(time.available * 3600 / t.time.sim / base) * base
I

#I <- 10000
#I

# set.seed(seed_value)
time.sim1 <- Sim(expression(run(design.1)), I, "SRS", T, F) / I
time.sim1

# set.seed(seed_value)
time.sim2 <- Sim(expression(run(design.2)), I, "Clust", T, F) / I
time.sim2

# set.seed(seed_value)
time.sim4 <- Sim(expression(run(design.4)), I, "Two.Stage", T, F) / I
time.sim4


time.sim <- sum(time.sim1, time.sim2, time.sim4)
c(time.sim1, time.sim2, time.sim4)
time.sim



### END ###

### Results

setwd(dir.work)

load("SRS.Rdata")
load("Clust.Rdata")
load("Two.Stage.Rdata")

head(SRS)
head(Clust)
head(Two.Stage)

res <- rbind(SRS, Clust, Two.Stage)

names(res)[4] <- "dist"
names(res)

head(res)
nrow(res)

table(res$name)
table(res$err)

m <- tapply(res$dist, res$name, mean, na.rm = T)
sd <- tapply(res$dist, res$name, sd, na.rm = T)

m
sd
sd/m

tapply(res$dist, res$name, min, na.rm = T)
tapply(res$dist, res$name, max, na.rm = T)


gr.den <- function(var.name) {
  var.name <- as.character(var.name)
  ggplot(data = res, aes_string(x = var.name, color = "name", group = "name")) +
    geom_density(size = 1) +
    xlab(var.name)
}

gr.den("dist")

res1 <- res[res$name == "Clust",]

ggplot(data = res, aes(x = dist, fill = name)) +
  geom_density(size = 1)

ggplot(data = res[res$name == "SRS",], aes(x = dist, color = name, group = name)) +
  geom_density(size = 1)


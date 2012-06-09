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


### Libs
require(bigmemory)
require(bigtabulate)
require(ggplot2)


### Procedures

setwd(dir.proc)

source("Define_SamplingSRS.R")
source("Define_SamplingSRSWeek.R")

source("Define_SamplingCluster.R")
source("Define_SamplingClusterWeek.R")

source("Define_SamplingClusterStr.R")

source("Define_SamplingTwoStage.R")

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

setwd(dir.temp)

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




### Sim functions

design.1 <- expression(SamplingSRSWeek(frame.p, n, ".dw", weeks = weeks))

design.2 <- expression(SamplingClusterWeek(frame.p, frame.h, n, ".dw", "H_ID",
                                           weeks = weeks))

design.4 <- expression(SamplingTwoStage(frame.PSU, frame.h, frame.p,
                                        ".dw1", ".dw2", ".dw", ".week",
                                        "iec2010", "H_ID", "strata",
                                        sampl.des.par))

run.SRS <- function(n = 50) {
  s <- eval(design.1)
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  time <- Cost(d, 40, n.h, n.p, 2/6, 1/6)
  res <- data.frame(n = n, count.P = n.p, count.H = n.h, dist = d / 1e3,
                    time = time)
  return(res)
}

run.Clust <- function(n = 50) {
  s <- eval(design.2)
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  time <- Cost(d, 40, n.h, n.p, 2/6, 1/6)
  res <- data.frame(n = n, count.P = n.p, count.H = n.h, dist = d / 1e3,
                    time = time)
  return(res)
}

run.TwoStage <- function(n = 50) {
  s <- eval(design.4)
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  time <- Cost(d, 40, n.h, n.p, 2/6, 1/6)
  res <- data.frame(n = n, count.P = n.p, count.H = n.h, dist = d / 1e3,
                    time = time)
  return(res)
}

run.SRS()
run.SRS(100)
run.SRS(200)

run.Clust()
run.Clust(100)
run.Clust(200)

run.TwoStage()
run.TwoStage(100)
run.TwoStage(6032)



###################################################
### Part I                                      ###
### Estimation of sample size for SRS and Clust ###
###################################################

### Test

setwd(dir.temp)

I <- 1

Sim.4("run.SRS", 100, I, "t.SRS", T, T)[[1]]
Sim.4("run.SRS", c(100, 200), I, "t.SRS", T, T)[[1]]
Sim.4("run.SRS", seq(100, 300, 100), I, "t.SRS", T, T)[[1]]

Sim.4("run.Clust", 100, I, "t.SRS", T, T)[[1]]
Sim.4("run.Clust", c(100, 200), I, "t.SRS", T, T)[[1]]
Sim.4("run.Clust", seq(100, 300, 100), I, "t.SRS", T, T)[[1]]

Sim.4("run.TwoStage", 6032, I, "t.SRS", T, T, seed = 2)[[1]]


ss.h <- seq(200, 10e3, 200)
ss.p <- round(ss.h * N/M)

ss.h
ss.p

t.time.sim1 <- Sim.4("run.SRS", ss.p, I, "t.SRS", T, T)
t.time.sim1[[2]] / I

t.time.sim2 <- Sim.4("run.Clust", ss.h, I, "t.Clust", T, T)
t.time.sim2[[2]] / I

t.time.sim3 <- Sim.4("run.TwoStage", 6032, I, "t.Clust", T, T, seed = 2)
t.time.sim3[[2]] / I

c(t.time.sim1[[2]], t.time.sim2[[2]], t.time.sim3[[2]]) / I
sum(t.time.sim1[[2]], t.time.sim2[[2]], t.time.sim3[[2]]) / I

head(t.time.sim1[[1]])
head(t.time.sim2[[1]])
head(t.time.sim3[[1]])



################
### Real sim ###
################

# Estimation of the number of iterations

setwd(dir.temp)

I <- 5

t.time.sim1 <- Sim.4("run.SRS", ss.p, I, "t.SRS", T, T)
t.time.sim2 <- Sim.4("run.Clust", ss.h, I, "t.Clust", T, T)
t.time.sim3 <- Sim.4("run.TwoStage", 6032, I, "t.Clust", T, T)

t.time.sim <- sum(t.time.sim1[[2]], t.time.sim2[[2]], t.time.sim3[[2]]) / I
t.time.sim

time.available <- 3  ### in hours

# Rounding base
base <- 10

I <- floor(time.available * 3600 / t.time.sim / base) * base
I

###########
### RUN ###
###########

setwd(dir.work)

res1 <- Sim.4("run.SRS", ss.p, I, "SRS", T, T)
res2 <- Sim.4("run.Clust", ss.h, I, "Clust", T, T)
res3 <- Sim.4("run.TwoStage", 6032, I, "TwoStage", T, T)

# res3 <- Sim.4("run.TwoStage", 6032, 10, "TwoStage", T, T)


head(res1[[1]])
head(res2[[1]])
head(res3[[1]])


#################
### Results 1 ###
#################

# 2012-03-22

setwd(dir.res)

load("SRS 2012-03-22 09:21:24.Rdata")
load("Clust 2012-03-22 10:36:31.Rdata")
load("TwoStage 2012-03-22 12:00:44.Rdata")

# SRS <- res1[[1]]
# Clust <- res2[[1]]
# TwoStage <- res3[[1]]

test.df(SRS)
test.df(Clust)
test.df(TwoStage)

res <- rbind(SRS, Clust, TwoStage)

head(res)
names(res)
table(res$name)

res_agg <- aggregate(res[7:10], list(name = res$name, n = res$n), mean)
head(res_agg)
class(res_agg)
sapply(res_agg, class)


head(TwoStage)
d1 <- mean(TwoStage$dist, na.rm = T)
t1 <- mean(TwoStage[,10], na.rm = T)
d1
t1


p1 <- ggplot(res_agg, aes(count.P, dist)) +
  geom_point(aes(colour = name)) +
  geom_hline(yintercept = d1, color = "red") +
  theme_bw()

p2 <- ggplot(res_agg, aes(count.P, time)) +
  geom_point(aes(colour = name)) +
  geom_hline(yintercept = t1, color = "red") +
  theme_bw()

p1
p2

pdf(paste("Plots ", Sys.time(), ".pdf", sep=""))
p1
p2
dev.off()


###
# Select sample size for SRS and Clust = TwoSTage

head(res)
names(res)[1] <- "timestamp"
head(res)

head(res_agg)

table(res_agg$name)

b1 <- res_agg[res_agg$name == "TwoStage", "time"]
b1

time1 <- abs(res_agg[res_agg$name == "SRS", "time"] - b1)
time2 <- abs(res_agg[res_agg$name == "Clust", "time"] - b1)

time1 == min(time1)
time2 == min(time2)

b1
res_agg[res_agg$name == "SRS", ][time1 == min(time1), ]
res_agg[res_agg$name == "Clust", ][time2 == min(time2), ]

n1 <- res_agg[res_agg$name == "SRS", ][time1 == min(time1), "n"]
n2 <- res_agg[res_agg$name == "Clust", ][time2 == min(time2), "n"]

n1
n2

res_agg[(res_agg$name == "SRS" & res_agg$n == n1) |
  (res_agg$name == "Clust" & res_agg$n == n2) |
  res_agg$name == "TwoStage", ]

head(res)

res_sub <- res[(res$name == "SRS" & res$n == n1) |
  (res$name == "Clust" & res$n == n2) | res$name == "TwoStage", ]

head(res_sub)

aggregate(res_sub["time"], list(design = res_sub$name), mean, na.rm = T)
aggregate(res_sub["time"], list(design = res_sub$name), sd, na.rm = T)

head(res)



tab1 <- aggregate(res[c("count.P", "dist", "time")], list(design = res$name, n = res$n), mean, na.rm = T)
tab2 <- aggregate(res[c("count.P", "dist", "time")], list(design = res$name, n = res$n), sd, na.rm = T)

names(tab1)
names(tab2)

names(tab1)[-(1:2)] <- paste(names(tab1)[-(1:2)], "mean", sep=".")
names(tab2)[-(1:2)] <- paste(names(tab2)[-(1:2)], "sd", sep=".")

head(tab1)
head(tab2)

tab <- merge(tab1, tab2)
tab <- tab[order(tab$design, tab$n), ]
head(tab)

tab[tab$design == "TwoStage", ]

d1 <- tab[tab$design == "TwoStage", "dist.mean"]
t1 <- tab[tab$design == "TwoStage", "time.mean"]

plot.count.P.sd <- ggplot(tab, aes(count.P.mean, count.P.sd)) +
  geom_point(aes(colour = design, size = count.P.sd)) +
  theme_bw()

plot.dist.mean <- ggplot(tab, aes(count.P.mean, dist.mean)) +
  geom_point(aes(colour = design, size = dist.sd)) +
  geom_hline(yintercept = d1, color = "red") +
  theme_bw()

plot.dist.sd <- ggplot(tab, aes(count.P.mean, dist.sd)) +
  geom_point(aes(colour = design, size = dist.sd)) +
  theme_bw()

plot.time.mean <- ggplot(tab, aes(count.P.mean, time.mean)) +
  geom_point(aes(colour = design, size = time.sd)) +
  geom_hline(yintercept = t1, color = "red") +
  theme_bw()

plot.time.sd <- ggplot(tab, aes(count.P.mean, time.sd)) +
  geom_point(aes(colour = design, size = time.sd)) +
  theme_bw()

plot.count.P.sd
plot.dist.mean
plot.dist.sd
plot.time.mean
plot.time.sd

pdf(paste("Plots2 ", Sys.time(), ".pdf", sep=""))
plot.count.P.sd
plot.dist.mean
plot.dist.sd
plot.time.mean
plot.time.sd
dev.off()


gr.den <- function(var.name) {
  var.name <- as.character(var.name)
  ggplot(data = res_sub, aes_string(x = var.name, color = "name", group = "name")) +
    geom_density(size = 1) +
    xlab(var.name)
}

gr.den("time")

ggplot(res[res$name == "SRS", ], aes(n, time)) +
  geom_point(aes(colour = "red")) +
  theme_bw()

ggplot(data = res[res$name == "SRS", ], aes_string(x = "time", group = "n")) +
  geom_density(size = 1) +
  xlab("time")



#################
### Results 2 ###
#################

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


set.seed(runif(1))


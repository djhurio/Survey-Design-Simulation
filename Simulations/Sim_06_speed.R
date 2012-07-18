############# Simulation of Cluster Sample, Two stage sampling

### Simulation of survey costs
### SRS of individuals

### Ver01
# Initial version

### Ver05
# Simulation of sampling of population changing over time

### Ver06
# Simulation of different travel speeds

################
### Start up ###
################


### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


### Options
options(default.stringsAsFactors = FALSE)
getOption("default.stringsAsFactors")
default.stringsAsFactors()



### Libs
require(bigmemory)
require(bigtabulate)
require(ggplot2)
require(foreach)


### Procedures

sourceDir(dir.proc, F)


### DATA

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")
load("frame.PSU.Rdata")
load("frame.int.Rdata")
pop.eka <- attach.big.matrix("pop.eka.desc")




##################
### Simulation ###
##################

setwd(dir.tmp)


### Simulation designs

N <- nrow(frame.p)
N

M <- nrow(frame.h)
M

M.h <- as.vector(bigtable(frame.h, "strata"))
M.h
sum(M.h)
sum(M.h) == M

max.PSU.size.h <- aggregate(frame.PSU["size"],
                            frame.PSU["strata"], max)[, 2]

delta <- 1 / max.PSU.size.h
delta

weeks <- 13

sampl.des.par <- data.frame(s = 1:4,
                            A = 8,
                            B = c(1, 2, 2, 2),
                            W = 13,
                            #d = delta,
                            d = 0,
                            Q = 0,
                            w = weeks,
                            M = M.h,
                            m = c(10, 7, 8, 9))


sampl.des.par

m <- sum(sampl.des.par$A * 
  sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
m

n <- round(m * N / M)
n

m.h <- round(m * M.h / M)  # Proportional sample allocation
m.h
sum(m.h)
sum(m.h) == m




### Sim functions

# design.1 <- expression(SamplingSRSWeek(frame.p, n, ".dw",
#                                        weeks = weeks))
# 
# design.2 <- expression(SamplingClusterWeek(frame.p, frame.h, n,
#                                            ".dw", "H_ID",
#                                            weeks = weeks))
# 
# design.4 <- expression(SamplingTwoStage(frame.PSU, frame.h, frame.p,
#                                         ".dw1", ".dw2", ".dw", ".week",
#                                         "iec2010", "H_ID", "strata",
#                                         sampl.des.par))


d1 <- 'SamplingSRSWeek(frame.p, n, ".dw", weeks = weeks)'

d2 <- 'SamplingClusterWeek(frame.p, frame.h, n, ".dw", "H_ID", weeks = weeks)'

d3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par)'

s1 <- eval(parse(text = d1))
s2 <- eval(parse(text = d2))
s3 <- eval(parse(text = d3))

head(s1)
head(s2)
head(s3)


##### run function

run <- function(code, n, speed, time.int.H, time.int.P) {
  s <- eval(parse(text = code))
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  time <- Cost(d, speed, n.h, n.p, time.int.H, time.int.P)
  res <- data.frame(n = n, count.P = n.p, count.H = n.h, dist = d / 1e3,
                    time = time)
  return(res)
}

run(d1, 50, 20, 1/3, 1/6)
run(d1, 100, 20, 1/3, 1/6)

run(d2, 50, 20, 1/3, 1/6)
run(d2, 100, 20, 1/3, 1/6)

run(d3, 50, 20, 1/3, 1/6)
run(d3, 6032, 20, 1/3, 1/6)


aaa <- data.frame(code = d1, n = c(100, 200), speed = 20,
                  time.int.H = 1/3, time.int.P = 1/6,
                  stringsAsFactors = F)

aaa

do.call(run, aaa[1, ])
do.call(run, aaa[2, ])


###################################################
### Part I                                      ###
### Estimation of sample size for SRS and Clust ###
###################################################

### Test

setwd(dir.tmp)

I <- 1

arg1 <- data.frame(code = d1, n = seq(100, 300, 100),
                   speed = 20, time.int.H = 1/3, time.int.P = 1/6,
                   stringsAsFactors = F)

arg2 <- data.frame(code = d2, n = seq(100, 300, 100),
                   speed = 20, time.int.H = 1/3, time.int.P = 1/6,
                   stringsAsFactors = F)

arg3 <- data.frame(code = d3, n = NA,
                   speed = 20, time.int.H = 1/3, time.int.P = 1/6,
                   stringsAsFactors = F)

arg <- rbind(arg1, arg2, arg3)
arg

Sim(fun = "run", arg = arg3, I = I, print = T)[[1]]
Sim(fun = "run", arg = arg, I = I, print = T)[[1]]

ss.h <- seq(200, 10e3, 400)
ss.p <- round(ss.h * N/M)

ss.h
ss.p

speed <- c(20, 30, 40)

rep(speed, each = length(ss.p))
rep(ss.p, times = length(speed))

arg1 <- data.frame(code = d1, n = rep(ss.p, times = length(speed)),
                   speed = rep(speed, each = length(ss.p)),
                   time.int.H = 1/3, time.int.P = 1/6,
                   stringsAsFactors = F)

arg2 <- data.frame(code = d2, n = rep(ss.h, times = length(speed)),
                   speed = rep(speed, each = length(ss.h)),
                   time.int.H = 1/3, time.int.P = 1/6,
                   stringsAsFactors = F)

arg3 <- data.frame(code = d3, n = NA,
                   speed = speed, time.int.H = 1/3, time.int.P = 1/6,
                   stringsAsFactors = F)

arg <- rbind(arg1, arg2, arg3)
# arg


t.time.sim1 <- Sim(fun = "run", arg = arg, I = I, print = T)
t.time.sim1[[1]]
t.time.sim1[[2]] / I / 60

head(t.time.sim1[[1]])



################
### Real sim ###
################

# Estimation of the number of iterations

setwd(dir.tmp)

I <- 5

t.time.sim1 <- Sim(fun = "run", arg = arg, I = I, print = T)

t.time.sim <- sum(t.time.sim1[[2]]) / (I + 1)
t.time.sim

time.available <- 4  ### in hours

# Rounding base
base <- 10

I <- floor(time.available * 3600 / t.time.sim / base) * base
I

###########
### RUN ###
###########

setwd(dir.tmp)

res1 <- Sim(fun = "run", arg = arg, I = I, print = T)

# res3 <- Sim("run.TwoStage", 6032, 10, "TwoStage", T, T)


head(res1[[1]])


#################
### Results 1 ###
#################

setwd(dir.res)

# 2012-03-22
# load("SRS 2012-03-22 09:21:24.Rdata")
# load("Clust 2012-03-22 10:36:31.Rdata")
# load("TwoStage 2012-03-22 12:00:44.Rdata")

# SRS <- res1[[1]]
# Clust <- res2[[1]]
# TwoStage <- res3[[1]]

# 2012-06-11
load("SRS 2012-06-10 16:58:31.Rdata")
load("Clust 2012-06-10 19:00:00.Rdata")
load("TwoStage 2012-06-10 21:17:40.Rdata")

test.df(SRS)
test.df(Clust)
test.df(TwoStage)

head(TwoStage[!is.na(TwoStage$err), ])

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

# pdf(paste("Plots ", Sys.time(), ".pdf", sep=""))
# p1
# p2
# dev.off()


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

n_SRS <- n1
n_Clust <- n2

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



tab1 <- aggregate(res[c("count.P", "count.H", "dist", "time")],
                  list(design = res$name, n = res$n),
                  mean, na.rm = T)

tab2 <- aggregate(res[c("count.P", "count.H", "dist", "time")],
                  list(design = res$name, n = res$n),
                  sd, na.rm = T)

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

plot.time.mean.P <- ggplot(tab, aes(count.P.mean, time.mean)) +
  geom_point(aes(colour = design, size = time.sd)) +
  geom_hline(yintercept = t1, color = "red") +
  theme_bw()

plot.time.mean.H <- ggplot(tab, aes(count.H.mean, time.mean)) +
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
plot.time.mean.H

pdf("plot_time_mean.pdf")
plot.time.mean.P
dev.off()

plot.time.sd

# pdf(paste("Plots2 ", Sys.time(), ".pdf", sep=""))
#   plot.count.P.sd
#   plot.dist.mean
#   plot.dist.sd
#   plot.time.mean
#   plot.time.sd
# dev.off()


gr.den("time", data = res_sub, fill = "name", adjust = 2)

ggplot(res[res$name == "SRS", ], aes(n, time)) +
  geom_point(aes(colour = "red")) +
  theme_bw()

ggplot(data = res[res$name == "SRS", ], aes_string(x = "time", group = "n")) +
  geom_density(size = 1) +
  xlab("time")



###########################################
### Part II                             ###
### Estimation of population parameters ###
###########################################


### True Values
### Can skip

tv.week <- foreach(week = 1:52, .combine = rbind, .final = as.data.frame) %do% {
  c(week, bigtable(pop.eka, paste("eka", week, sep="")))
}

class(tv.week)
head(tv.week)

rownames(tv.week) <- NULL
colnames(tv.week) <- c("week", "sum.empl", "sum.unempl", "sum.inact")
head(tv.week)

tv.week$year <- trunc((tv.week$week - 1) / 52) + 1
tv.week$qrt <- trunc((tv.week$week - 1) / 13) + 1
head(tv.week)

# tv <- as.data.frame(matrix(colMeans(tv.week), 1, 3))

tv <- aggregate(tv.week[, c("sum.empl", "sum.unempl", "sum.inact")],
                tv.week[, c("year", "qrt")], mean)
tv

# tv$pop <- rowSums(tv[c("empl", "unempl", "inact")])
# tv$act <- rowSums(tv[c("empl", "unempl")])
# tv$r.empl <- tv$empl / tv$pop
# tv$r.unempl <- tv$unempl / tv$act

tv <- CompEmp(tv, colnames(tv)[3:5])
tv

setwd(dir.res)
save(tv, file = "pop.eka_tv.Rdata")



###

setwd(dir.tmp)

n_SRS
n_Clust

# n_SRS <- 2890
# n_Clust <- 1300

# s <- eval(design.1)
# head(s)
# nrow(s)

estimation <- function(s, n) {
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  time <- Cost(d, 20, n.h, n.p, 2/6, 1/6)
  
  s2 <- extr.data(pop.eka, s$casenum, s$.week, 3, "eka.time")
  s2$empl <- as.integer(s2$eka.time == 1)
  s2$unempl <- as.integer(s2$eka.time == 2)
  s2$inact <- as.integer(s2$eka.time == 3)
  head(s2)

  param <- rbind(c("sum", "empl", NA),
                 c("sum", "unempl", NA),
                 c("sum", "inact", NA))

  est <- Estimation(s2, s$.dw, param)
  
  res <- data.frame(n = n,
                    count.P = n.p,
                    count.H = n.h,
                    dist = d / 1e3,
                    time = time,
                    est)

  return(res)
}

run.SRS <- function(n = 50) {
  s <- eval(design.1)
  return(estimation(s, n))
}

run.Clust <- function(n = 50) {
  s <- eval(design.2)
  return(estimation(s, n))
}

run.TwoStage <- function(n = 50) {
  s <- eval(design.4)
  return(estimation(s, n))
}

run.SRS(100)
run.SRS(n_SRS)

run.Clust(100)
run.Clust(n_Clust)

# run.TwoStage(6032)
run.TwoStage(1300)


### Test

setwd(dir.tmp)

I <- 1

Sim("run.SRS", 100, I, "t.SRS", T, T)[[1]]
Sim("run.SRS", n_SRS, I, "t.SRS", T, T)[[1]]

Sim("run.Clust", 100, I, "t.Clust", T, T)[[1]]
Sim("run.Clust", n_Clust, I, "t.Clust", T, T)[[1]]

Sim("run.TwoStage", n_Clust, I, "t.TwoStage", T, T, seed = 1)[[1]]
Sim("run.TwoStage", n_Clust, I, "t.TwoStage", T, T, seed = 2)[[1]]


t.time.sim1 <- Sim("run.SRS", n_SRS, I, "t.SRS", T, T)
t.time.sim1[[2]] / I

t.time.sim2 <- Sim("run.Clust", n_Clust, I, "t.Clust", T, T)
t.time.sim2[[2]] / I

t.time.sim3 <- Sim("run.TwoStage", 6032, I, "t.TwoStage", T, T, seed = 2)
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

setwd(dir.tmp)

I <- 5

t.time.sim1 <- Sim("run.SRS", n_SRS, I, "t.SRS", T, T)
t.time.sim2 <- Sim("run.Clust", n_Clust, I, "t.Clust", T, T)
t.time.sim3 <- Sim("run.TwoStage", 6032, I, "t.TwoStage", T, T)

t.time.sim <- sum(t.time.sim1[[2]], t.time.sim2[[2]], t.time.sim3[[2]]) /
  (I + 1)
t.time.sim

time.available <- 8  ### in hours

# Rounding base
base <- 10

I <- floor(time.available * 3600 / t.time.sim / base) * base
I

###########
### RUN ###
###########

# setwd(dir.res)
setwd(dir.tmp)

t1 <- Sys.time()
res1 <- Sim("run.SRS", n_SRS, I, "SRS", T, T)
res2 <- Sim("run.Clust", n_Clust, I, "Clust", T, T)
res3 <- Sim("run.TwoStage", n_Clust, I, "TwoStage", T, T)
t2 <- Sys.time()

t2 - t1

head(res1[[1]])
head(res2[[1]])
head(res3[[1]])



### Save

# setwd(dir.res)
# 
# Sys.time()
# 
# save(res1, res2, res3,
#      file = paste("Sim_Results_", Sys.time(),".Rdata", sep=""))





### Load

setwd(dir.res)

# True values
load("pop.eka_tv.Rdata")
tv

# # 2012-05-17
# load("SRS 2012-05-17 19:41:08.Rdata")
# load("Clust 2012-05-17 19:53:31.Rdata")
# load("TwoStage 2012-05-17 20:13:50.Rdata")

# 2012-05-26
# load("SRS 2012-05-26 10:07:39.Rdata")
# load("Clust 2012-05-26 10:56:28.Rdata")
# load("TwoStage 2012-05-26 12:16:28.Rdata")

# 2012-06-09
# load("SRS 2012-06-09 13:10:10.Rdata")
# load("Clust 2012-06-09 13:17:40.Rdata")
# load("TwoStage 2012-06-09 13:25:43.Rdata")

# load("SRS 2012-06-09 15:44:23.Rdata")
# load("Clust 2012-06-09 16:00:03.Rdata")
# load("TwoStage 2012-06-09 16:16:31.Rdata")

# load("SRS 2012-06-09 18:17:21.Rdata")
# load("Clust 2012-06-09 18:30:47.Rdata")
# load("TwoStage 2012-06-09 18:45:17.Rdata")

load("SRS 2012-06-11 07:55:18.Rdata")
load("Clust 2012-06-11 09:20:00.Rdata")
load("TwoStage 2012-06-11 11:46:55.Rdata")


test.df(SRS)
test.df(Clust)
test.df(TwoStage)

res <- rbind(SRS, Clust, TwoStage)

head(res)
names(res)
table(res$name)

names(res)
tail(names(res), 3)

res <- CompEmp(res, tail(names(res), 3))
head(res)

write.csv(res, "res.txt")

ncol(res)

aggregate(res[6:ncol(res)], res["name"], mean, na.rm = T)
tv


aggregate(res[6:ncol(res)], res["name"], sd, na.rm = T)
aggregate(res[6:ncol(res)], res["name"], min, na.rm = T)
aggregate(res[6:ncol(res)], res["name"], max, na.rm = T)

head(res)
tv

nrow(res)

# gr.den("r.unempl", data = res, tv = tv[1, ], fill = "name", color = "name")
# gr.den("time", data = res, tv = tv[1, ], fill = "name", color = "name")
# gr.den("dist", data = res, tv = tv[1, ], fill = "name", color = "name")


v <- c("sum.empl", "sum.unempl", "sum.inact", "sum.act",
       "r.act", "r.empl", "r.unempl",
       "dist", "time")
v

gr0 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
              fill = "name", color = "name",
              adjust = 1/2, title = "adjust = 1/2")
gr0

gr1 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
              fill = "name", color = "name",
              adjust = 1, title = "adjust = 1")
gr1

gr2 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
              fill = "name", color = "name",
              adjust = 2, title = "")
gr2

gr.den("sum.empl", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")
gr.den("sum.unempl", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")
gr.den("sum.act", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")
gr.den("sum.inact", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")
gr.den("r.empl", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")
gr.den("r.unempl", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")
gr.den("r.act", data = res, tv = tv[1, ],
       fill = "name", color = "name", adjust = 2, title = "")



qplot(sum.empl, data=res, geom="histogram")

pdf("plots_estim_%03d.pdf", onefile = F)
gr2
dev.off()

I

qplot(sum.empl, data = res, geom="freqpoly", binwidth = 1000, 
      color = name)

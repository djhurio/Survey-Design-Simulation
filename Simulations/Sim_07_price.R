############# Simulation of Cluster Sample, Two stage sampling

### Simulation of survey costs
### SRS of individuals

### Ver01
# Initial version

### Ver05
# Simulation of sampling of population changing over time

### Ver06
# Simulation of different travel speeds

### Ver07
# Simulation when cost is defined by money


################
### Start up ###
################


### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


### Options
# options(default.stringsAsFactors = FALSE)
# getOption("default.stringsAsFactors")
# default.stringsAsFactors()

# pdf.options()
pdf.options(width = 0, height = 0, paper = "a4r")


### Libs
require(bigmemory)
require(bigtabulate)
require(ggplot2)
require(foreach)
require(nlme)

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
                            d = delta,
                            #d = 0,
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

d1 <- 'SamplingSRSWeek(frame.p, n, ".dw", weeks = weeks)'
d2 <- 'SamplingClusterWeek(frame.p, frame.h, n, ".dw", "H_ID", weeks = weeks)'
d3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par)'



###################################################
### Part I                                      ###
### Estimation of sample size for SRS and Clust ###
###################################################

##### run function

# run <- function(code, n, speed, time.int.H, time.int.P) {
#   s <- eval(parse(text = code))
#   n.p <- nrow(s)
#   n.h <- length(unique(s$H_ID))
#   d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
#                     frame.int[c("int.ID", "x_int", "y_int")])
#   time <- Cost(d, speed, n.h, n.p, time.int.H, time.int.P)
#   res <- data.frame(count.P = n.p, count.H = n.h, dist = d / 1e3, time = time)
#   return(res)
# }

run <- function(code, n) {
  s <- eval(parse(text = code))
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  res <- data.frame(count.P = n.p, count.H = n.h, dist = d)
  return(res)
}


### Test

setwd(dir.tmp)

I <- 1

# arg1 <- data.frame(code = d1, n = seq(100, 300, 100),
#                    speed = 20, time.int.H = 1/3, time.int.P = 1/6,
#                    stringsAsFactors = F)
# 
# arg2 <- data.frame(code = d2, n = seq(100, 300, 100),
#                    speed = 20, time.int.H = 1/3, time.int.P = 1/6,
#                    stringsAsFactors = F)
# 
# arg3 <- data.frame(code = d3, n = NA,
#                    speed = 20, time.int.H = 1/3, time.int.P = 1/6,
#                    stringsAsFactors = F)
# 
# arg <- rbind(arg1, arg2, arg3)
# arg
# 
# Sim(fun = "run", arg = arg3, I = I, print = T)[[1]]
# Sim(fun = "run", arg = arg, I = I, print = T)[[1]]


###

ss.h <- round(seq(800, 10e3, 400) / 13) * 13
ss.p <- round(ss.h * N/M / 13) * 13

ss.h
ss.p

ss.h %% 13
ss.p %% 13


arg1 <- data.frame(code = d1, n = ss.p, stringsAsFactors = F)
arg2 <- data.frame(code = d2, n = ss.h, stringsAsFactors = F)
arg3 <- data.frame(code = d3, n = 6032, stringsAsFactors = F)

arg <- rbind(arg1, arg2, arg3)

head(arg)
nrow(arg)

arg$n %% 13


################
### Real sim ###
################

# Estimation of the number of iterations

setwd(dir.tmp)

I <- 5

t.time.sim1 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T)

t.time.sim <- sum(t.time.sim1[[2]]) / (I)
t.time.sim
t.time.sim / 60

time.available <- 5  ### in hours

# Rounding base
base <- 10

I <- floor(time.available * 3600 / t.time.sim / base) * base
I

###########
### RUN ###
###########

setwd(dir.res)

I
I * nrow(arg)

res1 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T)

head(res1[[1]])


#################
### Results 1 ###
#################

# res <- res1[[1]]

setwd(dir.res)
load("res 2012-09-19 22:36:10.Rdata")

dim(res)

test.df(res)

head(res)

# Errors in data
head(res[!is.na(res$err), ])

# Remove errors
nrow(res)
res <- res[is.na(res$err), ]
nrow(res)

table(res$timestamp, useNA = "ifany")
table(res$name, useNA = "ifany")
table(res$seed, useNA = "ifany")
table(res$err, useNA = "ifany")

res$timestamp <- NULL
res$name <- NULL
res$seed <- NULL
res$err <- NULL

head(res)


### design

a <- regexpr("(", res$code, fixed = T)
head(a)
table(a)

b <- substr(res$code, 9, a-1)
head(b)
table(b)

res$design <- b
head(res)

res$code <- NULL
head(res)


### Speed as character

# class(res$speed)
# res$speed <- as.character(res$speed)
# class(res$speed)


### Cost calculation

head(res)

res$dist <- res$dist / 1e3

res$cost.t <- Cost2(trip = res$dist, cons = 8, price.f = 1)
res$cost.i <- Cost2(n.h = res$count.H, n.p = res$count.P,
                    price.h = 3, price.p = 1)
res$cost <- res$cost.t + res$cost.i

head(res)


### TwoStage sampling results

head(res)
a <- grep("TwoStage", res$design)
head(a)
length(a)

res_TwoStage <- res[a, ]
head(res_TwoStage)
# table(res_TwoStage$speed)

dt <- aggregate(res_TwoStage[c("dist", "cost.t", "cost.i", "cost")],
                res_TwoStage[c("design")], mean)

dt

### NAs

head(res)

table(res$design)
table(is.na(res$n))

res$n[is.na(res$n)] <- 0
table(is.na(res$n))



#### Average results

res_agg <- aggregate(res[c("dist", "cost.t", "cost.i", "cost", "count.P", "count.H")],
                     res[c("n", "design")], mean,
                     na.action = na.pass)

table(res_agg$design)

head(res_agg)
dim(res_agg)
sapply(res_agg, class)


dt

p1 <- ggplot(res_agg, aes(x = count.P, y = dist, colour = design)) +
  stat_smooth(method=lm, linetype = "dotted", alpha = .1) +
  geom_point() +
  geom_hline(aes(yintercept = dt$dist, colour = dt$design)) +
  theme_bw()
p1

p2 <- ggplot(res_agg, aes(x = count.P, y = cost.t, colour = design)) +
  stat_smooth(method=lm, linetype = "dotted", alpha = .1) +
  geom_point() +
  geom_hline(aes(yintercept = dt$cost.t, colour = dt$design)) +
  theme_bw()
p2

p3 <- ggplot(res_agg, aes(x = count.P, y = cost.i, colour = design)) +
  stat_smooth(method=lm, linetype = "dotted", alpha = .1) +
  geom_point() +
  geom_hline(aes(yintercept = dt$cost.i, colour = dt$design)) +
  theme_bw()
p3

p4 <- ggplot(res_agg, aes(x = count.P, y = cost, colour = design)) +
  stat_smooth(method=lm, linetype = "dotted", alpha = .1) +
  geom_point() +
  geom_hline(aes(yintercept = dt$cost, colour = dt$design)) +
  theme_bw()
p4

head(res_agg)

pdf(file = "Plot_count.P.pdf")
p1
p2
p3
p4
dev.off()


###
# Select sample size for SRS and Clust to have the same cost as TwoSTage

head(res_agg)
# res_agg$group <- paste(res_agg$design, res_agg$speed)

dt

a <- lmList(n ~ cost | design, res_agg,
            subset = res_agg$design != "TwoStage")
coef(a)
coeff <- as.data.frame(coef(a))

colnames(coeff) <- letters[1:2]
coeff

coeff$TS_cost <- dt$cost
coeff

coeff$n <- coeff$a + coeff$b * coeff$TS_cost
coeff

coeff$design <- rownames(coeff)
coeff

rownames(coeff) <- NULL
coeff

n_SRS <- coeff[coeff$design == "SRSWeek", c("n")]
n_SRS

n_Clust <- coeff[coeff$design == "ClusterWeek", c("n")]
n_Clust


setwd(dir.res)
save(n_SRS, n_Clust, file = "sample.sizes.Rdata")


###########################################
### Part II                             ###
### Estimation of population parameters ###
###########################################


### load sample sizes from Part I

setwd(dir.res)
load("sample.sizes.Rdata")


### estimation() function

setwd(dir.tmp)

estimation <- function(s) {
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  
  s2 <- extr.data(pop.eka, s$casenum, s$.week, 4, "eka.time")
  s2$empl <- as.integer(s2$eka.time == 1)
  s2$unempl <- as.integer(s2$eka.time == 2)
  s2$inact <- as.integer(s2$eka.time == 3)

  param <- rbind(c("sum", "empl", NA),
                 c("sum", "unempl", NA),
                 c("sum", "inact", NA))

  est <- Estimation(s2, s$.dw, param)
  
  res <- data.frame(count.P = n.p,
                    count.H = n.h,
                    dist = d / 1e3,
                    est)

  return(res)
}


### run.2() function

run.2 <- function(code, n) {
  s <- eval(parse(text = code))
  return(estimation(s))
}


run.2(d1, 50)
run.2(d1, n_SRS)

run.2(d2, 50)
run.2(d2, n_Clust)

run.2(d3, 6032)


### Test

setwd(dir.tmp)

designs <- c(d1, d2, d3)

arg <- data.frame(code = designs,
                  n = c(n_SRS, n_Clust, 6032),
                  stringsAsFactors = F)
arg

I <- 1

Sim(fun = "run.2", arg = arg[1, ], I = I, print = T)[[1]]
Sim(fun = "run.2", arg = arg, I = I, print = T)[[1]]


################
### Real sim ###
################

# Estimation of the number of iterations

setwd(dir.tmp)

I <- 5

t.time.sim <- Sim(fun = "run.2", arg = arg, I = I, log = F)[[2]] / I
t.time.sim

# time.available <- 5  ### in hours

# Rounding base
# base <- 10

# I <- floor(time.available * 3600 / t.time.sim / base) * base
# I

I <- 5e3
time.sim <- t.time.sim * I
time.sim / 60 / 60

Sys.time() + time.sim


###########
### RUN ###
###########

I

# setwd(dir.res)
setwd(dir.tmp)

t1 <- Sys.time()
res <- Sim(fun = "run.2", arg = arg, I = I, log = F)
t2 <- Sys.time()

t1
t2

t2 - t1

head(res[[1]])
dim(res[[1]])


### Save

setwd(dir.res)

Sys.time()
save(res, file = paste("Sim_Results_II_", Sys.time(),".Rdata", sep=""))






###################
### True Values ###
###################


head(pop.eka)

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







##########################
### Results of part II ###
##########################

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

# load("SRS 2012-06-11 07:55:18.Rdata")
# load("Clust 2012-06-11 09:20:00.Rdata")
# load("TwoStage 2012-06-11 11:46:55.Rdata")

# 2012.07.29
# load("Sim_Results_II_2012-07-30 06:51:27.Rdata")
load("Sim_Results_II_2012-08-07 07:42:01.Rdata")
res <- res[[1]]

head(res)
names(res)
table(res$name)

names(res)
tail(names(res), 3)

res <- CompEmp(res, tail(names(res), 3))
head(res)


### design

a <- regexpr("(", res$code, fixed = T)
head(a)
table(a)

b <- substr(res$code, 9, a-1)
head(b)
table(b)

res$design <- b
head(res)

res$code <- NULL
head(res)

### Speed as character

class(res$speed)
res$speed <- as.character(res$speed)
class(res$speed)


### Write out
# write.csv(res, "res.txt")

ncol(res)
head(res)

varnames <- colnames(res)[7:(ncol(res) - 1)][-3]
varnames

aggregate(res[varnames], res[c("speed", "design")], mean, na.rm = T)
tv


aggregate(res[varnames], res[c("design", "speed")], sd, na.rm = T)
aggregate(res[varnames], res[c("design", "speed")], min, na.rm = T)
aggregate(res[varnames], res[c("design", "speed")], max, na.rm = T)


names(res)





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

# gr0 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
#               fill = "design", color = "design", linetype = "speed",
#               adjust = 1/2, title = "adjust = 1/2")
# gr0
# 
# gr1 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
#               fill = "design", color = "design", linetype = "speed",
#               adjust = 1, title = "adjust = 1")
# gr1
# 
# gr2 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
#               fill = "design", color = "design", linetype = "speed",
#               adjust = 2, title = "")
# gr2

head(res)


# pdf("plots_estim_%03d.pdf", onefile = F)
pdf("plots_estim_2.pdf", onefile = T)

gr.den("time", data = res[res$speed == "20", ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("time", data = res[res$speed == "30", ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("time", data = res[res$speed == "40", ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")



head(res[res$design == "SRSWeek", ])

gr.den("sum.empl", data = res[res$design == "SRSWeek", ], tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.empl", data = res[res$design == "ClusterWeek", ], tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.empl", data = res[res$design == "TwoStage", ], tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.empl", data = res[res$speed == "40", ], tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.empl", data = res[res$speed == "30", ], tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.empl", data = res[res$speed == "20", ], tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.empl", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.unempl", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.act", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("sum.inact", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("r.empl", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("r.unempl", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

gr.den("r.act", data = res, tv = tv[1, ],
       fill = "design", color = "design", linetype = "speed",
       adjust = 2, title = "")

dev.off()


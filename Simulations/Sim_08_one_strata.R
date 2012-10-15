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

### Ver08
# One stratum


################
### Start up ###
################

### Clock
T1 <- Sys.time()



### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")

# Redefine dir.res
dir.res <- paste(projwd, "Results", "Lauki", sep = "/")


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


### Selection of strata
strata.selected <- 4


# frame.p
head(frame.p)
bigtable(frame.p, "strata")
class(frame.p)
frame.p <- frame.p[frame.p[, "strata"] %in% strata.selected, ]
class(frame.p)

# frame.h
head(frame.h)
bigtable(frame.h, "strata")
class(frame.h)
frame.h <- frame.h[frame.h[, "strata"] %in% strata.selected, ]
class(frame.h)

# frame.PSU
head(frame.PSU)
bigtable(frame.PSU, "strata")
class(frame.PSU)
frame.PSU <- frame.PSU[frame.PSU[, "strata"] %in% strata.selected, ]
class(frame.PSU)

# frame.int
head(frame.int)

# pop.eka
head(pop.eka)
bigtable(pop.eka, "strata")
class(pop.eka)
pop.eka <- pop.eka[pop.eka[, "strata"] %in% strata.selected, ]
class(pop.eka)



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

# sampl.des.par <- data.frame(s = 1:4,
#                             A = 8,
#                             B = c(1, 2, 2, 2),
#                             W = 13,
#                             d = delta,
#                             Q = 0,
#                             w = weeks,
#                             M = M.h,
#                             m = c(10, 7, 8, 9))

sampl.des.par <- data.frame(s = 4,
                            A = 8,
                            B = 2,
                            W = 13,
                            d = 0,
                            Q = 0,
                            w = weeks,
                            M = M.h,
                            m = 9)

sampl.des.par

m <- sum(sampl.des.par$A * 
  sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
m

# n <- round(m * N / M)
# n
# 
# m.h <- round(m * M.h / M)  # Proportional sample allocation
# m.h
# sum(m.h)
# sum(m.h) == m




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
  res <- data.frame(count.P = n.p, count.H = n.h, dist = d / 1e3)
  return(res)
}


### Arguments

ss.h <- round(seq(m/3, m, length.out = 10) / 13) * 13
ss.p <- round(ss.h * N/M / 13) * 13

ss.h
ss.p

ss.h %% 13
ss.p %% 13


arg1 <- data.frame(code = d1, n = ss.p, stringsAsFactors = F)
arg2 <- data.frame(code = d2, n = ss.h, stringsAsFactors = F)
arg3 <- data.frame(code = d3, n = m, stringsAsFactors = F)

arg <- rbind(arg1, arg2, arg3)

head(arg)
nrow(arg)

arg$n %% 13


### Test

setwd(dir.tmp)

I <- 1


################
### Real sim ###
################

# Estimation of the number of iterations

# setwd(dir.tmp)
# 
# I <- 10
# 
# t.time.sim1 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T, cores = 1)
# t.time.sim2 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T, cores = 2)
# t.time.sim3 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T, cores = 3)
# t.time.sim4 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T, cores = 4)
# 
# t.time.sim <- c(t.time.sim1[[2]],
#                 t.time.sim2[[2]],
#                 t.time.sim3[[2]],
#                 t.time.sim4[[2]]) / I
# t.time.sim
# t.time.sim / 60
# 
# time.available <- 3  ### in hours
# 
# # Rounding base
# base <- 10
# 
# I <- floor(time.available * 3600 / t.time.sim / base) * base
# I
# 
# I <- 500
# 
# a <- as.difftime(I * t.time.sim, units = "secs")
# a
# class(a)
# as.numeric(a, units = "hours")

###########
### RUN ###
###########

setwd(dir.res)

I <- 500

I
I * nrow(arg)

t1 <- Sys.time()
res1 <- Sim(fun = "run", arg = arg, I = I, print = F, log = T, cores = 2,
            name = "results_phase1")
t2 <- Sys.time()

t2 - t1

head(res1[[1]])


#################
### Results 1 ###
#################


res <- res1[[1]]

# setwd(dir.res)
# load("res 2012-10-13 16:58:01.Rdata")

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


### Cost calculation

head(res)

res$dist <- res$dist * 4.2

res$cost.t <- Cost2(trip = res$dist, cons = 9.6, price.f = .760)
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

res_agg <- aggregate(res[c("dist", "cost.t", "cost.i", "cost",
                           "count.P", "count.H")],
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

pdf(file = "Plot_count.P_phase1.pdf")
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


### Saving

setwd(dir.res)

save(n_SRS, n_Clust, file = "sample.sizes.Rdata")

head(res_agg)
write.csv(res_agg, "res_Phase1.csv")
write.csv(coeff, "Sample_size_est.csv")



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

run.2(d3, m)



### Test

setwd(dir.tmp)

designs <- c(d1, d2, d3)

arg <- data.frame(code = designs,
                  n = c(n_SRS, n_Clust, m),
                  stringsAsFactors = F)
arg

I <- 1

# Sim(fun = "run.2", arg = arg[1, ], I = I, print = T)[[1]]
Sim(fun = "run.2", arg = arg, I = I, print = T)[[1]]


################
### Real sim ###
################

# Estimation of the number of iterations

# setwd(dir.tmp)
# 
# I <- 5
# 
# t.time.sim <- Sim(fun = "run.2", arg = arg, I = I, log = F)[[2]] / I
# t.time.sim
# 
# time.available <- 2  ### in hours
# 
# # Rounding base
# base <- 10
# 
# I <- floor(time.available * 3600 / t.time.sim / base) * base
# I
# 
# I <- 1e3
# time.sim <- t.time.sim * I
# time.sim / 60 / 60
# 
# Sys.time() + time.sim


###########
### RUN ###
###########

setwd(dir.res)

I <- 1e3

t1 <- Sys.time()
res2 <- Sim(fun = "run.2", arg = arg, I = I, log = F, cores = 2,
            name = "results_phase2")
t2 <- Sys.time()

t1
t2

t2 - t1




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

res <- res2[[1]]

head(res)
names(res)
table(res$name)




### Errors

table(res$err)

nrow(res)
res <- res[is.na(res$err), ]
nrow(res)


### Design

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





### Cost calculation

head(res)

res$dist <- res$dist * 4.2
head(res)

res$cost.t <- Cost2(trip = res$dist, cons = 9.6, price.f = .760)
res$cost.i <- Cost2(n.h = res$count.H, n.p = res$count.P,
                    price.h = 3, price.p = 1)
res$cost <- res$cost.t + res$cost.i

head(res)




### Estimates of parameters

names(res)

res <- CompEmp(res, c("sum.empl", "sum.unempl", "sum.inact"))
head(res)






### Write out
#write.csv(res, "res_Phase2.csv")

ncol(res)
head(res)

varnames <- c("count.P", "count.H", "dist", "cost.t", "cost.i", "cost",
              "N", "n.1", "sum.empl", "sum.unempl", "sum.inact",
              "sum.pop", "sum.act", "r.act", "r.empl", "r.unempl")
varnames

tv

t1 <- aggregate(res[varnames], res[c("design")], mean, na.rm = T)
t2 <- aggregate(res[varnames], res[c("design")], sd, na.rm = T)
t3 <- aggregate(res[varnames], res[c("design")], min, na.rm = T)
t4 <- aggregate(res[varnames], res[c("design")], max, na.rm = T)


names(res)





head(res)
tv

nrow(res)


head(res)

v1 <- c("dist", "cost.t", "cost.i", "cost")
v2 <- c("sum.pop", "sum.act", "sum.empl", "sum.unempl", "sum.inact",
        "r.act", "r.empl", "r.unempl")
v <- c(v1, v2)
v

gr2 <- lapply(X = v, FUN = "gr.den", data = res, tv = tv[1, ],
              fill = "design", color = "design",
              adjust = 2, title = "")

pdf("plots_phase_2.pdf", onefile = T)
gr2
dev.off()


### Save

write.csv(tv, "tv.csv")
write.csv(t1, "res_Phase2_mean.csv")
write.csv(t2, "res_Phase2_sd.csv")



### Clock
T2 <- Sys.time()

T2 - T1

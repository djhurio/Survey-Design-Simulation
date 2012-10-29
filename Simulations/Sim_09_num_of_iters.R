######## Simulation of Cluster Sample, Two stage sampling ############

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

### Ver09
# The number of iterations is estimated


### Start up ###################

### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


### Clock
T1 <- Sys.time()



# Redefine dir.res
dir.res <- paste(projwd, "Results", "numb_iter", sep = "/")



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
require(scales)

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

rm(pop.eka)
pop.eka <- attach.big.matrix("pop.eka.desc")
#pop.eka <- attach.big.matrix("pop.eka.static.desc")


### Selection of strata
strata.selected <- 4


# frame.p
head(frame.p)
bigtable(frame.p, "strata")
class(frame.p)
frame.p <- frame.p[frame.p[, "strata"] %in% strata.selected, ]
frame.p[, "casenum"] <- 1:nrow(frame.p)
class(frame.p)
head(frame.p)

# frame.h
head(frame.h)
bigtable(frame.h, "strata")
class(frame.h)
frame.h <- frame.h[frame.h[, "strata"] %in% strata.selected, ]
frame.h[, "casenum"] <- 1:nrow(frame.h)
class(frame.h)
head(frame.h)

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
pop.eka[, "casenum"] <- 1:nrow(pop.eka)
class(pop.eka)
head(pop.eka)


dim(frame.p)
dim(pop.eka)

length(as.numeric(pop.eka[, paste("eka", 1:13, sep = "")] == 1)) == nrow(pop.eka) * 13

var(as.numeric(pop.eka[, paste("eka", 1:13, sep = "")] == 1))
var(as.numeric(pop.eka[, paste("eka", 1:13, sep = "")] == 2))
var(as.numeric(pop.eka[, paste("eka", 1:13, sep = "")] == 3))

### True Values ############################

setwd(dir.res)

head(pop.eka)

tv.week <- foreach(week = 1:13, .combine = rbind, .final = as.data.frame) %do% {
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

save(tv, file = "pop.eka_tv.Rdata")




### True Variances #################################

setwd(dir.res)

head(pop.eka)

varnames <- paste("eka", 1:13, sep = "")

nrow(pop.eka)
N
n <- m / 13
n

sum(N ^ 2 * (1 - n / N) / n * cov((pop.eka[, varnames] == 1) * 1)) / (13 ^ 2)

sum(N^2 * (1 - n / N) / n * diag(var((pop.eka[, varnames] == 1) * 1))) / 13^2

(N*13)^2 * (1-m/(N*13)) / m * var(pop.eka[, "eka1"])




### Simulation designs ###############################

setwd(dir.tmp)

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

# weeks <- 13
weeks <- 1

# sampl.des.par <- data.frame(s = 1:4,
#                             A = 8,
#                             B = c(1, 2, 2, 2),
#                             W = 13,
#                             d = delta,
#                             Q = 0,
#                             w = weeks,
#                             M = M.h,
#                             m = c(10, 7, 8, 9))

sampl.des.par <- data.frame(s = strata.selected,
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



### Sim functions

d1 <- 'SamplingSRSWeek(frame.p, n, ".dw", weeks = weeks)'
d2 <- 'SamplingClusterWeek(frame.p, frame.h, n, ".dw", "H_ID", weeks = weeks)'
d3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par)'



### Part I - Estimation of sample size for SRS and Clust ############

##### run function

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

ss.h <- round(seq(m/10, m, length.out = 10) / 13) * 13
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

test1 <- Sim(fun = "run", arg = arg1, I = I, print = T, log = F, cores = 2)
test2 <- Sim(fun = "run", arg = arg2, I = I, print = T, log = F, cores = 2)
test3 <- Sim(fun = "run", arg = arg3, I = I, print = T, log = F, cores = 2)
test4 <- Sim(fun = "run", arg = arg, I = I, print = T, log = F, cores = 2)



### Estimation of the number of iterations ###################

setwd(dir.res)

I <- 5000

t1 <- Sys.time()
res <- Sim(fun = "run", arg = arg, I = I, name = "res1_5000",
           print = F, log = F, cores = 2)
t2 <- Sys.time()

t2 - t1


### Load results

setwd(dir.res)

load("res.Rdata")


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


### Estimation

res$cost_mean <- cumsum(res$cost) / res$i
res$cost_var <- cumsum(res$cost ^ 2) / (res$i - 1) - (res$cost_mean ^ 2) * res$i / (res$i - 1)
res$cost_sd <- sqrt(res$cost_var)

Zn <- qnorm(.5 + cl / 2)
Zn

Zt <- qt(.5 + cl / 2, res$i - 1)
head(Zt)
tail(Zt)

head(data.frame(i = res$i, Zt))

ggplot(data.frame(i = res$i, Zt)[Zt < 2,], aes(x = i, y = Zt)) +
  geom_point(size = 1, colour = "red") +
  geom_hline(aes(yintercept = Zn), colour = "green", linetype = "dashed") +
  theme_bw()
  

prec <- .0005

res$cost_MoE <- res$cost_sd / sqrt(res$i) * Zt
res$cost_rel_MoE <- res$cost_MoE / res$cost_mean
res$cost_mean_cil <- res$cost_mean - res$cost_MoE
res$cost_mean_ciu <- res$cost_mean + res$cost_MoE
res$I <- ((Zt * res$cost_sd) / prec / res$cost_mean) ^ 2

last_estim <- tail(res$cost_mean, 1)

head(res, 10)

t1 <- res$i > res$I
t2 <- res$cost_rel_MoE < prec
table(t1, t2)

tail(res$I, 1)
mean(res$I, na.rm = T)

p1 <- ggplot(res[11:2e3,], aes(x = i)) +
  geom_point(aes(y = cost_mean, colour = cost_rel_MoE < prec), size = 1) +
  geom_point(aes(y = cost_mean_cil), size = 1, colour = "brown") +
  geom_point(aes(y = cost_mean_ciu), size = 1, colour = "brown") +
  geom_hline(aes(yintercept = last_estim * (1 - prec)), colour = "green", linetype = "dashed") +
  geom_hline(aes(yintercept = last_estim), colour = "blue", linetype = "dashed") +
  geom_hline(aes(yintercept = last_estim * (1 + prec)), colour = "green", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(breaks = round(last_estim * seq(1 - prec * 10, 1 + prec * 10, prec))) +
  scale_x_continuous(breaks = seq(0, 1e4, 100))
p1

p2 <- ggplot(res[11:2e3,]) +
  geom_point(aes(i, cost_sd), size = 1) +
  geom_hline(aes(yintercept = tail(cost_sd, 1)), colour = "blue", linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 1e4, 100))
p2

p3 <- ggplot(res[11:2e3,]) +
  geom_point(aes(i, I, colour = I > i), size = 1) +
  geom_hline(aes(yintercept = tail(I, 1)), colour = "blue", linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 1e4, 100))
p3

png("MC_convergence.png", width = 800, height = 450)
p1
dev.off()

pdf("MC_convergence.pdf")
p1
dev.off()

pdf("MC_convergence_2.pdf")
p1
p2
p3
dev.off()


# head(res, 100)
# tail(res, 100)

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


### RUN ###############

setwd(dir.res)

I <- 600

I
I * nrow(arg)

t1 <- Sys.time()
tmp <- Sim(fun = "run", arg = arg, I = I, print = F, log = F, cores = 2,
           name = "res1")
t2 <- Sys.time()

t2 - t1

head(res1[[1]])


### Results I #########################

setwd(dir.res)
load("res1.Rdata")
res <- res1

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



### Part II - Estimation of population parameters ################


### load sample sizes from Part I

setwd(dir.res)
load("sample.sizes.Rdata")


### estimation() function

setwd(dir.tmp)

################## Check columns of pop.eka !!!
head(pop.eka)

estimation <- function(s) {
  n.p <- nrow(s)
  n.h <- length(unique(s$H_ID))
  d <- vTrip.fast.1(s[c("int.ID", ".week", "coord_x_p", "coord_y_p")],
                    frame.int[c("int.ID", "x_int", "y_int")])
  
  s2 <- extr.data(pop.eka, s$casenum, s$.week, 5, "eka.time")
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


run.2(d1, m)
# run.2(d1, n_SRS)

run.2(d2, m)
# run.2(d2, n_Clust)

run.2(d3, m)



### Test

setwd(dir.tmp)

designs <- c(d1, d2, d3)

# arg <- data.frame(code = designs,
#                   n = c(n_SRS, n_Clust, m),
#                   stringsAsFactors = F)
arg <- data.frame(code = designs,
                  n = m,
                  stringsAsFactors = F)
arg

I <- 1

# Sim(fun = "run.2", arg = arg[1, ], I = I, print = T)[[1]]
Sim(fun = "run.2", arg = arg, I = I, print = T, cores = 2)[[1]]


### Estimation of the number of iterations #################

setwd(dir.res)

I <- 5e3

t1 <- Sys.time()
tmp <- Sim(fun = "run.2", arg = arg, I = I, log = F, cores = 2,
           name = "res2_MC_con_v5_1week")
t2 <- Sys.time()

t2 - t1

q()



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


### Load results #############

setwd(dir.res)

# True values
load("pop.eka_tv.Rdata")
tv

### Dynamic population - Run #1
load("res2_MC_con.Rdata")
head(res2_MC_con)

### Dynamic population - Run #2
load("res2_MC_con_v2.Rdata")
head(res2_MC_con_v2)

### Dynamic population - Run #3
load("res2_MC_con_v3_cores2.Rdata")
head(res2_MC_con_v3_cores2)


### Static population, 13 weeks - Run #1
load("res2_MC_con_v3_stp.Rdata")
head(res2_MC_con_v3_stp)

### Static population, 13 weeks - Run #2
load("res2_MC_con_v4_stp.Rdata")
head(res2_MC_con_v4_stp)


### Static population, 1 week - Run #1
load("res2_MC_con_v5_1week.Rdata")
head(res2_MC_con_v5_1week)



### Edit results

# res <- res2_MC_con
# res <- res2_MC_con_v2
# res <- res2_MC_con_v3_cores2
# res <- rbind(res2_MC_con, res2_MC_con_v2, res2_MC_con_v3_cores2)
# res <- rbind(res2_MC_con_v3_stp, res2_MC_con_v4_stp)
# res <- res2_MC_con_v5_1week

res <- rbind(res2_MC_con, res2_MC_con_v2, res2_MC_con_v3_cores2,
             res2_MC_con_v3_stp, res2_MC_con_v4_stp, res2_MC_con_v5_1week)

class(res)

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


### Del

res$timestamp <- NULL
# res$name <- NULL
res$seed <- NULL
res$err <- NULL
res$n.1 <- NULL



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

varnames <- c("count.P", "count.H", "dist", "cost.t", "cost.i", "cost",
              "N", "n.1", "sum.empl", "sum.unempl", "sum.inact",
              "sum.pop", "sum.act", "r.act", "r.empl", "r.unempl")
varnames


res2_MC_con_2 <- res





### Analyse - SRS, sum.unempl ###############

# Confidence level
cl <- .99

head(res2_MC_con_2)
table(res2_MC_con_2$design)
table(res2_MC_con_2$name, res2_MC_con_2$design)

# design <- "SRSWeek"
# variable <- "sum.unempl"


### Temp var
# head(res2_MC_con_2)
# design <- "TwoStage"
# variable <- "sum.unempl"
# names <- "res2_MC_con_v3_stp"
# prec.p <- .005
# prec.v <- .05


plot_MC_conv <- function(design, variable, names, prec.p = .01, prec.v = .05) {

  res <- res2_MC_con_2[res2_MC_con_2$design == design & res2_MC_con_2$name %in% names,
                       c("i", variable)]

  names(res)[2] <- "y"
  
  res$i <- 1:nrow(res)
  
  head(res)
  dim(res)
  
  res$y_mean <- cumsum(res$y) / res$i
  res$y_var <- cumsum(res$y ^ 2) / (res$i - 1) - (res$y_mean ^ 2) * res$i / (res$i - 1)
  res$y_sd <- sqrt(res$y_var)
  
  Zn <- qnorm(.5 + cl / 2)
  Zn
  
  Zt <- qt(.5 + cl / 2, ifelse(res$i > 1, res$i, NA) - 1)
  head(Zt)
  tail(Zt)
  
  prec <- prec.p
  
  res$y_MoE <- res$y_sd / sqrt(res$i) * Zt
  res$y_rel_MoE <- res$y_MoE / res$y_mean
  res$y_mean_cil <- res$y_mean - res$y_MoE
  res$y_mean_ciu <- res$y_mean + res$y_MoE
  res$I <- ((Zt * res$y_sd) / prec / res$y_mean) ^ 2
  
  res$test.p <- res$y_rel_MoE < prec.p
  
  tv
  tail(res$y_mean, 1)
  tv[1, variable]
  
  last_estim <- tail(res$y_mean, 1)
  true_val <- tv[1, variable]
  
  head(res, 10)
  
  t1 <- res$i > res$I
  t2 <- res$y_rel_MoE < prec
  table(t1, t2)
  tail(res$I, 1)
  
  res$V <- res$y_var
  
  res$V_var <- 2 * res$V ^ 2 / (res$i - 1)
  res$V_sd <- sqrt(res$V_var)
  
  prec_V <- prec.v
  
  res$V_MoE <- res$V_sd * Zt
  res$V_rel_MoE <- res$V_MoE / res$V
  res$V_cil <- res$V - res$V_MoE
  res$V_ciu <- res$V + res$V_MoE
  
  res$test.v <- res$V_rel_MoE < prec.v

  head(res)
  
  t2 <- res$V_rel_MoE < prec_V
  table(t2)
  
  last_estim_V <- tail(res$V, 1)
  last_estim_V
  
  int.p <- c(-prec.p, prec.p)
  
  p1 <- ggplot(res[-1,], aes(x = i)) +
    geom_point(aes(y = y_mean, colour = test.p), size = 1) +
    geom_point(aes(y = y_mean_cil), size = 1, colour = "brown") +
    geom_point(aes(y = y_mean_ciu), size = 1, colour = "brown") +
    geom_hline(yintercept = true_val, colour = "orange", linetype = "solid", size = 1) +
    geom_hline(yintercept = true_val * (1 + c(int.p, 2 * int.p)), colour = "orange", linetype = "dashed") +
    geom_hline(yintercept = last_estim, colour = "blue", linetype = "dashed") +
    theme_bw() +
    ggtitle(paste("Mean", design, variable, "prec =", prec.p)) +
    scale_y_continuous(breaks = c(last_estim, true_val * (1 + seq(-2 * prec.p, 2 * prec.p, length.out = 5))),
                       limits = true_val * (1 + 2 * int.p),
                       labels = comma) +
    scale_x_continuous(breaks = seq(0, nrow(res), length.out=11), labels = comma)
  
#   p2 <- ggplot(res[-(1:50),], aes(x = i)) +
#     geom_point(aes(y = V, colour = test.v), size = 1) +
#     geom_point(aes(y = V_cil), size = 1, colour = "brown") +
#     geom_point(aes(y = V_ciu), size = 1, colour = "brown") +
#     geom_hline(yintercept = last_estim_V * (1 - prec.v), colour = "green", linetype = "dashed") +
#     geom_hline(yintercept = last_estim_V, colour = "blue", linetype = "dashed") +
#     geom_hline(yintercept = last_estim_V * (1 + prec.v), colour = "green", linetype = "dashed") +
#     theme_bw() + ggtitle(paste("Var", design, variable, "prec =", prec.v)) +
#     scale_y_continuous(breaks = (last_estim_V * seq(1 - prec.v * 100, 1 + prec.v * 100, prec.v)), labels = comma) +
#     scale_x_continuous(breaks = seq(0, nrow(res), length.out=11), labels = comma)
#   
#   p3 <- ggplot(res[-(1:50),], aes(x = i)) +
#     geom_point(aes(y = y), size = 1, alpha = .2) +
#     geom_hline(yintercept = last_estim, colour = "blue", linetype = "dashed") +
#     geom_hline(yintercept = true_val, colour = "orange", linetype = "solid") +
#     theme_bw() + ggtitle(paste("Observations", design, variable)) +
#     scale_y_continuous(labels = comma) +
#     scale_x_continuous(breaks = seq(0, nrow(res), length.out=11), labels = comma)
  
# return(list(p1, p2, p3))
  return(list(p1))
  
}

cl
tv

head(res2_MC_con_2)

table(res2_MC_con_2$design)
table(res2_MC_con_2$name)

tn <- sort(as.character(unique(res2_MC_con_2$name)))
tn

td <- sort(as.character(unique(res2_MC_con_2$design)))
td

tv
tvar <- names(tv)[-(1:2)]
tvar

plot_MC_conv(td[2], tvar[1], tn[1], prec.p = 0.005)[[1]]
plot_MC_conv(td[2], tvar[2], tn[1], prec.p = 0.005)[[1]]


### Dyn pop

pdf("MC_con_param_sum.unempl_dyn.pop.pdf")
  plot_MC_conv(td[2], tvar[2], tn[1], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[1:2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[2:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[1:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[1], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[1:2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[2:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[1:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[1], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[1:2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[2:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[1:3], prec.p = 0.005)[[1]]
dev.off()

pdf("MC_con_param_sum.empl_dyn.pop.pdf")
  plot_MC_conv(td[2], tvar[1], tn[1], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[1:2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[2:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[1:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[1], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[1:2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[2:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[1:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[1], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[1:2], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[2:3], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[1:3], prec.p = 0.005)[[1]]
dev.off()


### Stat pop

pdf("MC_con_param_sum.unempl_stat.pop.pdf")
  plot_MC_conv(td[2], tvar[2], tn[4], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[2], tn[4:5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[4], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[2], tn[4:5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[4], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[2], tn[4:5], prec.p = 0.005)[[1]]
dev.off()

pdf("MC_con_param_sum.empl_stat.pop.pdf")
  plot_MC_conv(td[2], tvar[1], tn[4], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[2], tvar[1], tn[4:5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[4], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[1], tvar[1], tn[4:5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[4], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[5], prec.p = 0.005)[[1]]
  plot_MC_conv(td[3], tvar[1], tn[4:5], prec.p = 0.005)[[1]]
dev.off()


### 1 week

pdf("MC_convergence_3_v7_1week.pdf")
  plot_MC_conv("SRSWeek", "sum.act", prec.p = 0.005)[[1]]
  plot_MC_conv("SRSWeek", "sum.empl", prec.p = 0.005)[[1]]
  plot_MC_conv("SRSWeek", "sum.unempl", prec.p = 0.005)[[1]]
  plot_MC_conv("SRSWeek", "sum.inact", prec.p = 0.005)[[1]]
  plot_MC_conv("SRSWeek", "r.act", prec.p = 0.005)[[1]]
  plot_MC_conv("SRSWeek", "r.empl", prec.p = 0.005)[[1]]
  plot_MC_conv("SRSWeek", "r.unempl", prec.p = 0.005)[[1]]
  
  plot_MC_conv("ClusterWeek", "sum.act", prec.p = 0.005)[[1]]
  plot_MC_conv("ClusterWeek", "sum.empl", prec.p = 0.005)[[1]]
  plot_MC_conv("ClusterWeek", "sum.unempl", prec.p = 0.005)[[1]]
  plot_MC_conv("ClusterWeek", "sum.inact", prec.p = 0.005)[[1]]
  plot_MC_conv("ClusterWeek", "r.act", prec.p = 0.005)[[1]]
  plot_MC_conv("ClusterWeek", "r.empl", prec.p = 0.005)[[1]]
  plot_MC_conv("ClusterWeek", "r.unempl", prec.p = 0.005)[[1]]
  
  plot_MC_conv("TwoStage", "sum.act", prec.p = 0.005)[[1]]
  plot_MC_conv("TwoStage", "sum.empl", prec.p = 0.005)[[1]]
  plot_MC_conv("TwoStage", "sum.unempl", prec.p = 0.005)[[1]]
  plot_MC_conv("TwoStage", "sum.inact", prec.p = 0.005)[[1]]
  plot_MC_conv("TwoStage", "r.act", prec.p = 0.005)[[1]]
  plot_MC_conv("TwoStage", "r.empl", prec.p = 0.005)[[1]]
  plot_MC_conv("TwoStage", "r.unempl", prec.p = 0.005)[[1]]
dev.off()

pdf("MC_convergence_4_Var_estim.pdf")
  plot_MC_conv("SRSWeek", "sum.act", prec.p = 0.001)[[2]]
  plot_MC_conv("SRSWeek", "sum.empl", prec.p = 0.001)[[2]]
  plot_MC_conv("SRSWeek", "sum.unempl", prec.p = 0.002)[[2]]
  plot_MC_conv("SRSWeek", "sum.inact", prec.p = 0.001)[[2]]
  plot_MC_conv("SRSWeek", "r.act", prec.p = 0.001)[[2]]
  plot_MC_conv("SRSWeek", "r.empl", prec.p = 0.001)[[2]]
  plot_MC_conv("SRSWeek", "r.unempl", prec.p = 0.002)[[2]]
  
  plot_MC_conv("ClusterWeek", "sum.act", prec.p = 0.001)[[2]]
  plot_MC_conv("ClusterWeek", "sum.empl", prec.p = 0.001)[[2]]
  plot_MC_conv("ClusterWeek", "sum.unempl", prec.p = 0.002)[[2]]
  plot_MC_conv("ClusterWeek", "sum.inact", prec.p = 0.001)[[2]]
  plot_MC_conv("ClusterWeek", "r.act", prec.p = 0.001)[[2]]
  plot_MC_conv("ClusterWeek", "r.empl", prec.p = 0.001)[[2]]
  plot_MC_conv("ClusterWeek", "r.unempl", prec.p = 0.002)[[2]]
  
  plot_MC_conv("TwoStage", "sum.act", prec.p = 0.001)[[2]]
  plot_MC_conv("TwoStage", "sum.empl", prec.p = 0.001)[[2]]
  plot_MC_conv("TwoStage", "sum.unempl", prec.p = 0.002)[[2]]
  plot_MC_conv("TwoStage", "sum.inact", prec.p = 0.001)[[2]]
  plot_MC_conv("TwoStage", "r.act", prec.p = 0.001)[[2]]
  plot_MC_conv("TwoStage", "r.empl", prec.p = 0.001)[[2]]
  plot_MC_conv("TwoStage", "r.unempl", prec.p = 0.002)[[2]]
dev.off()

pdf("MC_Observations.pdf")
  plot_MC_conv("SRSWeek", "sum.act")[[3]]
  plot_MC_conv("SRSWeek", "sum.empl")[[3]]
  plot_MC_conv("SRSWeek", "sum.unempl")[[3]]
  plot_MC_conv("SRSWeek", "sum.inact")[[3]]
  plot_MC_conv("SRSWeek", "r.act")[[3]]
  plot_MC_conv("SRSWeek", "r.empl")[[3]]
  plot_MC_conv("SRSWeek", "r.unempl")[[3]]
  
  plot_MC_conv("ClusterWeek", "sum.act")[[3]]
  plot_MC_conv("ClusterWeek", "sum.empl")[[3]]
  plot_MC_conv("ClusterWeek", "sum.unempl")[[3]]
  plot_MC_conv("ClusterWeek", "sum.inact")[[3]]
  plot_MC_conv("ClusterWeek", "r.act")[[3]]
  plot_MC_conv("ClusterWeek", "r.empl")[[3]]
  plot_MC_conv("ClusterWeek", "r.unempl")[[3]]
  
  plot_MC_conv("TwoStage", "sum.act")[[3]]
  plot_MC_conv("TwoStage", "sum.empl")[[3]]
  plot_MC_conv("TwoStage", "sum.unempl")[[3]]
  plot_MC_conv("TwoStage", "sum.inact")[[3]]
  plot_MC_conv("TwoStage", "r.act")[[3]]
  plot_MC_conv("TwoStage", "r.empl")[[3]]
  plot_MC_conv("TwoStage", "r.unempl")[[3]]
dev.off()


head(res)

res[res$i == 1,]


p3 <- ggplot(res[-(1:50),], aes(x = i)) +
  geom_point(aes(y = y_sd), size = 1) +
  geom_hline(aes(yintercept = tail(y_sd, 1)), colour = "blue", linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 1e4, 1000))
p3

p3 <- ggplot(res[11:2e3,]) +
  geom_point(aes(i, I, colour = I > i), size = 1) +
  geom_hline(aes(yintercept = tail(I, 1)), colour = "blue", linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 1e4, 100))
p3

png("MC_convergence.png", width = 800, height = 450)
p1
dev.off()

pdf("MC_convergence.pdf")
p1
dev.off()

pdf("MC_convergence_2.pdf")
p1
p2
p3
dev.off()




### RUN ####################

setwd(dir.res)

I <- 1e3

t1 <- Sys.time()
tmp <- Sim(fun = "run.2", arg = arg, I = I, log = F, cores = 2,
           name = "res2")
t2 <- Sys.time()

t1
t2

t2 - t1








### Results of part II #####################

setwd(dir.res)

# True values
load("pop.eka_tv.Rdata")
tv

load("res2.Rdata")
res <- res2

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

######## Simulation of Cluster Sample, Two stage sampling ############

### Phase 2
# The aim of the second phase simulation is to estimate sample sizes
# and sample allocation by three strata for the two other designs
# so that expected fieldwork cost and fieldwork cost allocation by three strata
# for these designs would be approximately equal to the cots of two stage design

### Ver01
# Initial version


### Start up ###################

### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


### Clock
# T1 <- Sys.time()


# Redefine dir.res
dir.res <- paste(projwd, "Results", "RCloud", "Ph2", sep = "/")

# Results of Phase1 - FWB
load("~/Dropbox/LU/Darbs/Promotion_Work/Promotion_Work_v4/R-PhD/01FWB/Results/res_Ph1_FWB_data.Rdata")


### Options
# options(default.stringsAsFactors = FALSE)
# getOption("default.stringsAsFactors")
# default.stringsAsFactors()

# pdf.options()
# pdf.options(width = 0, height = 0, paper = "a4r")


### Libs

require(bigmemory)
require(bigtabulate)

require(ggplot2)
require(scales)
require(gridExtra)

require(nlme)
citation("nlme")

require(xtable)


### Procedures

sourceDir(dir.proc, F)


### DATA

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")
load("frame.PSU.Rdata")
load("frame.int.Rdata")
pop.eka <- attach.big.matrix("pop.eka.desc")


### Simulation designs ###############################

setwd(dir.tmp)

### Sim functions

d1 <- 'SamplingSRSStrWeek(frame.1 = frame.p, n = n, name.strata = "strata3", weeks = 13)'
d2 <- 'SamplingClusterStrWeek(frame.1 = frame.p, frame.2 = frame.h, n = n, name.cluster = "H_ID", name.strata = "strata3", weeks = 13)'



### Part I - Estimation of sample size for SRS and Clust ############

setwd(dir.tmp)

##### run function

run <- function(code, n) {
  s <- eval(parse(text = code))
  #n.p.0 <- nrow(s)
  #n.h.0 <- as.integer(sum(s$P_ID == 1))
  n.p.s <- as.integer(bigtable(s, "strata3"))
  n.h.s <- as.integer(bigtable(unique(s[c("H_ID", "strata3")]), "strata3"))
  #d0 <- vTrip.fast.1(s[ , c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d1 <- vTrip.fast.1(s[s$strata3 == 1, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d2 <- vTrip.fast.1(s[s$strata3 == 2, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d3 <- vTrip.fast.1(s[s$strata3 == 3, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  #res <- data.frame(strata = 0:3, count.P = c(n.p.0, n.p.s), count.H = c(n.h.0, n.h.s), dist = c(d0, d1, d2, d3) / 1e3)
  res <- data.frame(strata = 1:3, count.P = n.p.s, count.H = n.h.s, dist = c(d1, d2, d3) / 1e3)
  return(res)
}


### Test

# run(d1, rep(10, 3))
# run(d2, rep(10, 3))
# 
# arg1 <- list(code = d1, n = rep(10, 3))
# arg2 <- list(code = d2, n = rep(10, 3))
# 
# test1 <- Sim(fun = "run", arg = list(arg1, arg2), I = 1, print = F)
# test1[[1]]



### Functions ##################

### Function for argument creating
dev.arg <- function(a, b, K, r.base = 13, code) {
  solis <- (b - a) / (K - 1)
  arg <- vector(mode = "list", length = K)
  for (i in 1:K) arg[[i]] <- list(code = code,
    n = round((a + (i - 1) * solis) / r.base) * r.base)
  return(arg)
}
### 

### Function for estimates from simulation results
proc.res <- function(res) {
  ### Data cleaning
  # Errors in data
  num.err <- sum(!is.na(res$err))
  if (num.err > 0) {
    cat("There are", num.err, "in resuls \n")
    res <- res[is.na(res$err), ]
  }
  
  # Compute design variable
  res$design <- substr(res$code, 9, regexpr("(", res$code, fixed = T)-1)
  
  # sample size - n
  res$n <- ifelse(res$strata == 1, res$n.1,
                  ifelse(res$strata == 2, res$n.2, res$n.3))
  
  # Delete variables
  res$timestamp <- NULL
  res$name <- NULL
  res$seed <- NULL
  res$err <- NULL
  res$code <- NULL
  res$n.1 <- NULL
  res$n.2 <- NULL
  res$n.3 <- NULL
  
  ### Cost calculation
  # According the situation in 2012
  res$cost_travel <- Cost(trip = res$dist, cons = .0955, price.f = .950,
                          k.d = k.adj)
  res$cost_interview <- Cost(n.h = res$count.H, n.p = res$count.P,
                      price.h = 3, price.p = 1)
  res$cost <- res$cost_travel + res$cost_interview

  return(res)
}
###

### Function for Monte Carlo Estimations
proc.est <- function(res, alpha = 0.01) {
  t1 <- aggregate(res["cost"], res[c("design", "strata", "a", "n")], mean)
  t2 <- aggregate(res["cost"], res[c("design", "strata", "a", "n")], sd)
  colnames(t2)[ncol(t1)] <- "cost.sd"
  t3 <- aggregate(res["i"], res[c("design", "strata", "a", "n")], max)
  
  est <- merge(merge(t1, t2), t3)
  est <- est[order(est$design, est$strata, est$a), ]
  Zt <- qt(1 - alpha/2, est$i - 1)
  est$cost.cil <- est$cost - est$cost.sd / sqrt(est$i) * Zt
  est$cost.ciu <- est$cost + est$cost.sd / sqrt(est$i) * Zt
  
  return(est)
}
###

### Function for regression

# est <- est1
# y <- FWB$cost[-1]

proc.reg <- function(est, y) {
  est$group <- paste(est$strata, est$design)
  reg.mod <- lmList(cost ~ n + I(sqrt(n)) | group, data = est)
  coeff <- as.data.frame(coef(reg.mod))
  colnames(coeff) <- letters[1:ncol(coeff)]
  coeff$TS_cost <- rep(y, each = 2)
  coeff$n <- (-coeff$c + sqrt(coeff$c^2 - 4*coeff$b*(coeff$a-coeff$TS_cost)))^2/4/coeff$b^2
  coeff$strata <- substring(rownames(coeff), 1, 1)
  coeff$design <- substring(rownames(coeff), 3)
  rownames(coeff) <- NULL
  return(coeff)
}
###

### Function to plot regression
plot2 <- function(est, coeff, design, strata, I = NULL) {
  plot.data <- est[est$design == design & est$strata == strata, ]
  coeff.data <- coeff[coeff$design == design & coeff$strata == strata, ]
  limits <- aes(ymax = cost.ciu, ymin = cost.cil)
  ggplot(plot.data, aes(y = cost, x = n)) +
    geom_point(colour = 1) +
    geom_errorbar(limits, width = (max(plot.data$n) - min(plot.data$n)) / 40,
                  colour = 2) +
    geom_hline(yintercept = FWB[strata + 1, "cost"], colour = 3) +
    geom_point(data = coeff.data, aes(y = TS_cost, x = n), size = 3, colour = 4, shape = 3) +
    stat_smooth(method=lm, formula = y ~ x + I(sqrt(x)), se = T, fullrange = T,
                linetype = "dotted", alpha = .2, colour = 5) +
    scale_y_continuous(breaks = round(c(plot.data$cost), 1), labels = comma) +
    scale_x_continuous(breaks = plot.data$n, labels = comma) +
    theme_bw() + ggtitle(paste(design, "strata", strata, "iterations", I))
}
###


### Arguments ###########

N <- bigtable(frame.p, "strata3")
M <- bigtable(frame.h, "strata3")

m.max <- c(1, 2, 3) * 8 * 13 * c(10, 7, 8+9)
# m.max is too big. c(1, 2, 2) has to be used. Does not make impact to the results.
n.max <- round(m.max * N / M / 13) * 13
m.min <- rep(13, 3)
n.min <- m.min


### Simulation 1 ###############

arg1 <- dev.arg(n.min, n.max, 8, 13, d1)
arg2 <- dev.arg(m.min, m.max, 8, 13, d2)

I1 <- 5
setwd(dir.tmp)
test1 <- Sim(fun = "run", arg = c(arg1, arg2), I = I1, print = F)
head(test1[[1]])
test1[[2]] / I1 * 100 / 3600

I1 <- 100
setwd(dir.res)
res1 <- Sim(fun = "run", arg = c(arg1, arg2), I = I1, name = "res_Ph2_1")
head(res1[[1]])
res1[[2]] / 3600


############# Load results 1 #############

setwd(dir.res)
load("res_Ph2_1.Rdata")

head(res_Ph2_1)

res1 <- proc.res(res_Ph2_1)
head(res1)

est1 <- proc.est(res1)
head(est1)
table(est1$i)

coeff1 <- proc.reg(est1, FWB$cost[-1])
coeff1

pl1 <- plot2(est1, coeff1, "SRSStrWeek", 1, est1$i[1])
pl2 <- plot2(est1, coeff1, "SRSStrWeek", 2, est1$i[1])
pl3 <- plot2(est1, coeff1, "SRSStrWeek", 3, est1$i[1])
pl4 <- plot2(est1, coeff1, "ClusterStrWeek", 1, est1$i[1])
pl5 <- plot2(est1, coeff1, "ClusterStrWeek", 2, est1$i[1])
pl6 <- plot2(est1, coeff1, "ClusterStrWeek", 3, est1$i[1])




### Simulation 2 #####

coeff1
n1 <- round(coeff1[coeff1$design == "SRSStrWeek", "n"] / 13) * 13
n2 <- round(coeff1[coeff1$design == "ClusterStrWeek", "n"] / 13) * 13
K <- 3

arg1b <- dev.arg(n1 - K * 13, n1 + K * 13, 2*K+1, 13, d1)
arg2b <- dev.arg(n2 - K * 13, n2 + K * 13, 2*K+1, 13, d2)

I2 <- 5
setwd(dir.tmp)
test2 <- Sim(fun = "run", arg = c(arg1b, arg2b), I = I2)
head(test2[[1]])
test2[[2]] / I2 * 500 / 3600

I2 <- 500
setwd(dir.res)
res2 <- Sim(fun = "run", arg = c(arg1b, arg2b), I = I2, name = "res_Ph2_2a")
head(res2[[1]])
res2[[2]] / 3600



### Results 2 ####

setwd(dir.res)
load("res_Ph2_2a.Rdata")

res2 <- proc.res(res_Ph2_2a)
dim(res2)
head(res2)

est2 <- proc.est(res2)
head(est2)
table(est2$i)

coeff2 <- proc.reg(est2, FWB$cost[-1])
coeff2

pl11 <- plot2(est2, coeff2, "SRSStrWeek", 1, est2$i[1])
pl12 <- plot2(est2, coeff2, "SRSStrWeek", 2, est2$i[1])
pl13 <- plot2(est2, coeff2, "SRSStrWeek", 3, est2$i[1])
pl14 <- plot2(est2, coeff2, "ClusterStrWeek", 1, est2$i[1])
pl15 <- plot2(est2, coeff2, "ClusterStrWeek", 2, est2$i[1])
pl16 <- plot2(est2, coeff2, "ClusterStrWeek", 3, est2$i[1])


### Sample size selection ------------------------------------------------------

FWB
x <- FWB[-1, ]
colnames(x)[2] <- "cost.lim"
x

head(est2)

est3 <- merge(est2, x)
est3 <- est3[order(est3$strata, est3$a), ]
head(est3)

est3$d.abs <- (est3$cost - est3$cost.lim)
est3$d.abs[est3$d.abs < 0] <- Inf
head(est3)

tmp <- aggregate(est3["d.abs"], est3[c("design", "strata")], min)
est4 <- merge(est3, tmp)

est4 <- est4[order(est4$design, est4$strata), ]
est4

est4$n %% 13

pl11b <- pl11 + geom_point(data = est4[4,], aes(x = n, y = cost),
                           shape = 1, size = 20, colour = 3)
pl12b <- pl12 + geom_point(data = est4[5,], aes(x = n, y = cost),
                           shape = 1, size = 20, colour = 3)
pl13b <- pl13 + geom_point(data = est4[6,], aes(x = n, y = cost),
                           shape = 1, size = 20, colour = 3)
pl14b <- pl14 + geom_point(data = est4[1,], aes(x = n, y = cost),
                           shape = 1, size = 20, colour = 3)
pl15b <- pl15 + geom_point(data = est4[2,], aes(x = n, y = cost),
                           shape = 1, size = 20, colour = 3)
pl16b <- pl16 + geom_point(data = est4[3,], aes(x = n, y = cost),
                           shape = 1, size = 20, colour = 3)

pl11b
pl14b
pl12b
pl15b
pl13b
pl16b


est5 <- est4[c("design", "strata", "n")]
est5

est6 <- est4[c("design", "strata", "n", "cost",
               "cost.sd", "cost.cil", "cost.ciu", "cost.lim")]
est6


####### Save results ##########

setwd(dir.res)

pdf(file = "plot_Ph2.pdf", width = 9, height = 12)
  grid.arrange(pl1, pl4, pl2, pl5, pl3, pl6, ncol = 2)
  grid.arrange(pl11, pl14, pl12, pl15, pl13, pl16, ncol = 2)
  grid.arrange(pl11b, pl14b, pl12b, pl15b, pl13b, pl16b, ncol = 2)
dev.off()

write.csv(rbind(est1, est2), file = "tab_estimates_Ph2.csv")
write.csv(rbind(coeff1, coeff2), file = "tab_reg_coeff_Ph2.csv")
write.csv(est4, file = "tab_ssizes_Ph2.csv")

save(est5, file = "sample_sizes_Ph2.Rdata")

est6
ncol(est6)+1
print(xtable(est6, digits = c(rep(0, 4), rep(1, 5))), include.rownames = F,
      include.colnames = T, only.contents = T, append = F,
      file = "tab_ssizes_Ph2.tex")

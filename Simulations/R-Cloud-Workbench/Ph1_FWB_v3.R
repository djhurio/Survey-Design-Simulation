### Simulation of SRS, Cluster Sample, Two stage sampling ############

### R Cloud Workbench
### http://www.ebi.ac.uk/Tools/rcloud/

### Phase 1 - The Estimation of Field Work Budget
### Prepare data



### Start up ###################

### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")

ls()
# dir.proc


# Redefine dir.res
dir.res <- paste(projwd, "Results", "RCloud", sep = "/")



### Options


### Libs

require(bigmemory)
require(bigtabulate)
require(foreach)
require(nlme)


### Procedures

sourceDir(dir.proc, F)


### DATA

setwd(dir.data)

frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")

# load("frame.p.Rdata")
# load("frame.h.Rdata")

load("frame.PSU.Rdata")
load("frame.int.Rdata")

# pop.eka <- attach.big.matrix("pop.eka.desc")
# dim(pop.eka)


#### Average cluster size ####

head(frame.PSU)

aggregate(frame.PSU["size"], frame.PSU["strata"], function(x) round(mean(x)))


### Simulation design ###############################

# N <- nrow(frame.p)
# N
M <- nrow(frame.h)
M
# N/M

M.h <- as.vector(bigtable(frame.h, "strata"))
M.h
sum(M.h)
sum(M.h) == M

# max.PSU.size.h <- aggregate(frame.PSU["size"], frame.PSU["strata"], max)[, 2]
# delta <- 1 / max.PSU.size.h
# delta <- max.PSU.size.h / M.h
# delta
# 1/(16*13)

weeks <- 13

sampl.des.par.1 <- data.frame(s = 1:4,
        A = 8,
        B = c(1, 2, 2, 2),
        W = 13,
        d = 0,
        Q = c(6, 2, 7, 11),
        w = weeks,
        M = M.h,
        m = c(10, 7, 8, 9))

sampl.des.par.1

m <- sum(sampl.des.par.1$A * sampl.des.par.1$B * sampl.des.par.1$w * sampl.des.par.1$m)
m
# m * N/M

# rm(M.h, delta, max.PSU.size.h, strata.selected)


### Sim functions

des1 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par.1)'

s <- eval(parse(text = des1))
head(s)
table(s$strata[s$P_ID == 1])
sum(table(s$strata[s$P_ID == 1]))

### Part I - Estimation of sample size for SRS and Clust ############

##### run function

run.1 <- function(code) {
  s <- eval(parse(text = code))
  s$strata[s$strata == 4] <- 3
  n.p.0 <- nrow(s)
  n.h.0 <- as.integer(sum(s$P_ID == 1))
  n.p.s <- as.integer(bigtable(s, "strata"))
  n.h.s <- as.integer(bigtable(s[s$P_ID == 1, ], "strata"))
  d0 <- vTrip.fast.1(s[ , c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d1 <- vTrip.fast.1(s[s$strata == 1, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d2 <- vTrip.fast.1(s[s$strata == 2, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d3 <- vTrip.fast.1(s[s$strata == 3, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  res <- data.frame(strata = 0:3, count.P = c(n.p.0, n.p.s), count.H = c(n.h.0, n.h.s), dist = c(d0, d1, d2, d3) / 1e3)
  return(res)
}

run.1(des1)

### Arguments

arg.1 <- data.frame(code = c(des1), stringsAsFactors = F)

### Test

setwd(dir.tmp)

I <- 16

test <- Sim(fun = "run.1", arg = arg.1, I = I, print = F, log = F)


I <- 16*5
test <- Sim(fun = "run.1", arg = arg.1, I = I, print = F, log = F)

head(test[[1]])
time_iter <- test[[2]] / I
time_iter * 6e3 / 3600

Sys.time() + time_iter * 6e3


### Run simulation
### Phase1 - The Estimation of Field Work Budget

setwd(dir.res)
getwd()

I <- 6e3
I

tmp <- Sim(fun = "run.1", name = "res_Ph1_FWB_v3", arg = arg.1, I = I, print = F, log = F)

head(tmp[[1]])
tmp[[2]] / I

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

# ls()
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

sampl.des.par.2 <- data.frame(s = 1:4,
        A = 8,
        B = c(1, 2, 2, 2),
        W = 13,
        d = 0,
        Q = 0,
        w = weeks,
        M = M.h,
        m = c(10, 7, 8, 9))

sampl.des.par.1
sampl.des.par.2

m <- sum(sampl.des.par.1$A * sampl.des.par.1$B * sampl.des.par.1$w * sampl.des.par.1$m)
m
# m * N/M

# rm(M.h, delta, max.PSU.size.h, strata.selected)


### Sim functions

des1 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par.1)'
des2 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par.2)'

s <- eval(parse(text = des1))
head(s)
table(s$strata[s$P_ID == 1])
sum(table(s$strata[s$P_ID == 1]))

s <- eval(parse(text = des2))
head(s)
table(s$strata[s$P_ID == 1])
sum(table(s$strata[s$P_ID == 1]))

### Part I - Estimation of sample size for SRS and Clust ############

##### run function

run.1 <- function(code) {
    s <- eval(parse(text = code))
    #print(head(s))
    n.p.s <- as.integer(table(s$strata))
    n.h.s <- as.integer(table(s$strata[s$P_ID == 1]))
    d1 <- vTrip.fast.1(s[s$strata == 1, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
    d2 <- vTrip.fast.1(s[s$strata == 2, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
    d3 <- vTrip.fast.1(s[s$strata == 3, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
    d4 <- vTrip.fast.1(s[s$strata == 4, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
    res <- data.frame(strata = 1:4, count.P = c(n.p.s), count.H = c(n.h.s), dist = c(d1, d2, d3, d4) / 1e3)
    return(res)
}

#run.1(des1)

run.2 <- function(code) {
  s <- eval(parse(text = code))
  n.p.0 <- nrow(s)
  n.p.s <- as.integer(tapply(s$P_ID, s$strata, length))
  n.h.0 <- as.integer(sum(s$P_ID == 1))
  n.h.s <- as.integer(tapply(s$P_ID[s$P_ID == 1], s$strata[s$P_ID == 1], length))
  d0 <- vTrip.fast.1(s[ , c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d1 <- vTrip.fast.1(s[s$strata == 1, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d2 <- vTrip.fast.1(s[s$strata == 2, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d3 <- vTrip.fast.1(s[s$strata == 3, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d4 <- vTrip.fast.1(s[s$strata == 4, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  res <- data.frame(strata = 0:4, count.P = c(n.p.0, n.p.s), count.H = c(n.h.0, n.h.s), dist = c(d0, d1, d2, d3, d4) / 1e3)
  return(res)
}


### Arguments

arg.1 <- data.frame(code = c(des1), stringsAsFactors = F)
arg.2 <- data.frame(code = c(des2), stringsAsFactors = F)
arg.3 <- data.frame(code = c(des1, des2), stringsAsFactors = F)

### Test

setwd(dir.tmp)

I <- 2
test <- Sim(fun = "run.1", arg = arg.1, I = I, print = F, log = F)

I <- 16*10
test <- Sim(fun = "run", arg = arg3, I = I, print = F, log = F)
#test <- Sim(fun = "run", arg = arg4, I = I, print = F, log = F)

head(test[[1]])
time_iter <- test[[2]] / I
time_iter * 10000 / 3600


### Run simulation
### Phase1 - The Estimation of Field Work Budget

setwd(dir.res)
getwd()

I <- 10e3
I

tmp <- Sim(fun = "run", name = "res_Ph1_FWB", arg = arg3, I = I, print = F, log = F)

head(tmp[[1]])
tmp[[2]] / I


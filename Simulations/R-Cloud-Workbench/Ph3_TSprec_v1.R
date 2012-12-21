### Simulation of Two stage sampling ############

### R Cloud Workbench
### http://www.ebi.ac.uk/Tools/rcloud/

### Phase 3 - The Estimation of the precision of the two stage sampling design
### Prepare data



### Start up ###################

# .Platform$GUI

### Reset
# setwd(projwd)
# rm(list = ls())
# gc()
source(".Rprofile")

ls()
# dir.proc

# Redefine dir.res
dir.res <- paste(projwd, "Results", "RCloud", "Ph3", sep = "/")
dir.res.Ph2 <- paste(projwd, "Results", "RCloud", "Ph2", sep = "/")



### Libs

require(bigmemory)
require(bigtabulate)
require(digest)

### Procedures

sourceDir(dir.proc, F)


### DATA

setwd(dir.data)
frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")
load("frame.PSU.Rdata")
load("frame.int.Rdata")
pop.eka <- attach.big.matrix("pop.eka.desc")

# load("pop.Rdata")
# load("pop.h.Rdata")
# digest(pop)
# digest(pop.h)

nrow(frame.p)
nrow(frame.h)

# digest(frame.p)
# digest(frame.h)
# digest(frame.PSU)
# digest(frame.int)
# digest(pop.eka)


# Sample sizes
setwd(dir.res.Ph2)
load("sample_sizes_Ph2.Rdata")
est5

### Simulation design ###############################

M <- nrow(frame.h)
M.h <- as.vector(bigtable(frame.h, "strata"))
M.h
sum(M.h) == M

sampl.des.par.1 <- data.frame(s = 1:4,
        A = 8,
        B = c(1, 2, 2, 2),
        W = 13,
        d = 0,
        Q = c(6, 2, 7, 11),
        w = 13,
        M = M.h,
        m = c(10, 7, 8, 9))
sampl.des.par.1

m <- sum(sampl.des.par.1$A * sampl.des.par.1$B * sampl.des.par.1$w * sampl.des.par.1$m)
m


### Sim functions

est5

n1 <- est5[4:6, 3]
n2 <- est5[1:3, 3]
n3 <- rep(NA, 3)

des1 <- 'SamplingSRSStrWeek(frame.1 = frame.p, n = n, name.strata = "strata3", weeks = 13)'
des2 <- 'SamplingClusterStrWeek(frame.1 = frame.p, frame.2 = frame.h, n = n, name.cluster = "H_ID", name.strata = "strata3", weeks = 13)'
des3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par.1)'

n <- n1
test <- eval(parse(text = des1))
head(test)
nrow(test)
nrow(test) == sum(n)

n <- n2
test <- eval(parse(text = des2))
head(test)
nrow(test[test$P_ID == 1, ])
nrow(test[test$P_ID == 1, ]) == sum(n)

n <- NULL
test <- eval(parse(text = des3))
head(test)
nrow(test[test$P_ID == 1, ])

### Part I - Estimation of sample size for SRS and Clust ############

##### run function

head(pop.eka[, 1:10])

run.1 <- function(code, n = NA) {
  s <- eval(parse(text = code))
  s2 <- extr.data(pop.eka, s$casenum, s$.week, 5, "eka.time")
  s <- merge(s, s2)
  
  s$empl <- as.integer(s$eka.time == 1)
  s$unempl <- as.integer(s$eka.time == 2)
  s$inact <- as.integer(s$eka.time == 3)
  s$age.gr <- ifelse(s$vec <= 24, 1, 2)
  
  d1 <- domeni(s[c("empl", "unempl", "inact")], s["strata"])
  d2 <- domeni(s[c("empl", "unempl", "inact")], s["age.gr"])
  d3 <- domeni(s[c("empl", "unempl", "inact")], s[c("strata", "age.gr")])
  s <- data.frame(s, d1, d2, d3)
  
  p <- cbind("sum", c("empl", "unempl", "inact",
                      colnames(d1), colnames(d2), colnames(d3)), NA)
  est <- Estimation(s, s$.dw, p)
  return(est)
}

test <- run.1(des1, n1)
t(test)

test <- run.1(des2, n2)
t(test)

test <- run.1(des3, n3)
t(test)

ncol(test)
length(test)

### Arguments

arg1 <- list(code = c(des1), n = n1)
arg2 <- list(code = c(des2), n = n2)
arg3 <- list(code = c(des3), n = n3)
arg <- list(arg1, arg2, arg3)
arg

### Test

setwd(dir.tmp)
test <- Sim(fun = "run.1", arg = arg, I = 4)
test[[1]]

I <- 4*5
test <- Sim(fun = "run.1", arg = arg, I = I)

head(test[[1]])
time_iter <- test[[2]] / I

I2 <- 10e3
time_iter * I2 / 3600

Sys.time() + time_iter * I2


### Run simulation
### Phase3 - Precision of Two Stage sampling design

setwd(dir.res)
getwd()

res <- Sim(fun = "run.1", name = "res_Ph3_TS_run2", arg = arg, I = I2)

head(res[[1]])
res[[2]] / I

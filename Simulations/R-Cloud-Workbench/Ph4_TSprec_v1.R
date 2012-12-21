### Simulation of Two stage sampling ############

### R Cloud Workbench
### http://www.ebi.ac.uk/Tools/rcloud/

### Phase 4 - Testing two stage sampling design
### Does variance is Q dependant?

### Start up ###################

# Redefine dir.res
dir.res <- paste(projwd, "Results", "RCloud", "Ph4", sep = "/")


### Libs

require(bigmemory)
require(bigtabulate)

### Procedures

sourceDir(dir.proc, F)


### DATA

setwd(dir.data)
frame.p <- attach.big.matrix("frame.p.desc")
frame.h <- attach.big.matrix("frame.h.desc")
load("frame.PSU.Rdata")
load("frame.int.Rdata")
pop.eka <- attach.big.matrix("pop.eka.desc")

nrow(frame.p)
nrow(frame.h)

### Simulation design ###############################

M.h <- as.vector(bigtable(frame.h, "strata"))

sdp <- function(q = 0) {
  data.frame(s = 1:4,
        A = 8,
        B = c(1, 2, 2, 2),
        W = 13,
        d = 0,
        Q = q,
        w = 13,
        M = M.h,
        m = c(10, 7, 8, 9))
}

sdp(3)


### Sim functions

des3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sdp(q))'

q <- 12
test <- eval(parse(text = des3))
head(test)
nrow(test[test$P_ID == 1, ])

### Part I - Estimation of sample size for SRS and Clust ############

##### run function

head(pop.eka[, 1:10])

run.1 <- function(code, q = 0) {
  s <- eval(parse(text = code))
  s2 <- extr.data(pop.eka, s$casenum, s$.week, 5, "eka.time")
  s <- merge(s, s2)
  
  s$empl <- as.integer(s$eka.time == 1)
  s$unempl <- as.integer(s$eka.time == 2)
  s$inact <- as.integer(s$eka.time == 3)
  s$age.gr <- ifelse(s$vec <= 24, 1, 2)
  
  d1 <- domeni(s[c("empl", "unempl", "inact")], s["strata"])
  #d2 <- domeni(s[c("empl", "unempl", "inact")], s["age.gr"])
  #d3 <- domeni(s[c("empl", "unempl", "inact")], s[c("strata", "age.gr")])
  #s <- data.frame(s, d1, d2, d3)
  s <- data.frame(s, d1)
  
  #p <- cbind("sum", c("empl", "unempl", "inact",
  #                    colnames(d1), colnames(d2), colnames(d3)), NA)
  p <- cbind("sum", c("empl", "unempl", "inact", colnames(d1)), NA)
  est <- Estimation(s, s$.dw, p)
  return(est)
}

test <- run.1(des3, q = 0)
t(test)

ncol(test)
length(test)

### Arguments

### Function for argument creating
dev.arg <- function(q, code) {
  K <- length(q)
  arg <- vector(mode = "list", length = K)
  for (i in 1:K) arg[[i]] <- list(code = code, q = q[i])
  return(arg)
}
###

arg <- dev.arg(0:12, des3)
arg

### Test

setwd(dir.tmp)
test <- Sim(fun = "run.1", arg = arg, I = 2)
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

### Simulation of SRS, Cluster Sample, Two stage sampling ############

### R Cloud Workbench
### http://www.ebi.ac.uk/Tools/rcloud/

### Phase 1 - cost
### Run



### Start up ###################

### Reset
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")


# Redefine dir.res
dir.res <- paste(projwd, "Results", "RCloud", sep = "/")



### Options


### Libs

require(bigmemory)
require(bigtabulate)
require(foreach)
require(nlme)


### Load WS

setwd(dir.res)

load("WS_Sim_P1.RData")


### Timing

setwd(dir.tmp)

I <- 16

t1 <- Sys.time()
res <- Sim(fun = "run", arg = arg, I = I, name = "test")
t2 <- Sys.time()

t2 - t1
res[[2]]

res[[2]] / I * 100e3 / 3600 / 24



### Run

setwd(dir.res)

I <- 100e3

t1 <- Sys.time()
res <- Sim(fun = "run", arg = arg, I = I, name = "Sim_P1")
t2 <- Sys.time()

t2 - t1
res[[2]]

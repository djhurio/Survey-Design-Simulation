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




### Simulation design ###############################

setwd(dir.tmp)

N <- nrow(frame.p)
N

M <- nrow(frame.h)
M

N/M

M.h <- as.vector(bigtable(frame.h, "strata"))
M.h
sum(M.h)
sum(M.h) == M

max.PSU.size.h <- aggregate(frame.PSU["size"], frame.PSU["strata"], max)[, 2]

# delta <- 1 / max.PSU.size.h
delta <- max.PSU.size.h / M.h
delta

1/(16*13)

weeks <- 13

sampl.des.par <- data.frame(s = 1:4,
                            A = 8,
                            B = c(1, 2, 2, 2),
                            W = 13,
                            d = 0,
                            Q = c(6, 2, 7, 11),
                            w = weeks,
                            M = M.h,
                            m = c(10, 7, 8, 9))

sampl.des.par

m <- sum(sampl.des.par$A * 
  sampl.des.par$B * sampl.des.par$w * sampl.des.par$m)
m
m * N/M

# rm(M.h, delta, max.PSU.size.h, strata.selected)


### Sim functions

d3 <- 'SamplingTwoStage(frame.PSU, frame.h, frame.p, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par)'
d4 <- 'SamplingTwoStage(frame.PSU, frame.h.df, frame.p.df, ".dw1", ".dw2", ".dw", ".week", "iec2010", "H_ID", "strata", sampl.des.par)'

s <- eval(parse(text = d3))
head(s)
table(s$strata)

s <- eval(parse(text = d4))
head(s)
table(s$strata)

### Part I - Estimation of sample size for SRS and Clust ############

##### run function

run <- function(code) {
  s <- eval(parse(text = code))
  n.p <- as.integer(table(s$strata))
  n.h <- as.integer(table(s$strata[s$P_ID == 1]))
  d1 <- vTrip.fast.1(s[s$strata == 1, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d2 <- vTrip.fast.1(s[s$strata == 2, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d3 <- vTrip.fast.1(s[s$strata == 3, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  d4 <- vTrip.fast.1(s[s$strata == 4, c("int.ID", ".week", "coord_x_p", "coord_y_p")], frame.int[c("int.ID", "x_int", "y_int")])
  res <- data.frame(strata = 1:4, count.P = n.p, count.H = n.h, dist = c(d1, d2, d3, d4) / 1e3)
  return(res)
}


### Arguments

arg3 <- data.frame(code = d3, stringsAsFactors = F)
arg4 <- data.frame(code = d4, stringsAsFactors = F)



### Test

setwd(dir.tmp)

I <- 10
test <- Sim(fun = "run", arg = arg3, I = I, print = F, log = F)
test <- Sim(fun = "run", arg = arg4, I = I, print = F, log = F)

head(test[[1]])
test[[2]]

# rm(I, test)


### Save workspace

setwd(dir.res)

rm(dir.data, dir.proc, dir.res, dir.tmp, projwd)

save.image("WS_Sim_P1.RData")

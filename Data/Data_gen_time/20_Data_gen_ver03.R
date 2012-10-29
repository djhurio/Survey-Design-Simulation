########################################
### Population data changing in time ###
########################################


### Libs

require(reshape2)
require(ggplot2)
require(foreach)
require(bigmemory)
require(bigtabulate)


### Reset ###
setwd(projwd)
rm(list = ls())
gc()
source(".Rprofile")





### Data

setwd(projwd)
load("Data/TM.Rdata")
TM


###

setwd(dir.data)

# load("frame.p.Rdata")
frame.p <- attach.big.matrix("frame.p.desc")

head(frame.p)
# test.df(frame.p)

nrow(frame.p)

bigtable(frame.p, "eka")
prop.table(bigtable(frame.p, "eka"))

eka.distr <- as.numeric(prop.table(bigtable(frame.p, "eka")))
eka.distr


eka <- frame.p[, "eka"]
class(eka)
length(eka)

# rm(frame.p)


###

TM

TM.cum <- cbind(TM[, c(1:3)], rowSums(TM[, c(3:4)]), rowSums(TM[, c(3:5)]))
colnames(TM.cum) <- colnames(TM)
TM.cum

TM.cum.list <- split(TM.cum[, c(3:5)], TM.cum$per)
TM.cum.list[[1]]
TM.cum.list[[2]]


### Function state.change()

state.change <- function(state, tm.cum) {
  n <- length(state)
  #if (n == 1) return(ncol(tm.cum) - sum(runif(1) < tm.cum[state, ]) + 1)
  #else return(ncol(tm.cum) - rowSums(runif(n) < tm.cum[state, ]) + 1)
  if (n == 1) return(as.integer(ncol(tm.cum) - sum(runif(1) < tm.cum[state, ]) + 1))
  else return(as.integer(ncol(tm.cum) - rowSums(runif(n) < tm.cum[state, ]) + 1))
}


### Function gen.pop()

gen.pop <- function(state0, tm.list, time,
                    file.name = "file", extra.col = NULL) {
  
  n <- length(state0)
  
  m1 <- length(time)
  m2 <- length(extra.col)
  
  pop <- filebacked.big.matrix(nrow = n,
    ncol = m2 + m1 + 1,
    type = "integer",
    dimnames = list(NULL, c(extra.col, paste("eka", 0:m1, sep = ""))),
    backingfile = paste(file.name, ".bin", sep = ""),
    descriptorfile = paste(file.name, ".desc", sep = ""))
  
  pop[, m2+1] <- as.integer(state0)
  
  for (i in 1:m1) {
    cat(paste("i:", i, "time:", time[i], "\n"))
    #pop[, m2+1+i] <- state.change(pop[, m2+i], tm.list[[time[i]]])
    # Hack to make static population
    pop[, m2+1+i] <- pop[, m2+i]
  }
  
  return(NULL)
}



################
### Work dir ###
################

setwd(dir.data)

# time <- rep(1:4, times = 1, each = 13)
time <- rep(1, times = 1, each = 13)
time
length(time)

set.seed(20120502)
gen.pop(eka, TM.cum.list, time,
        file.name = "pop.eka.static",
        extra.col = c("casenum", "H_ID", "P_ID", "strata"))



###

# popg <- attach.big.matrix("pop.eka.desc")
popg <- attach.big.matrix("pop.eka.static.desc")
class(popg)
head(popg)


head(frame.p)

popg[, c("casenum", "H_ID", "P_ID", "strata")] <- 
  as.integer(frame.p[, c("casenum","H_ID", "P_ID", "strata")])
head(popg)

# Save to file
flush(popg)

bigtabulate(popg, "strata")
